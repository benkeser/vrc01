#!/usr/local/bin/Rscript

##------------------------------------------------------------------------
## This file relies on output created from 01-get_fits_from_objective_1.R,
## so make sure to run that code prior to running this
## analysis.
##
## Due to the intense computational requirement of the aforementioned 
## script, we do not call "source" from within this file,
## and instead rely on the user to have run them prior to running this code.
##
## The purpose of this file is to fit the second-stage regressions for the 
## ensemble-based variable importance analysis; here, we fit based on 
## leaving out groups of features.
## 
## With that in mind, this code runs a Super Learner fit for
## the group of features for which importance is desired, and thus this code
## is quite time-intensive. This may be sped up by using a high-performance computing
## cluster, e.g., SLURM. The current implementation below may be adapted to run 
## on a cluster by removing the parser argument for job_id, and instead setting, e.g.,
## job_id <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
## in line 78 when using a job array.
##
## This code is also set up to run from the command line, e.g., using
## Rscript 02-second_stage_regression_individual --outcome ic50 --data-set 1 --indx 1
## which gets ensemble-based variable importance for feature group 1, in dataset 1, for the
## quantitative IC50 outcome.
##  
## Modify line 1 if your instance of Rscript is located in a different directory.
##------------------------------------------------------------------------

## load required libraries
library("methods")
library("data.table")
library("SuperLearner")
library("cvma")
library("argparse")
library("glmnet")
library("quadprog")
library("Matrix")
library("foreach")
library("randomForest")
library("xgboost")

## define directory for code
codeDir <- "./code/"

## define directory to get first-stage fitted values and predicted values from;
## this *must* match where results from 01-get_fits_from_objective_1.R are located
resDir <- "."
## naming convention for first-stage results; this *must* match 01-get_fits_from_objective_1.R
fitNms <- c("fits_", "_set", ".rds")

## define directory to save results
outDir <- "."

## naming convention for results
res_nms <- c("reduced_preds_", "_dataset_", "_group_", ".rds")

## run makeDataAndFunctions R script
## and other user-defined functions
source(paste0(codeDir, "objective1/00-prelims.R"))
source(paste0(codeDir, "objective2/00-ensemble_vim_helpers.R"))

## create a parser object
parser <- ArgumentParser()

# set up the parser
parser$add_argument("--outcome", default = "ic50", help = "Which outcome to use?")
parser$add_argument("--data-set", default = 1, help = "Which dataset to use?", type = "double")
## remove line 78 if you are using a high-performance cluster computer, and instead get the array task id or other identifier
parser$add_argument("--indx", default = 1, help = "Which feature to get importance for?", type = "double")

# get the args
args <- parser$parse_args()

## get the job id; if running on a high-performance computing cluster, change the next line to the array task id or equivalent identifier
job_id <- args$indx

## vector of feature sets
fs <- 1:13

## get the correct feature set based on the job id
f <- fs[job_id]

## set up the feature sets of interest
## get the amino acid feature sets to remove
preds <- unique(predictors)
feature_set_1 <- preds %in% AAVRC01contactsitescharactervars # VRC01 contact sites
feature_set_2 <- preds %in% AACD4bindingsitescharactervars # CD4 binding sites
feature_set_3 <- preds %in% AAESAsitescharactervars # sites with sufficient exposed surface area
feature_set_4 <- preds %in% AAGLYCOsitescharactervars # sites identified as important for glycosylation
feature_set_5 <- preds %in% AACOVARsitescharactervars # sites with residues that covary with VRC01 binding footprint
feature_set_6 <- preds %in% AAPNGsitescharactervars # sites associated with VRC01-specific PNGS effects
feature_set_7 <- preds %in% AAgp41sitescharactervars
feature_set_8 <- preds %in% AAglycosylationgp160NoVRC01vars # sites for indicating N-linked glycosylation in gp160
# feature_set_nonmajority_1 <- preds %in% AAVRC01contactsitesnonmajorityvars
# feature_set_nonmajority_2 <- preds %in% AAotherVRC01relevantnonmajorityvars

## get the other feature sets to remove
feature_set_9 <- preds %in% subtypevars 
feature_set_10 <- preds %in% glycosylationvars
feature_set_11 <- preds %in% viralgeometryvars
feature_set_12 <- preds %in% cysteinesvars
feature_set_13 <- grepl("taylor", preds)

cov.mat <- matrix(c(feature_set_1, feature_set_2, feature_set_3, feature_set_4, feature_set_5, feature_set_6,
    feature_set_7, feature_set_8, feature_set_9, feature_set_10, feature_set_11, feature_set_12,
    feature_set_13), ncol = length(preds), byrow = TRUE)

## pick off the correct row based on which job we are running
current <- cov.mat[f, ]

## load in the fitted values corresponding to the correct outcome and dataset
lst <- readRDS(paste0(resDir, fitNms[1], args$outcome, fitNms[2], args$data_set, fitNms[3]))
fits_lst <- lst$fit
folds <- lst$folds

## pick the correct dataset and fitted values
if (args$data_set == 1) {
    newX <- X[, !current]
} else {
    newX <- X2[, !current]
}

## fix the library
SL.library.vimp.continuous <- SL.library.vimp[!grepl("SL.naivebayes", SL.library.vimp)][c(1, 3, 5, 7, 9, 11, 13)]
## Set a seed for reproducibility
seeds <- c(4711, 1959, 9835, 9333, 1816, 
    6170, 9124, 1029, 8036, 3316, 2476, 8107)
seed <- seeds[f] 
set.seed(seed)

## get complete cases
cc <- complete.cases(newX)
## also get whether or not the original outcome was NA
orig_y <- eval(parse(text = match_chr_y(args$outcome, args$data_set)))
orig_y_not_na <- !is.na(orig_y)

## get indices for y
newX2 <- newX[orig_y_not_na & cc, ]

## run V=10-fold cross-validation, for the CV one-step
## save the fitted objects, predictions on the test data

# See if we can do this with one call to cvma...
red_lst <- list()
preds_lst <- list()
system.time(for (v in 1:10) {
    ## train/test split
    # in order match up with what's in fits_lst, we need
    # to go in reverse order. i.e., here for v == 1, 
    # fits_lst[[1]] is the predictions from the super learner 
    # fit using folds 1:9 (see the comments in get_preds.R)
    # and we want to use folds 1:9 to regress those predictions 
    # onto reduced covariates. Next for v == 2, fits_lst[[2]]
    # is predictions form the super learner fit using folds 1:8,10
    # and we want to use folds 1:8,10 to regress those predictions
    # onto reduced covariates. etc... 
    train_x <- newX2[folds != 11-v, ]
    test_x <- newX2[folds == 11-v, ]
    train_y <- fits_lst[[v]]

    ## run the Super Learner
    # Choosing V = 9 makes this second-stage 
    # super learner consistent with the first stage one. 
    fit <- SuperLearner(Y = train_y, X = train_x, newX = test_x, 
                        cvControl = list(V = 9), 
                        method = "method.CC_LS", 
                        SL.library = SL.library.vimp.continuous)
    red_lst[[v]] <- fit
    ## get predictions
    preds_lst[[v]] <- fit$SL.predict
})

## save off the predicted values
saveRDS(preds_lst, file = paste0(res_nms[1], args$outcome, res_nms[2], args$data_set, res_nms[3], job_id, res_nms[4]))