##---------------------------------------------------------
## This file relies on output created from objective 1,
## so make sure to run that code prior to running this
## analysis.
##
## The purpose of this file is to extract the fitted values
## from the objective 1 analysis, for use as the 
## outcome in the second-stage regressions for the 
## ensemble-based variable importance analysis.
##---------------------------------------------------------

## load required libraries
library("methods")
library("future")
library("future.apply")
library("data.table")
library("cvma")
library("SuperLearner")
library("glmnet")
library("randomForest")
library("xgboost")
library("e1071")

## define directory for code
path.home <- "/repository/home/path"
setwd(path.home)
codeDir <- "./code/"

## define directory to get initial SuperLearner results from
resDir <- "."

## define naming convention for initial SuperLearner results
## for both dataset 1 and dataset 2
resNms1 <- c("fit_", "_set1.RData")
resNms2 <- c("fit_", "_set2.RData")

## define directory to save results
outDir <- "."

## naming convention for fitted values, predicted values
fitNms1 <- c("fits_", "_set1.rds")
fitNms2 <- c("fits_", "_set2.rds")
predNms1 <- c("preds_", "_set1.rds")
predNms2 <- c("preds_", "_set2.rds")

## run makeDataAndFunctions R script
## and other user-defined functions
source(paste0(codeDir, "objective1/00-prelims.R"))
source(paste0(codeDir, "objective2/00-ensemble_vim_helpers.R"))


## get the fitted values
fits <- c("cens", "ic50", "ic80", "sens.resis", "slope_mod")
for (i in 1:length(fits)) {
  ## get the outcome
  y <- eval(parse(text = match_chr_y(fits[i], 1)))
  ## for set 1
  load(paste0(resDir, resNms1[1], fits[i], resNms1[2]))
  
  ## predicted values from each of the test sets
  cat("\n Running", fits[i], ", dataset 1 \n")
  tmp <- get_preds(fit, X[!is.na(y), ], 10)
  preds_list <- list(pred = tmp$preds, folds = tmp$folds)
  fitted_list <- list(fit = tmp$fits, folds = tmp$folds)
  saveRDS(preds_list, file = paste0(outDir, predNms1[1], fits[i], predNms1[2]))
  saveRDS(fitted_list, file = paste0(outDir, fitNms1[1], fits[i], fitNms1[2]))
  
  ## for set 2
  ## get the outcome
  y_2 <- eval(parse(text = match_chr_y(fits[i], 2)))
  load(paste0(resDir, resNms2[1], fits[i], resNms2[2]))
  ## predicted values on left out data
  cat("\n Running", fits[i], ", dataset 2")
  tmp <- get_preds(fit, X2[!is.na(y_2), ], 10)
  preds_list <- list(pred = tmp$preds, folds = tmp$folds)
  fitted_list <- list(fit = tmp$fits, folds = tmp$folds)
  saveRDS(preds_list, file = paste0(outDir, predNms2[1], fits[i], predNms2[2]))
  saveRDS(fitted_list, file = paste0(outDir, fitNms2[1], fits[i], fitNms2[2]))
}