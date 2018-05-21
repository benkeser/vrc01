#----------------------------------------------------------------------
# This script performs the super learner analysis for each of the 
# outcomes. Note that this the execution of this script is fairly 
# time intensive. The implementation can be sped up through multicore
# parallelization (see ?future::plan)
#----------------------------------------------------------------------

# load libraries
library(data.table)
library(SuperLearner)
library(future)
library(cvma)
library(glmnet)
library(xgboost)
library(e1071)
# note cvma may be installed from GitHub via
# devtools::install_github("benkeser/cvma")
library(cvma)
# define directory to source makeDataAndFunctions
codeDir <- "~/code/objective1/"
# define director to save results
outDir <- "~"
# run makeDataAndFunctions.R script
source(paste0(codeDir, "00-superlearner_prelims.R"))

# make future plan
# plan(multicore)
plan(sequential)

#----------------------------------------
# Analysis on data set 1
#----------------------------------------
Y.slope.mod <- Y.slope

# identifier of obs with complete data
idx <- !is.na(Y.80) & !is.na(Y.slope) & !(Y.slope == Inf)
idx.mod <- !is.na(Y.80) & !is.na(Y.slope)
# make a list of the relevant continuous outcomes
outcomes_list <- list(data.frame(ic50 = Y),
                      data.frame(ic80 = Y.80[!is.na(Y.80)]),
                      data.frame(slope_mod = Y.slope.mod[!is.na(Y.slope.mod)])
                    )
# make list of the relevant predictors
predictors_list <- list(X, X[!is.na(Y.80), ], X[!is.na(Y.slope.mod),])

# fit each outcome with super learner from cvma package using appropriate predictors
for(i in 1:length(outcomes_list)){
	this_name <- colnames(outcomes_list[[i]])
	cat(paste0("\n \n \n \n \n Fitting ", this_name, " on set 1. \n \n \n \n \n"))
	system.time(fit <- cvma(
  		Y = outcomes_list[[i]], X = predictors_list[[i]], 
  		learners = SL.library.continuous, V = 10,
      return_control = list(outer_weight = TRUE,
                            outer_sl = TRUE, inner_sl = TRUE, 
                            all_y = TRUE, all_learner_assoc = TRUE,
                            all_learner_fits = FALSE)))
	if(length(this_name) > 1){
		this_name <- ifelse("slope_mod" %in% this_name, "all_mod","all")
	}
	save(fit, file = paste0(outDir,"fit_",this_name,"_set1_v11_newest.RData"))
}

# make a list of the relevant dichotomous outcomes
outcomes_list <- list(data.frame(cens = Y.cens[!is.na(Y.cens)]),
                      data.frame(sens.resis = Y.sens.resis[!is.na(Y.sens.resis)]))

# make list of the relevant predictors
predictors_list <- list(X[!is.na(Y.cens), ], X[!is.na(Y.sens.resis), ])

for(i in 1:length(outcomes_list)){
	this_name <- colnames(outcomes_list[[i]])
	cat(paste0("\n \n \n \n \n Fitting ", this_name, " on set 1. \n \n \n \n \n \n"))
	system.time(fit <- cvma(
  		Y = outcomes_list[[i]], X = predictors_list[[i]], 
  		learners = SL.library.binary, V = 10, 
  		 sl_control = list(ensemble_fn = "ensemble_linear",
                                   optim_risk_fn = "optim_risk_sl_nloglik",
                                   weight_fn = "weight_sl_convex",
                                   cv_risk_fn = "cv_risk_sl_auc",
                                   family = binomial(),
                                   alpha = 0.05),
                y_weight_control = list(ensemble_fn = "ensemble_linear",
                                  weight_fn = "weight_y_01",
                                  optim_risk_fn = "optim_risk_y_auc",
                                  cv_risk_fn = "cv_risk_y_auc",
                                  alpha = 0.05),
                return_control = list(outer_weight = TRUE,
                                      outer_sl = TRUE, inner_sl = TRUE, 
                                      all_y = TRUE, all_learner_assoc = TRUE,
                                      all_learner_fits = FALSE)))
	save(fit, file = paste0(outDir, "fit_",
	                        colnames(outcomes_list[[i]]),"_set1_v11_newest.RData"))
}

#----------------------------------------
# Analysis on data set 2
#----------------------------------------
Y2.slope.mod <- Y2.slope

# identifier of obs with complete data
idx <- !is.na(Y2.80) & !is.na(Y2.slope) & !(Y2.slope == Inf)
idx.mod <- !is.na(Y2.80) & !is.na(Y2.slope)
# make a list of the relevant continuous outcomes
outcomes_list <- list(data.frame(ic50 = Y2),
                      data.frame(ic80 = Y2.80[!is.na(Y2.80)]),
                      data.frame(slope_mod = Y2.slope.mod[!is.na(Y2.slope.mod)])
                    )
# make list of the relevant predictors
predictors_list <- list(X2, X2[!is.na(Y2.80), ], X2[!is.na(Y2.slope.mod),])

for(i in 1:length(outcomes_list)){
  this_name <- colnames(outcomes_list[[i]])
	cat(paste0("\n \n \n \n \n Fitting ", this_name, " on set 2. \n \n \n \n \n"))
	system.time(fit <- cvma(
  		Y = outcomes_list[[i]], X = predictors_list[[i]], 
  		learners = SL.library.continuous, V = 10,
      return_control = list(outer_weight = TRUE,
                                      outer_sl = TRUE, inner_sl = TRUE, 
                                      all_y = TRUE, all_learner_assoc = TRUE,
                                      all_learner_fits = FALSE)))
	this_name <- colnames(outcomes_list[[i]])
	if(length(this_name) > 1){
		this_name <- ifelse("slope_mod" %in% this_name, "all_mod","all")
	}
	save(fit, file = paste0(outDir,
	                        this_name,"_set2_v11_newest.RData"))
}

# make a list of the relevant dichotomous outcomes
outcomes_list <- list(data.frame(cens = Y2.cens[!is.na(Y2.cens)]),
                      data.frame(sens.resis = Y2.sens.resis[!is.na(Y2.sens.resis)]))

# make list of the relevant predictors
predictors_list <- list(X2[!is.na(Y2.cens), ], X2[!is.na(Y2.sens.resis), ])

for(i in 1:length(outcomes_list)){
	this_name <- colnames(outcomes_list[[i]])
	cat(paste0("\n \n \n \n \n Fitting ", this_name, " on set 2. \n \n \n \n \n"))
	system.time(fit <- cvma(
  		Y = outcomes_list[[i]], X = predictors_list[[i]], 
  		learners = SL.library.binary, V = 10,
  		 sl_control = list(ensemble_fn = "ensemble_linear",
                                   optim_risk_fn = "optim_risk_sl_nloglik",
                                   weight_fn = "weight_sl_convex",
                                   cv_risk_fn = "cv_risk_sl_auc",
                                   family = binomial(),
                                   alpha = 0.05),
                y_weight_control = list(ensemble_fn = "ensemble_linear",
                                  weight_fn = "weight_y_01",
                                  optim_risk_fn = "optim_risk_y_auc",
                                  cv_risk_fn = "cv_risk_y_auc",
                                  alpha = 0.05),
                return_control = list(outer_weight = TRUE,
                                      outer_sl = TRUE, inner_sl = TRUE, 
                                      all_y = TRUE, all_learner_assoc = TRUE,
                                      all_learner_fits = FALSE)))
	save(fit, file = paste0(outDir,
	                        colnames(outcomes_list[[i]]),"_set2_v11_newest.RData"))
}
