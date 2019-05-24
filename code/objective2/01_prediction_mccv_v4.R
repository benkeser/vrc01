

# ---------------------------------------------------------------------------- #
# STEP -1:  prepare our environment
# ---------------------------------------------------------------------------- #


# refresh our workspace
rm (list=ls ())

# make this analysis deterministic
set.seed (1234)

# load required packages:  learners
library (glmnet)
library (lars)
library (randomForest)
library (e1071)
library (class)
library (xgboost)

# load required packages:  misc
library (ResourceSelection)
library (stringr)
library (ggplot2)
library (ROCR)
library (plotrix)
library (foreach)
library (doParallel)
#library (beepr)

# capture our session information
session.info <- sessionInfo ()

# set the scope of our parallelism
registerDoParallel (cores=4)


# ---------------------------------------------------------------------------- #
# STEP 0:  set up filesystem locations and load our project-specific libraries
# ---------------------------------------------------------------------------- #


# identify our filesystem locations; you will need to set "path.home" to the
# path of the repository on your local filesystem
path.home <- "/repository/home/path"
path.lib <- paste0 (path.home, "/code/objective2")
path.data <- paste0 (path.home, "/data")
path.rdata <- paste0 (path.home, "/data")

# load our library of analysis functions
setwd (path.lib)
source ('00_predlib_v3.Rlib')


# ---------------------------------------------------------------------------- #
# STEP 1:  load and prepare data
# ---------------------------------------------------------------------------- #


# load our data
setwd (path.data)
data.analysis.set1 <- read.csv ("data_analysis_set1.csv", header=T)
data.analysis.set2 <- read.csv ("data_analysis_set2.csv", header=T)

# set up data for analysis
#features.all <- c (7:17, 20:23, 25:26, 28, 30, 43:564)
#features <- c (7:17, 20:23, 43:564)
features.all <- c (7:17, 20:23, 25:26, 28, 30, 43:553)
features <- c (7:17, 20:23, 43:553)
classes <- c ('ic50.censored', 'binding.dichotomous.sens.resis',
              'ic50.geometric.mean.imputed.log10', 
              'ic80.geometric.mean.imputed.log10', 
              'neutralization.slope')
data.features.set1 <- data.analysis.set1[, features]
data.features.set2 <- data.analysis.set2[, features]
data.classes.set1 <- data.analysis.set1[, classes]
data.classes.set2 <- data.analysis.set2[, classes]

# define the geography variables that we want to control for
geography.vars <- c ("geographic.region.of.origin.is.Asia", 
                     "geographic.region.of.origin.is.Europe.Americas",
                     "geographic.region.of.origin.is.N.Africa",
                     "geographic.region.of.origin.is.S.Africa")

# immortalize our working environment
setwd (path.rdata)
datestamp <- date ()
save.image (file="analysis_final_01_load.Rdata")
#rm (list=ls ())
#load (file="analysis_final_01_load.Rdata")


# ---------------------------------------------------------------------------- #
# STEP 2:  ye olde LASSO analysis
# ---------------------------------------------------------------------------- #


# CONFIG:  define analytic parameters
set.seed (1234)
num.iter <- 1000
train.ratio <- 0.8

# dichotomous:  IC50 censored
print ("LASSO - Dichotomous - IC50 Censored - Set 1")
results.lasso.ic50.censored.set1 <- crunch.lasso.dichotomous (data.training.features=data.features.set1, 
                                                              data.training.class=data.classes.set1[, 1],
                                                              data.validation.features=data.features.set2, 
                                                              data.validation.class=data.classes.set2[, 1])
print ("LASSO - Dichotomous - IC50 Censored - Set 2")
results.lasso.ic50.censored.set2 <- crunch.lasso.dichotomous (data.training.features=data.features.set2, 
                                                              data.training.class=data.classes.set2[, 1],
                                                              data.validation.features=data.features.set1, 
                                                              data.validation.class=data.classes.set1[, 1])

# dichotomous:  sensitive/resistant
print ("LASSO - Dichotomous - Sensitive/Resistant - Set 1")
results.lasso.binding.sens.resis.set1 <- crunch.lasso.dichotomous (data.training.features=data.features.set1, 
                                                                   data.training.class=data.classes.set1[, 2],
                                                                   data.validation.features=data.features.set2, 
                                                                   data.validation.class=data.classes.set2[, 2])
print ("LASSO - Dichotomous - Sensitive/Resistant - Set 2")
results.lasso.binding.sens.resis.set2 <- crunch.lasso.dichotomous (data.training.features=data.features.set2, 
                                                                   data.training.class=data.classes.set2[, 2],
                                                                   data.validation.features=data.features.set1, 
                                                                   data.validation.class=data.classes.set1[, 2])

# continuous:  IC50
print ("LASSO - Continuous - IC50 - Set 1")
results.lasso.ic50.set1 <- crunch.lasso.continuous (data.training.features=data.features.set1, 
                                                    data.training.class=data.classes.set1[, 3],
                                                    data.validation.features=data.features.set2, 
                                                    data.validation.class=data.classes.set2[, 3])
print ("LASSO - Continuous - IC50 - Set 2")
results.lasso.ic50.set2 <- crunch.lasso.continuous (data.training.features=data.features.set2, 
                                                    data.training.class=data.classes.set2[, 3],
                                                    data.validation.features=data.features.set1, 
                                                    data.validation.class=data.classes.set1[, 3])

# continuous:  IC80
print ("LASSO - Continuous - IC80 - Set 1")
results.lasso.ic80.set1 <- crunch.lasso.continuous (data.training.features=data.features.set1, 
                                                    data.training.class=data.classes.set1[, 4],
                                                    data.validation.features=data.features.set2, 
                                                    data.validation.class=data.classes.set2[, 4])
print ("LASSO - Continuous - IC80 - Set 2")
results.lasso.ic80.set2 <- crunch.lasso.continuous (data.training.features=data.features.set2, 
                                                    data.training.class=data.classes.set2[, 4],
                                                    data.validation.features=data.features.set1, 
                                                    data.validation.class=data.classes.set1[, 4])

# continuous:  neutralization slope
print ("LASSO - Continuous - Neutralization Slope - Set 1")
results.lasso.slope.set1 <- crunch.lasso.continuous (data.training.features=data.features.set1, 
                                                     data.training.class=fix.infinite (data.classes.set1[, 5]),
                                                     data.validation.features=data.features.set2, 
                                                     data.validation.class=fix.infinite (data.classes.set2[, 5]))
print ("LASSO - Continuous - Neutralization Slope - Set 2")
results.lasso.slope.set2 <- crunch.lasso.continuous (data.training.features=data.features.set2, 
                                                     data.training.class=fix.infinite (data.classes.set2[, 5]),
                                                     data.validation.features=data.features.set1, 
                                                     data.validation.class=fix.infinite (data.classes.set1[, 5]))

# immortalize our working environment
setwd (path.rdata)
datestamp <- date ()
save.image (file="analysis_final_02_lasso.Rdata")
#rm (list=ls ())
#load (file="analysis_final_02_lasso.Rdata")


# ---------------------------------------------------------------------------- #
# STEP 3:  conduct ye olde Naive Bayes analysis
# ---------------------------------------------------------------------------- #


# CONFIG:  define analytic parameters
set.seed (1234)
num.iter <- 1000
train.ratio <- 0.8
#selection.threshold <- 0.8

# dichotomous:  IC50 censored
print ("Naive Bayes - Dichotomous - IC50 Censored - Set 1")
results.nb.ic50.censored.set1 <- crunch.nb.dichotomous (data.training.features=data.features.set1, 
                                                        data.training.class=data.classes.set1[, 1],
                                                        data.validation.features=data.features.set2, 
                                                        data.validation.class=data.classes.set2[, 1])
print ("Naive Bayes - Dichotomous - IC50 Censored - Set 2")
results.nb.ic50.censored.set2 <- crunch.nb.dichotomous (data.training.features=data.features.set2, 
                                                        data.training.class=data.classes.set2[, 1],
                                                        data.validation.features=data.features.set1, 
                                                        data.validation.class=data.classes.set1[, 1])

# dichotomous:  sensitive/resistant
print ("Naive Bayes - Dichotomous - Sensitive/Resistant - Set 1")
results.nb.binding.sens.resis.set1 <- crunch.nb.dichotomous (data.training.features=data.features.set1, 
                                                             data.training.class=data.classes.set1[, 2],
                                                             data.validation.features=data.features.set2, 
                                                             data.validation.class=data.classes.set2[, 2])
print ("Naive Bayes - Dichotomous - Sensitive/Resistant - Set 2")
results.nb.binding.sens.resis.set2 <- crunch.nb.dichotomous (data.training.features=data.features.set2, 
                                                             data.training.class=data.classes.set2[, 2],
                                                             data.validation.features=data.features.set1, 
                                                             data.validation.class=data.classes.set1[, 2])

# immortalize our working environment
setwd (path.rdata)
datestamp <- date ()
save.image (file="analysis_final_03_nb.Rdata")
#rm (list=ls ())
#load (file="analysis_final_03_nb.Rdata")


# ---------------------------------------------------------------------------- #
# STEP 4:  conduct ye olde Random Forest analysis
# ---------------------------------------------------------------------------- #


# CONFIG:  define analytic parameters
set.seed (1234)
num.iter <- 1000
train.ratio <- 0.8
#selection.threshold <- 0.8

# dichotomous:  IC50 censored
print ("Random Forest - Dichotomous - IC50 Censored - Set 1")
results.rf.ic50.censored.set1 <- crunch.rf.dichotomous (data.training.features=data.features.set1, 
                                                        data.training.class=data.classes.set1[, 1],
                                                        data.validation.features=data.features.set2, 
                                                        data.validation.class=data.classes.set2[, 1])
print ("Random Forest - Dichotomous - IC50 Censored - Set 2")
results.rf.ic50.censored.set2 <- crunch.rf.dichotomous (data.training.features=data.features.set2, 
                                                        data.training.class=data.classes.set2[, 1],
                                                        data.validation.features=data.features.set1, 
                                                        data.validation.class=data.classes.set1[, 1])

# dichotomous:  sensitive/resistant
print ("Random Forest - Dichotomous - Sensitive/Resistant - Set 1")
results.rf.binding.sens.resis.set1 <- crunch.rf.dichotomous (data.training.features=data.features.set1, 
                                                             data.training.class=data.classes.set1[, 2],
                                                             data.validation.features=data.features.set2, 
                                                             data.validation.class=data.classes.set2[, 2])
print ("Random Forest - Dichotomous - Sensitive/Resistant - Set 2")
results.rf.binding.sens.resis.set2 <- crunch.rf.dichotomous (data.training.features=data.features.set2, 
                                                             data.training.class=data.classes.set2[, 2],
                                                             data.validation.features=data.features.set1, 
                                                             data.validation.class=data.classes.set1[, 2])

# continuous:  IC50
print ("Random Forest - Continuous - IC50 - Set 1")
results.rf.ic50.set1 <- crunch.rf.continuous (data.training.features=data.features.set1, 
                                              data.training.class=data.classes.set1[, 3],
                                              data.validation.features=data.features.set2, 
                                              data.validation.class=data.classes.set2[, 3])
print ("Random Forest - Continuous - IC50 - Set 2")
results.rf.ic50.set2 <- crunch.rf.continuous (data.training.features=data.features.set2, 
                                              data.training.class=data.classes.set2[, 3],
                                              data.validation.features=data.features.set1, 
                                              data.validation.class=data.classes.set1[, 3])

# continuous:  IC80
print ("Random Forest - Continuous - IC80 - Set 1")
results.rf.ic80.set1 <- crunch.rf.continuous (data.training.features=data.features.set1, 
                                              data.training.class=data.classes.set1[, 4],
                                              data.validation.features=data.features.set2, 
                                              data.validation.class=data.classes.set2[, 4])
print ("Random Forest - Continuous - IC80 - Set 2")
results.rf.ic80.set2 <- crunch.rf.continuous (data.training.features=data.features.set2, 
                                              data.training.class=data.classes.set2[, 4],
                                              data.validation.features=data.features.set1, 
                                              data.validation.class=data.classes.set1[, 4])

# continuous:  neutralization slope
print ("Random Forest - Continuous - Neutralization Slope - Set 1")
results.rf.slope.noinf.set1 <- crunch.rf.continuous (data.training.features=data.features.set1, 
                                               data.training.class=fix.infinite (data.classes.set1[, 5]),
                                               data.validation.features=data.features.set2, 
                                               data.validation.class=fix.infinite (data.classes.set2[, 5]))
print ("Random Forest - Continuous - Neutralization Slope - Set 2")
results.rf.slope.noinf.set2 <- crunch.rf.continuous (data.training.features=data.features.set2, 
                                               data.training.class=fix.infinite (data.classes.set2[, 5]),
                                               data.validation.features=data.features.set1, 
                                               data.validation.class=fix.infinite (data.classes.set1[, 5]))

# immortalize our working environment
setwd (path.rdata)
datestamp <- date ()
save.image (file="analysis_final_04_rf.Rdata")
#rm (list=ls ())
#load (file="analysis_final_04_rf.Rdata")


# ---------------------------------------------------------------------------- #
# STEP 5:  ye olde gradient boosting analysis
# ---------------------------------------------------------------------------- #


# CONFIG:  define analytic parameters
set.seed (1234)
num.iter <- 1000
train.ratio <- 0.8
#selection.threshold <- 0.8

# dichotomous:  IC50 censored
print ("XGBoost - Dichotomous - IC50 Censored - Set 1")
results.xgb.ic50.censored.set1 <- crunch.xgb.dichotomous (data.training.features=data.features.set1, 
                                                          data.training.class=data.classes.set1[, 1],
                                                          data.validation.features=data.features.set2, 
                                                          data.validation.class=data.classes.set2[, 1])
print ("XGBoost - Dichotomous - IC50 Censored - Set 2")
results.xgb.ic50.censored.set2 <- crunch.xgb.dichotomous (data.training.features=data.features.set2, 
                                                          data.training.class=data.classes.set2[, 1],
                                                          data.validation.features=data.features.set1, 
                                                          data.validation.class=data.classes.set1[, 1])

# dichotomous:  sensitive/resistant
print ("XGBoost - Dichotomous - Sensitive/Resistant - Set 1")
results.xgb.binding.sens.resis.set1 <- crunch.xgb.dichotomous (data.training.features=data.features.set1, 
                                                               data.training.class=data.classes.set1[, 2],
                                                               data.validation.features=data.features.set2, 
                                                               data.validation.class=data.classes.set2[, 2])
print ("XGBoost - Dichotomous - Sensitive/Resistant - Set 2")
results.xgb.binding.sens.resis.set2 <- crunch.xgb.dichotomous (data.training.features=data.features.set2, 
                                                               data.training.class=data.classes.set2[, 2],
                                                               data.validation.features=data.features.set1, 
                                                               data.validation.class=data.classes.set1[, 2])

# continuous:  IC50
print ("XGBoost - Continuous - IC50 - Set 1")
results.xgb.ic50.set1 <- crunch.xgb.continuous (data.training.features=data.features.set1, 
                                                data.training.class=data.classes.set1[, 3],
                                                data.validation.features=data.features.set2, 
                                                data.validation.class=data.classes.set2[, 3])
print ("XGBoost - Continuous - IC50 - Set 2")
results.xgb.ic50.set2 <- crunch.xgb.continuous (data.training.features=data.features.set2, 
                                                data.training.class=data.classes.set2[, 3],
                                                data.validation.features=data.features.set1, 
                                                data.validation.class=data.classes.set1[, 3])

# continuous:  IC80
print ("XGBoost - Continuous - IC80 - Set 1")
results.xgb.ic80.set1 <- crunch.xgb.continuous (data.training.features=data.features.set1, 
                                                data.training.class=data.classes.set1[, 4],
                                                data.validation.features=data.features.set2, 
                                                data.validation.class=data.classes.set2[, 4])
print ("XGBoost - Continuous - IC80 - Set 2")
results.xgb.ic80.set2 <- crunch.xgb.continuous (data.training.features=data.features.set2, 
                                                data.training.class=data.classes.set2[, 4],
                                                data.validation.features=data.features.set1, 
                                                data.validation.class=data.classes.set1[, 4])

# continuous:  neutralization slope
print ("XGBoost - Continuous - Neutralization Slope (Imputed Infs) - Set 1")
results.xgb.slope.set1 <- crunch.xgb.continuous (data.training.features=data.features.set1, 
                                                 data.training.class=fix.infinite (data.classes.set1[, 5]),
                                                 data.validation.features=data.features.set2, 
                                                 data.validation.class=fix.infinite (data.classes.set2[, 5]))
print ("XGBoost - Continuous - Neutralization Slope (Imputed Infs) - Set 2")
results.xgb.slope.set2 <- crunch.xgb.continuous (data.training.features=data.features.set2, 
                                                 data.training.class=fix.infinite (data.classes.set2[, 5]),
                                                 data.validation.features=data.features.set1, 
                                                 data.validation.class=fix.infinite (data.classes.set1[, 5]))

# immortalize our working environment
setwd (path.rdata)
datestamp <- date ()
save.image (file="analysis_final_05_xgb.Rdata")
#rm (list=ls ())
#load (file="analysis_final_05_xgb.Rdata")


# ---------------------------------------------------------------------------- #
#                                    - 30 -
# ---------------------------------------------------------------------------- #



