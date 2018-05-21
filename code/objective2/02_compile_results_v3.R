

# ---------------------------------------------------------------------------- #
# STEP 0:  set things up
# ---------------------------------------------------------------------------- #


# refresh our workspace
rm (list=ls ())

# identify our filesystem locations; you will need to set "path.home" to the
# path of the repository on your local filesystem
path.home <- "/repository/home/path"
path.lib <- paste0 (path.home, "/code")
path.data <- paste0 (path.home, "/data")
path.rdata <- paste0 (path.home, "/data")

# load finished file
setwd (path.rdata)
load (file="analysis_final_05_xgb.rdata")

# set up our object for results
results.out <- data.frame (NULL)


# ---------------------------------------------------------------------------- #
# STEP 1:  LASSO
# ---------------------------------------------------------------------------- #


# IC50 censored
results.out <- rbind (results.out, data.frame (learner="lasso",
                                               endpoint="ic50.censored",
                                               data.set="set1",
                                               analysis.stage="cv",
                                               metric="auc",
                                               metric.results=results.lasso.ic50.censored.set1$auc.cv.mean,
                                               metric.ci.low=results.lasso.ic50.censored.set1$auc.cv.ci[1],
                                               metric.ci.high=results.lasso.ic50.censored.set1$auc.cv.ci[2]))
results.out <- rbind (results.out, data.frame (learner="lasso",
                                               endpoint="ic50.censored",
                                               data.set="set2",
                                               analysis.stage="cv",
                                               metric="auc",
                                               metric.results=results.lasso.ic50.censored.set2$auc.cv.mean,
                                               metric.ci.low=results.lasso.ic50.censored.set2$auc.cv.ci[1],
                                               metric.ci.high=results.lasso.ic50.censored.set2$auc.cv.ci[2]))
results.out <- rbind (results.out, data.frame (learner="lasso",
                                               endpoint="ic50.censored",
                                               data.set="set1",
                                               analysis.stage="training",
                                               metric="auc",
                                               metric.results=results.lasso.ic50.censored.set1$auc.training,
                                               metric.ci.low=results.lasso.ic50.censored.set1$auc.training.ci[1],
                                               metric.ci.high=results.lasso.ic50.censored.set1$auc.training.ci[2]))
results.out <- rbind (results.out, data.frame (learner="lasso",
                                               endpoint="ic50.censored",
                                               data.set="set2",
                                               analysis.stage="training",
                                               metric="auc",
                                               metric.results=results.lasso.ic50.censored.set2$auc.training,
                                               metric.ci.low=results.lasso.ic50.censored.set2$auc.training.ci[1],
                                               metric.ci.high=results.lasso.ic50.censored.set2$auc.training.ci[2]))
results.out <- rbind (results.out, data.frame (learner="lasso",
                                               endpoint="ic50.censored",
                                               data.set="set1",
                                               analysis.stage="validation",
                                               metric="auc",
                                               metric.results=results.lasso.ic50.censored.set1$auc.validation,
                                               metric.ci.low=results.lasso.ic50.censored.set1$auc.validation.ci[1],
                                               metric.ci.high=results.lasso.ic50.censored.set1$auc.validation.ci[2]))
results.out <- rbind (results.out, data.frame (learner="lasso",
                                               endpoint="ic50.censored",
                                               data.set="set2",
                                               analysis.stage="validation",
                                               metric="auc",
                                               metric.results=results.lasso.ic50.censored.set2$auc.validation,
                                               metric.ci.low=results.lasso.ic50.censored.set2$auc.validation.ci[1],
                                               metric.ci.high=results.lasso.ic50.censored.set2$auc.validation.ci[2]))

# binding sensitive/resistant
results.out <- rbind (results.out, data.frame (learner="lasso",
                                               endpoint="binding.sens.resis",
                                               data.set="set1",
                                               analysis.stage="cv",
                                               metric="auc",
                                               metric.results=results.lasso.binding.sens.resis.set1$auc.cv.mean,
                                               metric.ci.low=results.lasso.binding.sens.resis.set1$auc.cv.ci[1],
                                               metric.ci.high=results.lasso.binding.sens.resis.set1$auc.cv.ci[2]))
results.out <- rbind (results.out, data.frame (learner="lasso",
                                               endpoint="binding.sens.resis",
                                               data.set="set2",
                                               analysis.stage="cv",
                                               metric="auc",
                                               metric.results=results.lasso.binding.sens.resis.set2$auc.cv.mean,
                                               metric.ci.low=results.lasso.binding.sens.resis.set2$auc.cv.ci[1],
                                               metric.ci.high=results.lasso.binding.sens.resis.set2$auc.cv.ci[2]))
results.out <- rbind (results.out, data.frame (learner="lasso",
                                               endpoint="binding.sens.resis",
                                               data.set="set1",
                                               analysis.stage="training",
                                               metric="auc",
                                               metric.results=results.lasso.binding.sens.resis.set1$auc.training,
                                               metric.ci.low=results.lasso.binding.sens.resis.set1$auc.training.ci[1],
                                               metric.ci.high=results.lasso.binding.sens.resis.set1$auc.training.ci[2]))
results.out <- rbind (results.out, data.frame (learner="lasso",
                                               endpoint="binding.sens.resis",
                                               data.set="set2",
                                               analysis.stage="training",
                                               metric="auc",
                                               metric.results=results.lasso.binding.sens.resis.set2$auc.training,
                                               metric.ci.low=results.lasso.binding.sens.resis.set2$auc.training.ci[1],
                                               metric.ci.high=results.lasso.binding.sens.resis.set2$auc.training.ci[2]))
results.out <- rbind (results.out, data.frame (learner="lasso",
                                               endpoint="binding.sens.resis",
                                               data.set="set1",
                                               analysis.stage="validation",
                                               metric="auc",
                                               metric.results=results.lasso.binding.sens.resis.set1$auc.validation,
                                               metric.ci.low=results.lasso.binding.sens.resis.set1$auc.validation.ci[1],
                                               metric.ci.high=results.lasso.binding.sens.resis.set1$auc.validation.ci[2]))
results.out <- rbind (results.out, data.frame (learner="lasso",
                                               endpoint="binding.sens.resis",
                                               data.set="set2",
                                               analysis.stage="validation",
                                               metric="auc",
                                               metric.results=results.lasso.binding.sens.resis.set2$auc.validation,
                                               metric.ci.low=results.lasso.binding.sens.resis.set2$auc.validation.ci[1],
                                               metric.ci.high=results.lasso.binding.sens.resis.set2$auc.validation.ci[2]))

# ic50
results.out <- rbind (results.out, data.frame (learner="lasso",
                                               endpoint="ic50",
                                               data.set="set1",
                                               analysis.stage="cv",
                                               metric="r2",
                                               metric.results=results.lasso.ic50.set1$r2.cv.mean,
                                               metric.ci.low=results.lasso.ic50.set1$r2.cv.ci[1],
                                               metric.ci.high=results.lasso.ic50.set1$r2.cv.ci[2]))
results.out <- rbind (results.out, data.frame (learner="lasso",
                                               endpoint="ic50",
                                               data.set="set2",
                                               analysis.stage="cv",
                                               metric="r2",
                                               metric.results=results.lasso.ic50.set2$r2.cv.mean,
                                               metric.ci.low=results.lasso.ic50.set2$r2.cv.ci[1],
                                               metric.ci.high=results.lasso.ic50.set2$r2.cv.ci[2]))
results.out <- rbind (results.out, data.frame (learner="lasso",
                                               endpoint="ic50",
                                               data.set="set1",
                                               analysis.stage="training",
                                               metric="r2",
                                               metric.results=results.lasso.ic50.set1$r2.training,
                                               metric.ci.low=results.lasso.ic50.set1$r2.training.ci[1],
                                               metric.ci.high=results.lasso.ic50.set1$r2.training.ci[2]))
results.out <- rbind (results.out, data.frame (learner="lasso",
                                               endpoint="ic50",
                                               data.set="set2",
                                               analysis.stage="training",
                                               metric="r2",
                                               metric.results=results.lasso.ic50.set2$r2.training,
                                               metric.ci.low=results.lasso.ic50.set2$r2.training.ci[1],
                                               metric.ci.high=results.lasso.ic50.set2$r2.training.ci[2]))
results.out <- rbind (results.out, data.frame (learner="lasso",
                                               endpoint="ic50",
                                               data.set="set1",
                                               analysis.stage="validation",
                                               metric="r2",
                                               metric.results=results.lasso.ic50.set1$r2.validation,
                                               metric.ci.low=results.lasso.ic50.set1$r2.validation.ci[1],
                                               metric.ci.high=results.lasso.ic50.set1$r2.validation.ci[2]))
results.out <- rbind (results.out, data.frame (learner="lasso",
                                               endpoint="ic50",
                                               data.set="set2",
                                               analysis.stage="validation",
                                               metric="r2",
                                               metric.results=results.lasso.ic50.set2$r2.validation,
                                               metric.ci.low=results.lasso.ic50.set2$r2.validation.ci[1],
                                               metric.ci.high=results.lasso.ic50.set2$r2.validation.ci[2]))

# ic80
results.out <- rbind (results.out, data.frame (learner="lasso",
                                               endpoint="ic80",
                                               data.set="set1",
                                               analysis.stage="cv",
                                               metric="r2",
                                               metric.results=results.lasso.ic80.set1$r2.cv.mean,
                                               metric.ci.low=results.lasso.ic80.set1$r2.cv.ci[1],
                                               metric.ci.high=results.lasso.ic80.set1$r2.cv.ci[2]))
results.out <- rbind (results.out, data.frame (learner="lasso",
                                               endpoint="ic80",
                                               data.set="set2",
                                               analysis.stage="cv",
                                               metric="r2",
                                               metric.results=results.lasso.ic80.set2$r2.cv.mean,
                                               metric.ci.low=results.lasso.ic80.set2$r2.cv.ci[1],
                                               metric.ci.high=results.lasso.ic80.set2$r2.cv.ci[2]))
results.out <- rbind (results.out, data.frame (learner="lasso",
                                               endpoint="ic80",
                                               data.set="set1",
                                               analysis.stage="training",
                                               metric="r2",
                                               metric.results=results.lasso.ic80.set1$r2.training,
                                               metric.ci.low=results.lasso.ic80.set1$r2.training.ci[1],
                                               metric.ci.high=results.lasso.ic80.set1$r2.training.ci[2]))
results.out <- rbind (results.out, data.frame (learner="lasso",
                                               endpoint="ic80",
                                               data.set="set2",
                                               analysis.stage="training",
                                               metric="r2",
                                               metric.results=results.lasso.ic80.set2$r2.training,
                                               metric.ci.low=results.lasso.ic80.set2$r2.training.ci[1],
                                               metric.ci.high=results.lasso.ic80.set2$r2.training.ci[2]))
results.out <- rbind (results.out, data.frame (learner="lasso",
                                               endpoint="ic80",
                                               data.set="set1",
                                               analysis.stage="validation",
                                               metric="r2",
                                               metric.results=results.lasso.ic80.set1$r2.validation,
                                               metric.ci.low=results.lasso.ic80.set1$r2.validation.ci[1],
                                               metric.ci.high=results.lasso.ic80.set1$r2.validation.ci[2]))
results.out <- rbind (results.out, data.frame (learner="lasso",
                                               endpoint="ic80",
                                               data.set="set2",
                                               analysis.stage="validation",
                                               metric="r2",
                                               metric.results=results.lasso.ic80.set2$r2.validation,
                                               metric.ci.low=results.lasso.ic80.set2$r2.validation.ci[1],
                                               metric.ci.high=results.lasso.ic80.set2$r2.validation.ci[2]))

# neutralization slope
results.out <- rbind (results.out, data.frame (learner="lasso",
                                               endpoint="slope",
                                               data.set="set1",
                                               analysis.stage="cv",
                                               metric="r2",
                                               metric.results=results.lasso.slope.set1$r2.cv.mean,
                                               metric.ci.low=results.lasso.slope.set1$r2.cv.ci[1],
                                               metric.ci.high=results.lasso.slope.set1$r2.cv.ci[2]))
results.out <- rbind (results.out, data.frame (learner="lasso",
                                               endpoint="slope",
                                               data.set="set2",
                                               analysis.stage="cv",
                                               metric="r2",
                                               metric.results=results.lasso.slope.set2$r2.cv.mean,
                                               metric.ci.low=results.lasso.slope.set2$r2.cv.ci[1],
                                               metric.ci.high=results.lasso.slope.set2$r2.cv.ci[2]))
results.out <- rbind (results.out, data.frame (learner="lasso",
                                               endpoint="slope",
                                               data.set="set1",
                                               analysis.stage="training",
                                               metric="r2",
                                               metric.results=results.lasso.slope.set1$r2.training,
                                               metric.ci.low=results.lasso.slope.set1$r2.training.ci[1],
                                               metric.ci.high=results.lasso.slope.set1$r2.training.ci[2]))
results.out <- rbind (results.out, data.frame (learner="lasso",
                                               endpoint="slope",
                                               data.set="set2",
                                               analysis.stage="training",
                                               metric="r2",
                                               metric.results=results.lasso.slope.set2$r2.training,
                                               metric.ci.low=results.lasso.slope.set2$r2.training.ci[1],
                                               metric.ci.high=results.lasso.slope.set2$r2.training.ci[2]))
results.out <- rbind (results.out, data.frame (learner="lasso",
                                               endpoint="slope",
                                               data.set="set1",
                                               analysis.stage="validation",
                                               metric="r2",
                                               metric.results=results.lasso.slope.set1$r2.validation,
                                               metric.ci.low=results.lasso.slope.set1$r2.validation.ci[1],
                                               metric.ci.high=results.lasso.slope.set1$r2.validation.ci[2]))
results.out <- rbind (results.out, data.frame (learner="lasso",
                                               endpoint="slope",
                                               data.set="set2",
                                               analysis.stage="validation",
                                               metric="r2",
                                               metric.results=results.lasso.slope.set2$r2.validation,
                                               metric.ci.low=results.lasso.slope.set2$r2.validation.ci[1],
                                               metric.ci.high=results.lasso.slope.set2$r2.validation.ci[2]))


# ---------------------------------------------------------------------------- #
# STEP 2:  Naive Bayes
# ---------------------------------------------------------------------------- #


# IC50 censored
results.out <- rbind (results.out, data.frame (learner="naive.bayes",
                                               endpoint="ic50.censored",
                                               data.set="set1",
                                               analysis.stage="cv",
                                               metric="auc",
                                               metric.results=results.nb.ic50.censored.set1$auc.cv.mean,
                                               metric.ci.low=results.nb.ic50.censored.set1$auc.cv.ci[1],
                                               metric.ci.high=results.nb.ic50.censored.set1$auc.cv.ci[2]))
results.out <- rbind (results.out, data.frame (learner="naive.bayes",
                                               endpoint="ic50.censored",
                                               data.set="set2",
                                               analysis.stage="cv",
                                               metric="auc",
                                               metric.results=results.nb.ic50.censored.set2$auc.cv.mean,
                                               metric.ci.low=results.nb.ic50.censored.set2$auc.cv.ci[1],
                                               metric.ci.high=results.nb.ic50.censored.set2$auc.cv.ci[2]))
results.out <- rbind (results.out, data.frame (learner="naive.bayes",
                                               endpoint="ic50.censored",
                                               data.set="set1",
                                               analysis.stage="training",
                                               metric="auc",
                                               metric.results=results.nb.ic50.censored.set1$auc.training,
                                               metric.ci.low=results.nb.ic50.censored.set1$auc.training.ci[1],
                                               metric.ci.high=results.nb.ic50.censored.set1$auc.training.ci[2]))
results.out <- rbind (results.out, data.frame (learner="naive.bayes",
                                               endpoint="ic50.censored",
                                               data.set="set2",
                                               analysis.stage="training",
                                               metric="auc",
                                               metric.results=results.nb.ic50.censored.set2$auc.training,
                                               metric.ci.low=results.nb.ic50.censored.set2$auc.training.ci[1],
                                               metric.ci.high=results.nb.ic50.censored.set2$auc.training.ci[2]))
results.out <- rbind (results.out, data.frame (learner="naive.bayes",
                                               endpoint="ic50.censored",
                                               data.set="set1",
                                               analysis.stage="validation",
                                               metric="auc",
                                               metric.results=results.nb.ic50.censored.set1$auc.validation,
                                               metric.ci.low=results.nb.ic50.censored.set1$auc.validation.ci[1],
                                               metric.ci.high=results.nb.ic50.censored.set1$auc.validation.ci[2]))
results.out <- rbind (results.out, data.frame (learner="naive.bayes",
                                               endpoint="ic50.censored",
                                               data.set="set2",
                                               analysis.stage="validation",
                                               metric="auc",
                                               metric.results=results.nb.ic50.censored.set2$auc.validation,
                                               metric.ci.low=results.nb.ic50.censored.set2$auc.validation.ci[1],
                                               metric.ci.high=results.nb.ic50.censored.set2$auc.validation.ci[2]))

# binding sensitive/resistant
results.out <- rbind (results.out, data.frame (learner="naive.bayes",
                                               endpoint="binding.sens.resis",
                                               data.set="set1",
                                               analysis.stage="cv",
                                               metric="auc",
                                               metric.results=results.nb.binding.sens.resis.set1$auc.cv.mean,
                                               metric.ci.low=results.nb.binding.sens.resis.set1$auc.cv.ci[1],
                                               metric.ci.high=results.nb.binding.sens.resis.set1$auc.cv.ci[2]))
results.out <- rbind (results.out, data.frame (learner="naive.bayes",
                                               endpoint="binding.sens.resis",
                                               data.set="set2",
                                               analysis.stage="cv",
                                               metric="auc",
                                               metric.results=results.nb.binding.sens.resis.set2$auc.cv.mean,
                                               metric.ci.low=results.nb.binding.sens.resis.set2$auc.cv.ci[1],
                                               metric.ci.high=results.nb.binding.sens.resis.set2$auc.cv.ci[2]))
results.out <- rbind (results.out, data.frame (learner="naive.bayes",
                                               endpoint="binding.sens.resis",
                                               data.set="set1",
                                               analysis.stage="training",
                                               metric="auc",
                                               metric.results=results.nb.binding.sens.resis.set1$auc.training,
                                               metric.ci.low=results.nb.binding.sens.resis.set1$auc.training.ci[1],
                                               metric.ci.high=results.nb.binding.sens.resis.set1$auc.training.ci[2]))
results.out <- rbind (results.out, data.frame (learner="naive.bayes",
                                               endpoint="binding.sens.resis",
                                               data.set="set2",
                                               analysis.stage="training",
                                               metric="auc",
                                               metric.results=results.nb.binding.sens.resis.set2$auc.training,
                                               metric.ci.low=results.nb.binding.sens.resis.set2$auc.training.ci[1],
                                               metric.ci.high=results.nb.binding.sens.resis.set2$auc.training.ci[2]))
results.out <- rbind (results.out, data.frame (learner="naive.bayes",
                                               endpoint="binding.sens.resis",
                                               data.set="set1",
                                               analysis.stage="validation",
                                               metric="auc",
                                               metric.results=results.nb.binding.sens.resis.set1$auc.validation,
                                               metric.ci.low=results.nb.binding.sens.resis.set1$auc.validation.ci[1],
                                               metric.ci.high=results.nb.binding.sens.resis.set1$auc.validation.ci[2]))
results.out <- rbind (results.out, data.frame (learner="naive.bayes",
                                               endpoint="binding.sens.resis",
                                               data.set="set2",
                                               analysis.stage="validation",
                                               metric="auc",
                                               metric.results=results.nb.binding.sens.resis.set2$auc.validation,
                                               metric.ci.low=results.nb.binding.sens.resis.set2$auc.validation.ci[1],
                                               metric.ci.high=results.nb.binding.sens.resis.set2$auc.validation.ci[2]))


# ---------------------------------------------------------------------------- #
# STEP 3:  Random Forest
# ---------------------------------------------------------------------------- #


# IC50 censored
results.out <- rbind (results.out, data.frame (learner="random.forest",
                                               endpoint="ic50.censored",
                                               data.set="set1",
                                               analysis.stage="cv",
                                               metric="auc",
                                               metric.results=results.rf.ic50.censored.set1$auc.cv.mean,
                                               metric.ci.low=results.rf.ic50.censored.set1$auc.cv.ci[1],
                                               metric.ci.high=results.rf.ic50.censored.set1$auc.cv.ci[2]))
results.out <- rbind (results.out, data.frame (learner="random.forest",
                                               endpoint="ic50.censored",
                                               data.set="set2",
                                               analysis.stage="cv",
                                               metric="auc",
                                               metric.results=results.rf.ic50.censored.set2$auc.cv.mean,
                                               metric.ci.low=results.rf.ic50.censored.set2$auc.cv.ci[1],
                                               metric.ci.high=results.rf.ic50.censored.set2$auc.cv.ci[2]))
results.out <- rbind (results.out, data.frame (learner="random.forest",
                                               endpoint="ic50.censored",
                                               data.set="set1",
                                               analysis.stage="training",
                                               metric="auc",
                                               metric.results=results.rf.ic50.censored.set1$auc.training,
                                               metric.ci.low=results.rf.ic50.censored.set1$auc.training.ci[1],
                                               metric.ci.high=results.rf.ic50.censored.set1$auc.training.ci[2]))
results.out <- rbind (results.out, data.frame (learner="random.forest",
                                               endpoint="ic50.censored",
                                               data.set="set2",
                                               analysis.stage="training",
                                               metric="auc",
                                               metric.results=results.rf.ic50.censored.set2$auc.training,
                                               metric.ci.low=results.rf.ic50.censored.set2$auc.training.ci[1],
                                               metric.ci.high=results.rf.ic50.censored.set2$auc.training.ci[2]))
results.out <- rbind (results.out, data.frame (learner="random.forest",
                                               endpoint="ic50.censored",
                                               data.set="set1",
                                               analysis.stage="validation",
                                               metric="auc",
                                               metric.results=results.rf.ic50.censored.set1$auc.validation,
                                               metric.ci.low=results.rf.ic50.censored.set1$auc.validation.ci[1],
                                               metric.ci.high=results.rf.ic50.censored.set1$auc.validation.ci[2]))
results.out <- rbind (results.out, data.frame (learner="random.forest",
                                               endpoint="ic50.censored",
                                               data.set="set2",
                                               analysis.stage="validation",
                                               metric="auc",
                                               metric.results=results.rf.ic50.censored.set2$auc.validation,
                                               metric.ci.low=results.rf.ic50.censored.set2$auc.validation.ci[1],
                                               metric.ci.high=results.rf.ic50.censored.set2$auc.validation.ci[2]))

# binding sensitive/resistant
results.out <- rbind (results.out, data.frame (learner="random.forest",
                                               endpoint="binding.sens.resis",
                                               data.set="set1",
                                               analysis.stage="cv",
                                               metric="auc",
                                               metric.results=results.rf.binding.sens.resis.set1$auc.cv.mean,
                                               metric.ci.low=results.rf.binding.sens.resis.set1$auc.cv.ci[1],
                                               metric.ci.high=results.rf.binding.sens.resis.set1$auc.cv.ci[2]))
results.out <- rbind (results.out, data.frame (learner="random.forest",
                                               endpoint="binding.sens.resis",
                                               data.set="set2",
                                               analysis.stage="cv",
                                               metric="auc",
                                               metric.results=results.rf.binding.sens.resis.set2$auc.cv.mean,
                                               metric.ci.low=results.rf.binding.sens.resis.set2$auc.cv.ci[1],
                                               metric.ci.high=results.rf.binding.sens.resis.set2$auc.cv.ci[2]))
results.out <- rbind (results.out, data.frame (learner="random.forest",
                                               endpoint="binding.sens.resis",
                                               data.set="set1",
                                               analysis.stage="training",
                                               metric="auc",
                                               metric.results=results.rf.binding.sens.resis.set1$auc.training,
                                               metric.ci.low=results.rf.binding.sens.resis.set1$auc.training.ci[1],
                                               metric.ci.high=results.rf.binding.sens.resis.set1$auc.training.ci[2]))
results.out <- rbind (results.out, data.frame (learner="random.forest",
                                               endpoint="binding.sens.resis",
                                               data.set="set2",
                                               analysis.stage="training",
                                               metric="auc",
                                               metric.results=results.rf.binding.sens.resis.set2$auc.training,
                                               metric.ci.low=results.rf.binding.sens.resis.set2$auc.training.ci[1],
                                               metric.ci.high=results.rf.binding.sens.resis.set2$auc.training.ci[2]))
results.out <- rbind (results.out, data.frame (learner="random.forest",
                                               endpoint="binding.sens.resis",
                                               data.set="set1",
                                               analysis.stage="validation",
                                               metric="auc",
                                               metric.results=results.rf.binding.sens.resis.set1$auc.validation,
                                               metric.ci.low=results.rf.binding.sens.resis.set1$auc.validation.ci[1],
                                               metric.ci.high=results.rf.binding.sens.resis.set1$auc.validation.ci[2]))
results.out <- rbind (results.out, data.frame (learner="random.forest",
                                               endpoint="binding.sens.resis",
                                               data.set="set2",
                                               analysis.stage="validation",
                                               metric="auc",
                                               metric.results=results.rf.binding.sens.resis.set2$auc.validation,
                                               metric.ci.low=results.rf.binding.sens.resis.set2$auc.validation.ci[1],
                                               metric.ci.high=results.rf.binding.sens.resis.set2$auc.validation.ci[2]))

# ic50
results.out <- rbind (results.out, data.frame (learner="random.forest",
                                               endpoint="ic50",
                                               data.set="set1",
                                               analysis.stage="cv",
                                               metric="r2",
                                               metric.results=results.rf.ic50.set1$r2.cv.mean,
                                               metric.ci.low=results.rf.ic50.set1$r2.cv.ci[1],
                                               metric.ci.high=results.rf.ic50.set1$r2.cv.ci[2]))
results.out <- rbind (results.out, data.frame (learner="random.forest",
                                               endpoint="ic50",
                                               data.set="set2",
                                               analysis.stage="cv",
                                               metric="r2",
                                               metric.results=results.rf.ic50.set2$r2.cv.mean,
                                               metric.ci.low=results.rf.ic50.set2$r2.cv.ci[1],
                                               metric.ci.high=results.rf.ic50.set2$r2.cv.ci[2]))
results.out <- rbind (results.out, data.frame (learner="random.forest",
                                               endpoint="ic50",
                                               data.set="set1",
                                               analysis.stage="training",
                                               metric="r2",
                                               metric.results=results.rf.ic50.set1$r2.training,
                                               metric.ci.low=results.rf.ic50.set1$r2.training.ci[1],
                                               metric.ci.high=results.rf.ic50.set1$r2.training.ci[2]))
results.out <- rbind (results.out, data.frame (learner="random.forest",
                                               endpoint="ic50",
                                               data.set="set2",
                                               analysis.stage="training",
                                               metric="r2",
                                               metric.results=results.rf.ic50.set2$r2.training,
                                               metric.ci.low=results.rf.ic50.set2$r2.training.ci[1],
                                               metric.ci.high=results.rf.ic50.set2$r2.training.ci[2]))
results.out <- rbind (results.out, data.frame (learner="random.forest",
                                               endpoint="ic50",
                                               data.set="set1",
                                               analysis.stage="validation",
                                               metric="r2",
                                               metric.results=results.rf.ic50.set1$r2.validation,
                                               metric.ci.low=results.rf.ic50.set1$r2.validation.ci[1],
                                               metric.ci.high=results.rf.ic50.set1$r2.validation.ci[2]))
results.out <- rbind (results.out, data.frame (learner="random.forest",
                                               endpoint="ic50",
                                               data.set="set2",
                                               analysis.stage="validation",
                                               metric="r2",
                                               metric.results=results.rf.ic50.set2$r2.validation,
                                               metric.ci.low=results.rf.ic50.set2$r2.validation.ci[1],
                                               metric.ci.high=results.rf.ic50.set2$r2.validation.ci[2]))

# ic80
results.out <- rbind (results.out, data.frame (learner="random.forest",
                                               endpoint="ic80",
                                               data.set="set1",
                                               analysis.stage="cv",
                                               metric="r2",
                                               metric.results=results.rf.ic80.set1$r2.cv.mean,
                                               metric.ci.low=results.rf.ic80.set1$r2.cv.ci[1],
                                               metric.ci.high=results.rf.ic80.set1$r2.cv.ci[2]))
results.out <- rbind (results.out, data.frame (learner="random.forest",
                                               endpoint="ic80",
                                               data.set="set2",
                                               analysis.stage="cv",
                                               metric="r2",
                                               metric.results=results.rf.ic80.set2$r2.cv.mean,
                                               metric.ci.low=results.rf.ic80.set2$r2.cv.ci[1],
                                               metric.ci.high=results.rf.ic80.set2$r2.cv.ci[2]))
results.out <- rbind (results.out, data.frame (learner="random.forest",
                                               endpoint="ic80",
                                               data.set="set1",
                                               analysis.stage="training",
                                               metric="r2",
                                               metric.results=results.rf.ic80.set1$r2.training,
                                               metric.ci.low=results.rf.ic80.set1$r2.training.ci[1],
                                               metric.ci.high=results.rf.ic80.set1$r2.training.ci[2]))
results.out <- rbind (results.out, data.frame (learner="random.forest",
                                               endpoint="ic80",
                                               data.set="set2",
                                               analysis.stage="training",
                                               metric="r2",
                                               metric.results=results.rf.ic80.set2$r2.training,
                                               metric.ci.low=results.rf.ic80.set2$r2.training.ci[1],
                                               metric.ci.high=results.rf.ic80.set2$r2.training.ci[2]))
results.out <- rbind (results.out, data.frame (learner="random.forest",
                                               endpoint="ic80",
                                               data.set="set1",
                                               analysis.stage="validation",
                                               metric="r2",
                                               metric.results=results.rf.ic80.set1$r2.validation,
                                               metric.ci.low=results.rf.ic80.set1$r2.validation.ci[1],
                                               metric.ci.high=results.rf.ic80.set1$r2.validation.ci[2]))
results.out <- rbind (results.out, data.frame (learner="random.forest",
                                               endpoint="ic80",
                                               data.set="set2",
                                               analysis.stage="validation",
                                               metric="r2",
                                               metric.results=results.rf.ic80.set2$r2.validation,
                                               metric.ci.low=results.rf.ic80.set2$r2.validation.ci[1],
                                               metric.ci.high=results.rf.ic80.set2$r2.validation.ci[2]))

# neutralization slope
results.out <- rbind (results.out, data.frame (learner="random.forest",
                                               endpoint="slope",
                                               data.set="set1",
                                               analysis.stage="cv",
                                               metric="r2",
                                               metric.results=results.rf.slope.noinf.set1$r2.cv.mean,
                                               metric.ci.low=results.rf.slope.noinf.set1$r2.cv.ci[1],
                                               metric.ci.high=results.rf.slope.noinf.set1$r2.cv.ci[2]))
results.out <- rbind (results.out, data.frame (learner="random.forest",
                                               endpoint="slope",
                                               data.set="set2",
                                               analysis.stage="cv",
                                               metric="r2",
                                               metric.results=results.rf.slope.noinf.set2$r2.cv.mean,
                                               metric.ci.low=results.rf.slope.noinf.set2$r2.cv.ci[1],
                                               metric.ci.high=results.rf.slope.noinf.set2$r2.cv.ci[2]))
results.out <- rbind (results.out, data.frame (learner="random.forest",
                                               endpoint="slope",
                                               data.set="set1",
                                               analysis.stage="training",
                                               metric="r2",
                                               metric.results=results.rf.slope.noinf.set1$r2.training,
                                               metric.ci.low=results.rf.slope.noinf.set1$r2.training.ci[1],
                                               metric.ci.high=results.rf.slope.noinf.set1$r2.training.ci[2]))
results.out <- rbind (results.out, data.frame (learner="random.forest",
                                               endpoint="slope",
                                               data.set="set2",
                                               analysis.stage="training",
                                               metric="r2",
                                               metric.results=results.rf.slope.noinf.set2$r2.training,
                                               metric.ci.low=results.rf.slope.noinf.set2$r2.training.ci[1],
                                               metric.ci.high=results.rf.slope.noinf.set2$r2.training.ci[2]))
results.out <- rbind (results.out, data.frame (learner="random.forest",
                                               endpoint="slope",
                                               data.set="set1",
                                               analysis.stage="validation",
                                               metric="r2",
                                               metric.results=results.rf.slope.noinf.set1$r2.validation,
                                               metric.ci.low=results.rf.slope.noinf.set1$r2.validation.ci[1],
                                               metric.ci.high=results.rf.slope.noinf.set1$r2.validation.ci[2]))
results.out <- rbind (results.out, data.frame (learner="random.forest",
                                               endpoint="slope",
                                               data.set="set2",
                                               analysis.stage="validation",
                                               metric="r2",
                                               metric.results=results.rf.slope.noinf.set2$r2.validation,
                                               metric.ci.low=results.rf.slope.noinf.set2$r2.validation.ci[1],
                                               metric.ci.high=results.rf.slope.noinf.set2$r2.validation.ci[2]))


# ---------------------------------------------------------------------------- #
# STEP 4:  XGBoost
# ---------------------------------------------------------------------------- #


# IC50 censored
results.out <- rbind (results.out, data.frame (learner="xbgoost",
                                               endpoint="ic50.censored",
                                               data.set="set1",
                                               analysis.stage="cv",
                                               metric="auc",
                                               metric.results=results.xgb.ic50.censored.set1$auc.cv.mean,
                                               metric.ci.low=results.xgb.ic50.censored.set1$auc.cv.ci[1],
                                               metric.ci.high=results.xgb.ic50.censored.set1$auc.cv.ci[2]))
results.out <- rbind (results.out, data.frame (learner="xbgoost",
                                               endpoint="ic50.censored",
                                               data.set="set2",
                                               analysis.stage="cv",
                                               metric="auc",
                                               metric.results=results.xgb.ic50.censored.set2$auc.cv.mean,
                                               metric.ci.low=results.xgb.ic50.censored.set2$auc.cv.ci[1],
                                               metric.ci.high=results.xgb.ic50.censored.set2$auc.cv.ci[2]))
results.out <- rbind (results.out, data.frame (learner="xbgoost",
                                               endpoint="ic50.censored",
                                               data.set="set1",
                                               analysis.stage="training",
                                               metric="auc",
                                               metric.results=results.xgb.ic50.censored.set1$auc.training,
                                               metric.ci.low=results.xgb.ic50.censored.set1$auc.training.ci[1],
                                               metric.ci.high=results.xgb.ic50.censored.set1$auc.training.ci[2]))
results.out <- rbind (results.out, data.frame (learner="xbgoost",
                                               endpoint="ic50.censored",
                                               data.set="set2",
                                               analysis.stage="training",
                                               metric="auc",
                                               metric.results=results.xgb.ic50.censored.set2$auc.training,
                                               metric.ci.low=results.xgb.ic50.censored.set2$auc.training.ci[1],
                                               metric.ci.high=results.xgb.ic50.censored.set2$auc.training.ci[2]))
results.out <- rbind (results.out, data.frame (learner="xbgoost",
                                               endpoint="ic50.censored",
                                               data.set="set1",
                                               analysis.stage="validation",
                                               metric="auc",
                                               metric.results=results.xgb.ic50.censored.set1$auc.validation,
                                               metric.ci.low=results.xgb.ic50.censored.set1$auc.validation.ci[1],
                                               metric.ci.high=results.xgb.ic50.censored.set1$auc.validation.ci[2]))
results.out <- rbind (results.out, data.frame (learner="xbgoost",
                                               endpoint="ic50.censored",
                                               data.set="set2",
                                               analysis.stage="validation",
                                               metric="auc",
                                               metric.results=results.xgb.ic50.censored.set2$auc.validation,
                                               metric.ci.low=results.xgb.ic50.censored.set2$auc.validation.ci[1],
                                               metric.ci.high=results.xgb.ic50.censored.set2$auc.validation.ci[2]))

# binding sensitive/resistant
results.out <- rbind (results.out, data.frame (learner="xbgoost",
                                               endpoint="binding.sens.resis",
                                               data.set="set1",
                                               analysis.stage="cv",
                                               metric="auc",
                                               metric.results=results.xgb.binding.sens.resis.set1$auc.cv.mean,
                                               metric.ci.low=results.xgb.binding.sens.resis.set1$auc.cv.ci[1],
                                               metric.ci.high=results.xgb.binding.sens.resis.set1$auc.cv.ci[2]))
results.out <- rbind (results.out, data.frame (learner="xbgoost",
                                               endpoint="binding.sens.resis",
                                               data.set="set2",
                                               analysis.stage="cv",
                                               metric="auc",
                                               metric.results=results.xgb.binding.sens.resis.set2$auc.cv.mean,
                                               metric.ci.low=results.xgb.binding.sens.resis.set2$auc.cv.ci[1],
                                               metric.ci.high=results.xgb.binding.sens.resis.set2$auc.cv.ci[2]))
results.out <- rbind (results.out, data.frame (learner="xbgoost",
                                               endpoint="binding.sens.resis",
                                               data.set="set1",
                                               analysis.stage="training",
                                               metric="auc",
                                               metric.results=results.xgb.binding.sens.resis.set1$auc.training,
                                               metric.ci.low=results.xgb.binding.sens.resis.set1$auc.training.ci[1],
                                               metric.ci.high=results.xgb.binding.sens.resis.set1$auc.training.ci[2]))
results.out <- rbind (results.out, data.frame (learner="xbgoost",
                                               endpoint="binding.sens.resis",
                                               data.set="set2",
                                               analysis.stage="training",
                                               metric="auc",
                                               metric.results=results.xgb.binding.sens.resis.set2$auc.training,
                                               metric.ci.low=results.xgb.binding.sens.resis.set2$auc.training.ci[1],
                                               metric.ci.high=results.xgb.binding.sens.resis.set2$auc.training.ci[2]))
results.out <- rbind (results.out, data.frame (learner="xbgoost",
                                               endpoint="binding.sens.resis",
                                               data.set="set1",
                                               analysis.stage="validation",
                                               metric="auc",
                                               metric.results=results.xgb.binding.sens.resis.set1$auc.validation,
                                               metric.ci.low=results.xgb.binding.sens.resis.set1$auc.validation.ci[1],
                                               metric.ci.high=results.xgb.binding.sens.resis.set1$auc.validation.ci[2]))
results.out <- rbind (results.out, data.frame (learner="xbgoost",
                                               endpoint="binding.sens.resis",
                                               data.set="set2",
                                               analysis.stage="validation",
                                               metric="auc",
                                               metric.results=results.xgb.binding.sens.resis.set2$auc.validation,
                                               metric.ci.low=results.xgb.binding.sens.resis.set2$auc.validation.ci[1],
                                               metric.ci.high=results.xgb.binding.sens.resis.set2$auc.validation.ci[2]))

# ic50
results.out <- rbind (results.out, data.frame (learner="xbgoost",
                                               endpoint="ic50",
                                               data.set="set1",
                                               analysis.stage="cv",
                                               metric="r2",
                                               metric.results=results.xgb.ic50.set1$r2.cv.mean,
                                               metric.ci.low=results.xgb.ic50.set1$r2.cv.ci[1],
                                               metric.ci.high=results.xgb.ic50.set1$r2.cv.ci[2]))
results.out <- rbind (results.out, data.frame (learner="xbgoost",
                                               endpoint="ic50",
                                               data.set="set2",
                                               analysis.stage="cv",
                                               metric="r2",
                                               metric.results=results.xgb.ic50.set2$r2.cv.mean,
                                               metric.ci.low=results.xgb.ic50.set2$r2.cv.ci[1],
                                               metric.ci.high=results.xgb.ic50.set2$r2.cv.ci[2]))
results.out <- rbind (results.out, data.frame (learner="xbgoost",
                                               endpoint="ic50",
                                               data.set="set1",
                                               analysis.stage="training",
                                               metric="r2",
                                               metric.results=results.xgb.ic50.set1$r2.training,
                                               metric.ci.low=results.xgb.ic50.set1$r2.training.ci[1],
                                               metric.ci.high=results.xgb.ic50.set1$r2.training.ci[2]))
results.out <- rbind (results.out, data.frame (learner="xbgoost",
                                               endpoint="ic50",
                                               data.set="set2",
                                               analysis.stage="training",
                                               metric="r2",
                                               metric.results=results.xgb.ic50.set2$r2.training,
                                               metric.ci.low=results.xgb.ic50.set2$r2.training.ci[1],
                                               metric.ci.high=results.xgb.ic50.set2$r2.training.ci[2]))
results.out <- rbind (results.out, data.frame (learner="xbgoost",
                                               endpoint="ic50",
                                               data.set="set1",
                                               analysis.stage="validation",
                                               metric="r2",
                                               metric.results=results.xgb.ic50.set1$r2.validation,
                                               metric.ci.low=results.xgb.ic50.set1$r2.validation.ci[1],
                                               metric.ci.high=results.xgb.ic50.set1$r2.validation.ci[2]))
results.out <- rbind (results.out, data.frame (learner="xbgoost",
                                               endpoint="ic50",
                                               data.set="set2",
                                               analysis.stage="validation",
                                               metric="r2",
                                               metric.results=results.xgb.ic50.set2$r2.validation,
                                               metric.ci.low=results.xgb.ic50.set2$r2.validation.ci[1],
                                               metric.ci.high=results.xgb.ic50.set2$r2.validation.ci[2]))

# ic80
results.out <- rbind (results.out, data.frame (learner="xbgoost",
                                               endpoint="ic80",
                                               data.set="set1",
                                               analysis.stage="cv",
                                               metric="r2",
                                               metric.results=results.xgb.ic80.set1$r2.cv.mean,
                                               metric.ci.low=results.xgb.ic80.set1$r2.cv.ci[1],
                                               metric.ci.high=results.xgb.ic80.set1$r2.cv.ci[2]))
results.out <- rbind (results.out, data.frame (learner="xbgoost",
                                               endpoint="ic80",
                                               data.set="set2",
                                               analysis.stage="cv",
                                               metric="r2",
                                               metric.results=results.xgb.ic80.set2$r2.cv.mean,
                                               metric.ci.low=results.xgb.ic80.set2$r2.cv.ci[1],
                                               metric.ci.high=results.xgb.ic80.set2$r2.cv.ci[2]))
results.out <- rbind (results.out, data.frame (learner="xbgoost",
                                               endpoint="ic80",
                                               data.set="set1",
                                               analysis.stage="training",
                                               metric="r2",
                                               metric.results=results.xgb.ic80.set1$r2.training,
                                               metric.ci.low=results.xgb.ic80.set1$r2.training.ci[1],
                                               metric.ci.high=results.xgb.ic80.set1$r2.training.ci[2]))
results.out <- rbind (results.out, data.frame (learner="xbgoost",
                                               endpoint="ic80",
                                               data.set="set2",
                                               analysis.stage="training",
                                               metric="r2",
                                               metric.results=results.xgb.ic80.set2$r2.training,
                                               metric.ci.low=results.xgb.ic80.set2$r2.training.ci[1],
                                               metric.ci.high=results.xgb.ic80.set2$r2.training.ci[2]))
results.out <- rbind (results.out, data.frame (learner="xbgoost",
                                               endpoint="ic80",
                                               data.set="set1",
                                               analysis.stage="validation",
                                               metric="r2",
                                               metric.results=results.xgb.ic80.set1$r2.validation,
                                               metric.ci.low=results.xgb.ic80.set1$r2.validation.ci[1],
                                               metric.ci.high=results.xgb.ic80.set1$r2.validation.ci[2]))
results.out <- rbind (results.out, data.frame (learner="xbgoost",
                                               endpoint="ic80",
                                               data.set="set2",
                                               analysis.stage="validation",
                                               metric="r2",
                                               metric.results=results.xgb.ic80.set2$r2.validation,
                                               metric.ci.low=results.xgb.ic80.set2$r2.validation.ci[1],
                                               metric.ci.high=results.xgb.ic80.set2$r2.validation.ci[2]))

# neutralization slope
results.out <- rbind (results.out, data.frame (learner="xbgoost",
                                               endpoint="slope",
                                               data.set="set1",
                                               analysis.stage="cv",
                                               metric="r2",
                                               metric.results=results.xgb.slope.set1$r2.cv.mean,
                                               metric.ci.low=results.xgb.slope.set1$r2.cv.ci[1],
                                               metric.ci.high=results.xgb.slope.set1$r2.cv.ci[2]))
results.out <- rbind (results.out, data.frame (learner="xbgoost",
                                               endpoint="slope",
                                               data.set="set2",
                                               analysis.stage="cv",
                                               metric="r2",
                                               metric.results=results.xgb.slope.set2$r2.cv.mean,
                                               metric.ci.low=results.xgb.slope.set2$r2.cv.ci[1],
                                               metric.ci.high=results.xgb.slope.set2$r2.cv.ci[2]))
results.out <- rbind (results.out, data.frame (learner="xbgoost",
                                               endpoint="slope",
                                               data.set="set1",
                                               analysis.stage="training",
                                               metric="r2",
                                               metric.results=results.xgb.slope.set1$r2.training,
                                               metric.ci.low=results.xgb.slope.set1$r2.training.ci[1],
                                               metric.ci.high=results.xgb.slope.set1$r2.training.ci[2]))
results.out <- rbind (results.out, data.frame (learner="xbgoost",
                                               endpoint="slope",
                                               data.set="set2",
                                               analysis.stage="training",
                                               metric="r2",
                                               metric.results=results.xgb.slope.set2$r2.training,
                                               metric.ci.low=results.xgb.slope.set2$r2.training.ci[1],
                                               metric.ci.high=results.xgb.slope.set2$r2.training.ci[2]))
results.out <- rbind (results.out, data.frame (learner="xbgoost",
                                               endpoint="slope",
                                               data.set="set1",
                                               analysis.stage="validation",
                                               metric="r2",
                                               metric.results=results.xgb.slope.set1$r2.validation,
                                               metric.ci.low=results.xgb.slope.set1$r2.validation.ci[1],
                                               metric.ci.high=results.xgb.slope.set1$r2.validation.ci[2]))
results.out <- rbind (results.out, data.frame (learner="xbgoost",
                                               endpoint="slope",
                                               data.set="set2",
                                               analysis.stage="validation",
                                               metric="r2",
                                               metric.results=results.xgb.slope.set2$r2.validation,
                                               metric.ci.low=results.xgb.slope.set2$r2.validation.ci[1],
                                               metric.ci.high=results.xgb.slope.set2$r2.validation.ci[2]))


# ---------------------------------------------------------------------------- #
# STEP 5:  save results
# ---------------------------------------------------------------------------- #


setwd (path.data)
write.csv (results.out, file="mccv_results_v4.csv", row.names=F)


# ---------------------------------------------------------------------------- #
#                                    - 30 -
# ---------------------------------------------------------------------------- #



