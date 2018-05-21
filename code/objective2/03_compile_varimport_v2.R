

# ---------------------------------------------------------------------------- # 
# set up
# ---------------------------------------------------------------------------- # 


# refresh our workspace
rm (list=ls ())

# identify our filesystem locations; you will need to set "path.home" to the
# path of the repository on your local filesystem
path.home <- "/repository/home/path"
path.lib <- paste0 (path.home, "/code")
path.data <- paste0 (path.home, "/data")
path.rdata <- paste0 (path.home, "/data")

# load our results
setwd (path.data)
load ("analysis_final_05_xgb.Rdata")


# ---------------------------------------------------------------------------- # 
# define our functions
# ---------------------------------------------------------------------------- # 


# subset our features of interest and merging them with our variable importance 
# scores
get.results <- function (results) {
  return (data.frame (feature.name=results$featnames, importance=results$varimport))
}

# rescale rank data to a range of 0 through 100
rescale <- function (data) {
  data.tmp <- data - min (data)
  data.tmp <- data.tmp / max (data.tmp)
  return (data.tmp * 100)
}

# geometric mean
geometric.mean <- function (x) {
  return (exp (mean (log (x))))
}


# ---------------------------------------------------------------------------- # 
# compile results for LASSO
# ---------------------------------------------------------------------------- # 


results <- results.lasso.ic50.censored.set1
importance.all <- get.results (results)
names (importance.all) <- c ("feature", "lasso.ic50.censored.set1")
results.all <- results$auc.validation

results <- results.lasso.ic50.censored.set2
results.tmp <- get.results (results)
importance.all <- merge (importance.all, results.tmp, by=1, all=T)
names (importance.all)[ncol (importance.all)] <- "lasso.ic50.censored.set2"
results.all <- append (results.all, results$auc.validation)

results <- results.lasso.binding.sens.resis.set1
results.tmp <- get.results (results)
importance.all <- merge (importance.all, results.tmp, by=1, all=T)
names (importance.all)[ncol (importance.all)] <- "lasso.binding.sens.resis.set1"
results.all <- append (results.all, results$auc.validation)

results <- results.lasso.binding.sens.resis.set2
results.tmp <- get.results (results)
importance.all <- merge (importance.all, results.tmp, by=1, all=T)
names (importance.all)[ncol (importance.all)] <- "lasso.binding.sens.resis.set2"
results.all <- append (results.all, results$auc.validation)

results <- results.lasso.ic50.set1
results.tmp <- get.results (results)
importance.all <- merge (importance.all, results.tmp, by=1, all=T)
names (importance.all)[ncol (importance.all)] <- "lasso.ic50.set1"
results.all <- append (results.all, results$r2.validation)

results <- results.lasso.ic50.set2
results.tmp <- get.results (results)
importance.all <- merge (importance.all, results.tmp, by=1, all=T)
names (importance.all)[ncol (importance.all)] <- "lasso.ic50.set2"
results.all <- append (results.all, results$r2.validation)

results <- results.lasso.ic80.set1
results.tmp <- get.results (results)
importance.all <- merge (importance.all, results.tmp, by=1, all=T)
names (importance.all)[ncol (importance.all)] <- "lasso.ic80.set1"
results.all <- append (results.all, results$r2.validation)

results <- results.lasso.ic80.set2
results.tmp <- get.results (results)
importance.all <- merge (importance.all, results.tmp, by=1, all=T)
names (importance.all)[ncol (importance.all)] <- "lasso.ic80.set2"
results.all <- append (results.all, results$r2.validation)

results <- results.lasso.slope.set1
results.tmp <- get.results (results)
importance.all <- merge (importance.all, results.tmp, by=1, all=T)
names (importance.all)[ncol (importance.all)] <- "lasso.slope.set1"
results.all <- append (results.all, results$r2.validation)

results <- results.lasso.slope.set2
results.tmp <- get.results (results)
importance.all <- merge (importance.all, results.tmp, by=1, all=T)
names (importance.all)[ncol (importance.all)] <- "lasso.slope.set2"
results.all <- append (results.all, results$r2.validation)


# ---------------------------------------------------------------------------- # 
# compile results for Naive Bayes
# ---------------------------------------------------------------------------- # 


results <- results.nb.ic50.censored.set1
results.tmp <- get.results (results)
importance.all <- merge (importance.all, results.tmp, by=1, all=T)
names (importance.all)[ncol (importance.all)] <- "nb.ic50.censored.set1"
results.all <- append (results.all, results$auc.validation)

results <- results.nb.ic50.censored.set2
results.tmp <- get.results (results)
importance.all <- merge (importance.all, results.tmp, by=1, all=T)
names (importance.all)[ncol (importance.all)] <- "nb.ic50.censored.set2"
results.all <- append (results.all, results$auc.validation)

results <- results.nb.binding.sens.resis.set1
results.tmp <- get.results (results)
importance.all <- merge (importance.all, results.tmp, by=1, all=T)
names (importance.all)[ncol (importance.all)] <- "nb.binding.sens.resis.set1"
results.all <- append (results.all, results$auc.validation)

results <- results.nb.binding.sens.resis.set2
results.tmp <- get.results (results)
importance.all <- merge (importance.all, results.tmp, by=1, all=T)
names (importance.all)[ncol (importance.all)] <- "nb.binding.sens.resis.set2"
results.all <- append (results.all, results$auc.validation)


# ---------------------------------------------------------------------------- # 
# compile results for random forest
# ---------------------------------------------------------------------------- # 


results <- results.rf.ic50.censored.set1
results.tmp <- get.results (results)
importance.all <- merge (importance.all, results.tmp, by=1, all=T)
names (importance.all)[ncol (importance.all)] <- "rf.ic50.censored.set1"
results.all <- append (results.all, results$auc.validation)

results <- results.rf.ic50.censored.set2
results.tmp <- get.results (results)
importance.all <- merge (importance.all, results.tmp, by=1, all=T)
names (importance.all)[ncol (importance.all)] <- "rf.ic50.censored.set2"
results.all <- append (results.all, results$auc.validation)

results <- results.rf.binding.sens.resis.set1
results.tmp <- get.results (results)
importance.all <- merge (importance.all, results.tmp, by=1, all=T)
names (importance.all)[ncol (importance.all)] <- "rf.binding.sens.resis.set1"
results.all <- append (results.all, results$auc.validation)

results <- results.rf.binding.sens.resis.set2
results.tmp <- get.results (results)
importance.all <- merge (importance.all, results.tmp, by=1, all=T)
names (importance.all)[ncol (importance.all)] <- "rf.binding.sens.resis.set2"
results.all <- append (results.all, results$auc.validation)

results <- results.rf.ic50.set1
results.tmp <- get.results (results)
importance.all <- merge (importance.all, results.tmp, by=1, all=T)
names (importance.all)[ncol (importance.all)] <- "rf.ic50.set1"
results.all <- append (results.all, results$r2.validation)

results <- results.rf.ic50.set2
results.tmp <- get.results (results)
importance.all <- merge (importance.all, results.tmp, by=1, all=T)
names (importance.all)[ncol (importance.all)] <- "rf.ic50.set2"
results.all <- append (results.all, results$r2.validation)

results <- results.rf.ic80.set1
results.tmp <- get.results (results)
importance.all <- merge (importance.all, results.tmp, by=1, all=T)
names (importance.all)[ncol (importance.all)] <- "rf.ic80.set1"
results.all <- append (results.all, results$r2.validation)

results <- results.rf.ic80.set2
results.tmp <- get.results (results)
importance.all <- merge (importance.all, results.tmp, by=1, all=T)
names (importance.all)[ncol (importance.all)] <- "rf.ic80.set2"
results.all <- append (results.all, results$r2.validation)

results <- results.rf.slope.noinf.set1
results.tmp <- get.results (results)
importance.all <- merge (importance.all, results.tmp, by=1, all=T)
names (importance.all)[ncol (importance.all)] <- "rf.slope.set1"
results.all <- append (results.all, results$r2.validation)

results <- results.rf.slope.noinf.set2
results.tmp <- get.results (results)
importance.all <- merge (importance.all, results.tmp, by=1, all=T)
names (importance.all)[ncol (importance.all)] <- "rf.slope.set2"
results.all <- append (results.all, results$r2.validation)


# ---------------------------------------------------------------------------- # 
# compile results for XGBoost
# ---------------------------------------------------------------------------- # 


results <- results.xgb.ic50.censored.set1
results.tmp <- get.results (results)
importance.all <- merge (importance.all, results.tmp, by=1, all=T)
names (importance.all)[ncol (importance.all)] <- "xgb.ic50.censored.set1"
results.all <- append (results.all, results$auc.validation)

results <- results.xgb.ic50.censored.set2
results.tmp <- get.results (results)
importance.all <- merge (importance.all, results.tmp, by=1, all=T)
names (importance.all)[ncol (importance.all)] <- "xgb.ic50.censored.set2"
results.all <- append (results.all, results$auc.validation)

results <- results.xgb.binding.sens.resis.set1
results.tmp <- get.results (results)
importance.all <- merge (importance.all, results.tmp, by=1, all=T)
names (importance.all)[ncol (importance.all)] <- "xgb.binding.sens.resis.set1"
results.all <- append (results.all, results$auc.validation)

results <- results.xgb.binding.sens.resis.set2
results.tmp <- get.results (results)
importance.all <- merge (importance.all, results.tmp, by=1, all=T)
names (importance.all)[ncol (importance.all)] <- "xgb.binding.sens.resis.set2"
results.all <- append (results.all, results$auc.validation)

results <- results.xgb.ic50.set1
results.tmp <- get.results (results)
importance.all <- merge (importance.all, results.tmp, by=1, all=T)
names (importance.all)[ncol (importance.all)] <- "xgb.ic50.set1"
results.all <- append (results.all, results$r2.validation)

results <- results.xgb.ic50.set2
results.tmp <- get.results (results)
importance.all <- merge (importance.all, results.tmp, by=1, all=T)
names (importance.all)[ncol (importance.all)] <- "xgb.ic50.set2"
results.all <- append (results.all, results$r2.validation)

results <- results.xgb.ic80.set1
results.tmp <- get.results (results)
importance.all <- merge (importance.all, results.tmp, by=1, all=T)
names (importance.all)[ncol (importance.all)] <- "xgb.ic80.set1"
results.all <- append (results.all, results$r2.validation)

results <- results.xgb.ic80.set2
results.tmp <- get.results (results)
importance.all <- merge (importance.all, results.tmp, by=1, all=T)
names (importance.all)[ncol (importance.all)] <- "xgb.ic80.set2"
results.all <- append (results.all, results$r2.validation)

results <- results.xgb.slope.set1
results.tmp <- get.results (results)
importance.all <- merge (importance.all, results.tmp, by=1, all=T)
names (importance.all)[ncol (importance.all)] <- "xgb.slope.set1"
results.all <- append (results.all, results$r2.validation)

results <- results.xgb.slope.set2
results.tmp <- get.results (results)
importance.all <- merge (importance.all, results.tmp, by=1, all=T)
names (importance.all)[ncol (importance.all)] <- "xgb.slope.set2"
results.all <- append (results.all, results$r2.validation)


# ---------------------------------------------------------------------------- # 
# order up!
# ---------------------------------------------------------------------------- # 


# polish up our vector of prediction results
results.all.mat <- matrix (results.all, ncol=1)
row.names (results.all.mat) <- names (importance.all)[2:35]

# initialize our scoreboard
scoreboard <- data.frame (feature=importance.all$feature)

# which steps are classifiers (as opposed to regression methods)?
steps <- list ()
steps[['ic50.censored']] <- c (2:3, 12:13, 16:17, 26:27)
steps[['sens.resis']] <- c (4:5, 14:15, 18:19, 28:29)
steps[['ic50']] <- c (6:7, 20:21, 30:31)
steps[['ic80']] <- c (8:9, 22:23, 32:33)
steps[['slope']] <- c (10:11, 24:25, 34:35)
steps[['dichotomous.endpoints']] <- c (2:3, 12:13, 16:17, 26:27, 4:5, 14:15, 18:19, 28:29)
steps[['continuous.ic.endpoints']] <- c (6:7, 20:21, 30:31, 8:9, 22:23, 32:33)

# pick one for analytic fun!
steps.run <- 'ic50.censored'
steps.run <- 'sens.resis'
steps.run <- 'ic50'
steps.run <- 'ic80'
steps.run <- 'slope'
steps.run <- 'dichotomous.endpoints'
steps.run <- 'continuous.ic.endpoints'


steps.analysis <- steps[[steps.run]]
scoreboard.tmp <- data.frame (features=scoreboard[, 1])

# create an ordering system for our features
for (step in 1:((length (steps.analysis)) / 2)) {


  # define the results we are looking at for this "step"; these should be the
  # results of sets 1 and 2 for the same method and endpoint
  feature.cols <- c (steps.analysis[(step * 2) - 1], ((steps.analysis[(step * 2) - 1]) + 1))
  importance.tmp <- importance.all[, feature.cols]

  # identify our VIMs and rescale them from 0-100
  score.weighted <- data.frame (set.1=importance.tmp[, 1],
                                set.2=importance.tmp[, 2])
  score.weighted[, 1] <- rescale (importance.tmp[, 1])
  score.weighted[, 2] <- rescale (importance.tmp[, 2])

  # determine our mean rescaled VIM for the two sets
  mean.score.weighted <- apply (score.weighted, 1, geometric.mean)

  # create some new column names and add this information to the scoreboard
  scoreboard.tmp[, names (importance.tmp[1])] <- mean.score.weighted
}

scoreboard[, steps.run] <- rowMeans (scoreboard.tmp[, -1])

# write out our table of results
setwd (path.data)
write.csv (scoreboard, file="feature_selection_results_overall_v2.csv", row.names=F)

# perform some QC
plot (scoreboard$score.overall, type="l", ylab="Score", 
      main="Distribution of Global Importance Scores")

model <- lm (y ~ x, data.frame (x=100:526, y=scoreboard$score.overall[100:526]))

abline (a=coef (model)[1], b=coef (model)[2], col="red")
abline (h=coef (model)[1], lty=3, col="gray")


# ---------------------------------------------------------------------------- # 
#                                    - 30 -
# ---------------------------------------------------------------------------- # 





