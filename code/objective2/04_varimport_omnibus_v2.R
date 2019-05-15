

# ---------------------------------------------------------------------------- #
# STEP 0:  get everything ready to go
# ---------------------------------------------------------------------------- #


# refresh our workspace
rm (list=ls ())

# load our required packages
library (Exact)

# identify our filesystem locations; you will need to set "path.home" to the
# path of the repository on your local filesystem
path.home <- "/repository/home/path"
path.data <- paste0 (path.home, "/data")
path.rdata <- paste0 (path.home, "/data")

# load prediction-based VIM results
setwd (path.data)
results.me <- read.csv ("feature_selection_results_overall_v2.csv", header=T)
results.bw.ic50.censored <- read.csv ("vimp_table_by_outcome_cens.csv", header=T)
results.bw.sens.resis <- read.csv ("vimp_table_by_outcome_sens.resis.csv", header=T)
results.bw.ic50 <- read.csv ("vimp_table_by_outcome_ic50.csv", header=T)
results.bw.ic80 <- read.csv ("vimp_table_by_outcome_ic80.csv", header=T)
results.bw.slope <- read.csv ("vimp_table_by_outcome_slope_mod.csv", header=T)

# load data for calculating our own univariate VIM results
setwd (path.data)
data.analysis.all <- read.csv ("fulldata.csv"), header=T)

# separate our analysis data into groups for analysis:  outcomes
data.outcomes.binary <- data.analysis.all[, c (35, 42)]
data.outcomes.continuous <- data.analysis.all[, c (37, 40:41)]

# separate our analysis data into groups for analysis:  features
data.features.geog <- data.analysis.all[, 20:23]
data.features.binary <- data.analysis.all[, c (7:17, 20:23, 25:26, 43:533, 548, 550)]
data.features.continuous.normal <- data.analysis.all[, c (534:535, 539, 541, 543, 545:546, 549, 551:553)]
data.features.continuous.abnormal <- data.analysis.all[, c (28, 30, 536:538, 540, 542, 544, 547)]
data.features.categorical <- data.analysis.all[, c (6, 18, 19, 24)]
data.features.not.categorical <- cbind (data.features.binary, data.features.continuous.normal, data.features.continuous.abnormal)
data.features.lr <- cbind (geographic.region.of.origin=data.analysis.all$geographic.region.of.origin, 
                           data.features.not.categorical[, -c (12:15)])


# ---------------------------------------------------------------------------- #
# FXN DEPOT:  define the functions for our different analyses
# ---------------------------------------------------------------------------- #


# perform Barnard's test on 2x2 contingency table data
crunch.barnard <- function (data.x, data.y) {
  results <- data.frame (feature=names (data.x), p.val=rep (NA, ncol (data.x)))
  for (col in 1:ncol (data.x)) {
    if (!(0 %in% rowSums (table (data.x[, col], data.y))) & !(0 %in% colSums (table (data.x[, col], data.y)))) {
      results[col, "p.val"] <- exact.test (table (data.x[, col], data.y), to.plot=F)$p.value
    } else {
      results[col, "p.val"] <- NA
    }
  }
  return (results)
}

# perform t-test on two-sample data (normally distributed)
crunch.t <- function (data.x, data.y, reverse=F) {
  results <- data.frame (feature=names (data.x), p.val=rep (NA, ncol (data.x)))
  for (col in 1:ncol (data.x)) {
    data.analysis.x <- NULL
    data.analysis.y <- NULL
    if (!reverse) {
      data.analysis.x <- data.x[data.y == 0, col]
      data.analysis.y <- data.x[data.y == 1, col]
    } else {
      data.analysis.x <- data.y[data.x[, col] == 0]
      data.analysis.y <- data.y[data.x[, col] == 1]
    }
    if (length (data.analysis.x) - sum (is.na (data.analysis.x)) >= 2 & length (data.analysis.y) - sum (is.na (data.analysis.y)) >= 2) {
      results[col, "p.val"] <- t.test (data.analysis.x, data.analysis.y)$p.value
    } else {
      results[col, "p.val"] <- NA
    }
  }
  return (results)
}

# perform Wilcoxon test on two-sample data (not normally distributed)
crunch.wilcox <- function (data.x, data.y) {
  results <- data.frame (feature=names (data.x), p.val=rep (NA, ncol (data.x)))
  for (col in 1:ncol (data.x)) {
    data.analysis.x <- NULL
    data.analysis.y <- NULL
    data.analysis.x <- data.x[data.y == 0, col]
    data.analysis.y <- data.x[data.y == 1, col]
    results[col, "p.val"] <- wilcox.test (data.analysis.x, data.analysis.y)$p.value
  }
  return (results)
}

# perform Chi-squared test on contingency tables larger than 2x2
crunch.chisq <- function (data.x, data.y) {
  results <- data.frame (feature=names (data.x), p.val=rep (NA, ncol (data.x)))
  for (col in 1:ncol (data.x)) {
    results[col, "p.val"] <- chisq.test (table (data.x[, col], data.y))$p.value
  }
  return (results)
}

# perform correlation test on continuous data
crunch.cor <- function (data.x, data.y, method) {
  results <- data.frame (feature=names (data.x), p.val=rep (NA, ncol (data.x)))
  for (col in 1:ncol (data.x)) {
    results[col, "p.val"] <- cor.test (data.x[, col], data.y, method=method)$p.value
  }
  return (results)
}

# perform ANOVA test on categorical/grouped data
crunch.anova <- function (data.x, data.y, method) {
  results <- data.frame (feature=names (data.x), p.val=rep (NA, ncol (data.x)))
  for (col in 1:ncol (data.x)) {
    results[col, "p.val"] <- unlist (anova (lm (data.y ~ data.x[, col]))["Pr(>F)"])[1]
  }
  return (results)
}

# perform linear regression-based analyses
crunch.lm <- function (data.x, data.y) {
  data.x.geog <- data.x[, 1]
  data.x.no.geog <- data.x[, -1]
  results <- data.frame (feature=names (data.x.no.geog), 
                         coef=rep (NA, ncol (data.x.no.geog)), 
                         t.val=rep (NA, ncol (data.x.no.geog)),
                         p.val=rep (NA, ncol (data.x.no.geog)))
  for (col in 1:ncol (data.x.no.geog)) {
    data.tmp <- data.frame (x=data.x.no.geog[, col], data.x.geog, y=data.y)
    if ("x" %in% rownames (summary (lm (y ~ ., data=data.tmp))$coefficients)) {
      results[col, "coef"] <- summary (lm (y ~ ., data=data.tmp))$coefficients["x", "Estimate"]
      results[col, "t.val"] <- summary (lm (y ~ ., data=data.tmp))$coefficients["x", "t value"]
      results[col, "p.val"] <- summary (lm (y ~ ., data=data.tmp))$coefficients["x", "Pr(>|t|)"]
    } else {
      results[col, "coef"] <- NA
      results[col, "t.val"] <- NA
      results[col, "p.val"] <- NA
    }
  }
  return (results)
}

# perform logistic regression-based analyses
crunch.glm <- function (data.x, data.y) {
  data.x.geog <- data.x[, 1]
  data.x.no.geog <- data.x[, -1]
  results <- data.frame (feature=names (data.x.no.geog), 
                         coef=rep (NA, ncol (data.x.no.geog)), 
                         t.val=rep (NA, ncol (data.x.no.geog)),
                         p.val=rep (NA, ncol (data.x.no.geog)))
  for (col in 1:ncol (data.x.no.geog)) {
    data.tmp <- data.frame (x=data.x.no.geog[, col], data.x.geog, y=data.y)
    if ("x" %in% rownames (summary (glm (y ~ ., data=data.tmp))$coefficients)) {
      results[col, "coef"] <- summary (glm (y ~ ., data=data.tmp))$coefficients["x", "Estimate"]
      results[col, "t.val"] <- summary (glm (y ~ ., data=data.tmp))$coefficients["x", "t value"]
      results[col, "p.val"] <- summary (glm (y ~ ., data=data.tmp))$coefficients["x", "Pr(>|t|)"]
    } else {
      results[col, "coef"] <- NA
      results[col, "t.val"] <- NA
      results[col, "p.val"] <- NA
    }
  }
  return (results)
}


# ---------------------------------------------------------------------------- #
# STEP 1:  perform our own inferential analyses
# ---------------------------------------------------------------------------- #


# IC50 censored
results.ic50.censored.binary <- crunch.barnard (data.features.binary, data.outcomes.binary[, 1])
results.ic50.censored.continuous.normal <- crunch.t (data.features.continuous.normal, data.outcomes.binary[, 1])
results.ic50.censored.continuous.abnormal <- crunch.wilcox (data.features.continuous.abnormal, data.outcomes.binary[, 1])
results.ic50.censored.categorical <- crunch.chisq (data.features.categorical, data.outcomes.binary[, 1])
results.ic50.censored.all <- rbind (results.ic50.censored.binary, results.ic50.censored.continuous.normal, results.ic50.censored.continuous.abnormal, results.ic50.censored.categorical)
results.ic50.censored.all$q.val <- p.adjust (results.ic50.censored.all$p.val, method="BH")
results.ic50.censored.all$fwer.p.val <- p.adjust (results.ic50.censored.all$p.val, method="holm")

# sensitive/resistant
results.sens.resis.binary <- crunch.barnard (data.features.binary, data.outcomes.binary[, 2])
results.sens.resis.continuous.normal <- crunch.t (data.features.continuous.normal, data.outcomes.binary[, 2])
results.sens.resis.continuous.abnormal <- crunch.wilcox (data.features.continuous.abnormal, data.outcomes.binary[, 2])
results.sens.resis.categorical <- crunch.chisq (data.features.categorical, data.outcomes.binary[, 2])
results.sens.resis.all <- rbind (results.sens.resis.binary, results.sens.resis.continuous.normal, results.sens.resis.continuous.abnormal, results.sens.resis.categorical)
results.sens.resis.all$q.val <- p.adjust (results.sens.resis.all$p.val, method="BH")
results.sens.resis.all$fwer.p.val <- p.adjust (results.sens.resis.all$p.val, method="holm")

# IC50 (quantitative)
results.ic50.binary <- crunch.t (data.features.binary, data.outcomes.continuous[, 1], reverse=T)
results.ic50.continuous.normal <- crunch.cor (data.features.continuous.normal, data.outcomes.continuous[, 1], method="pearson")
results.ic50.continuous.abnormal <- crunch.cor (data.features.continuous.abnormal, data.outcomes.continuous[, 1], method="spearman")
results.ic50.categorical <- crunch.anova (data.features.categorical, data.outcomes.continuous[, 1])
results.ic50.all <- rbind (results.ic50.binary, results.ic50.continuous.normal, results.ic50.continuous.abnormal, results.ic50.categorical)
results.ic50.all$q.val <- p.adjust (results.ic50.all$p.val, method="BH")
results.ic50.all$fwer.p.val <- p.adjust (results.ic50.all$p.val, method="holm")

# IC80 (quantitative)
results.ic80.binary <- crunch.t (data.features.binary, data.outcomes.continuous[, 2], reverse=T)
results.ic80.continuous.normal <- crunch.cor (data.features.continuous.normal, data.outcomes.continuous[, 2], method="pearson")
results.ic80.continuous.abnormal <- crunch.cor (data.features.continuous.abnormal, data.outcomes.continuous[, 2], method="spearman")
results.ic80.categorical <- crunch.anova (data.features.categorical, data.outcomes.continuous[, 2])
results.ic80.all <- rbind (results.ic80.binary, results.ic80.continuous.normal, results.ic80.continuous.abnormal, results.ic80.categorical)
results.ic80.all$q.val <- p.adjust (results.ic80.all$p.val, method="BH")
results.ic80.all$fwer.p.val <- p.adjust (results.ic80.all$p.val, method="holm")

# neutralization slope
results.slope.binary <- crunch.t (data.features.binary, data.outcomes.continuous[, 3], reverse=T)
results.slope.continuous.normal <- crunch.cor (data.features.continuous.normal, data.outcomes.continuous[, 3], method="pearson")
results.slope.continuous.abnormal <- crunch.cor (data.features.continuous.abnormal, data.outcomes.continuous[, 3], method="spearman")
results.slope.categorical <- crunch.anova (data.features.categorical, data.outcomes.continuous[, 3])
results.slope.all <- rbind (results.slope.binary, results.slope.continuous.normal, results.slope.continuous.abnormal, results.slope.categorical)
results.slope.all$q.val <- p.adjust (results.slope.all$p.val, method="BH")
results.slope.all$fwer.p.val <- p.adjust (results.slope.all$p.val, method="holm")

# let's pause and take a moment to save
setwd (path.rdata)
save.image ("varimport_omnibus_step1.Rdata")
#load ("varimport_omnibus_step1.Rdata")


# ---------------------------------------------------------------------------- #
# STEP 2:  merge all of these results together
# ---------------------------------------------------------------------------- #


# first, create data frame of all prediction-based VIM results
results.merged <- merge (results.me, results.bw.ic50.censored[, 1:2], by=1, all.x=T)
results.merged <- merge (results.merged, results.bw.sens.resis[, 1:2], by=1, all.x=T)
results.merged <- merge (results.merged, results.bw.ic50[, 1:2], by=1, all.x=T)
results.merged <- merge (results.merged, results.bw.ic80[, 1:2], by=1, all.x=T)
results.merged <- merge (results.merged, results.bw.slope[, 1:2], by=1, all.x=T)
names (results.merged) <- c ("feature", "mccv.ic50.censored.vim", "mccv.sens.resis.vim", 
                             "mccv.ic50.vim", "mccv.ic80.vim", "mccv.slope.vim", 
                             "mccv.dichotomous.endpoints.vim", 
                             "mccv.continuous.ic.endpoints.vim", "sl.ic50.censored.vim",
                             "sl.sens.resis.vim", "sl.ic50.vim", "sl.ic80.vim", "sl.slope.vim")

# we need to add information about the rank-order of our VIMs
results.rank <- NULL
for (col in 2:13) {
  results.rank <- cbind (results.rank, rank (-results.merged[, col]))
}
results.rank <- as.data.frame (results.rank)
names (results.rank) <- paste0 (names (results.merged[2:13]), ".rank")
results.merged <- cbind (results.merged, results.rank)

# now add our significance results
results.merged <- merge (results.merged, results.ic50.censored.all, by=1, all=T)
results.merged <- merge (results.merged, results.sens.resis.all, by=1, all=T)
results.merged <- merge (results.merged, results.ic50.all, by=1, all=T)
results.merged <- merge (results.merged, results.ic80.all, by=1, all=T)
results.merged <- merge (results.merged, results.slope.all, by=1, all=T)
names (results.merged)[26:40] <- c ("test.ic50.censored.pval", "test.ic50.censored.qval", "test.ic50.censored.fwer.pval",
                                    "test.sens.resis.pval", "test.sens.resis.qval", "test.sens.resis.fwer.pval",
                                    "test.ic50.pval", "test.ic50.qval", "test.ic50.fwer.pval",
                                    "test.ic80.pval", "test.ic80.qval", "test.ic80.fwer.pval",
                                    "test.slope.pval", "test.slope.qval", "test.slope.fwer.pval")

# let's pause and take a moment to save
setwd (path.rdata)
save.image ("varimport_omnibus_step2.Rdata")
#load ("varimport_omnibus_step2.Rdata")


# ---------------------------------------------------------------------------- #
# STEP 3:  do our follow-up regression-based analyses to control for geography
# ---------------------------------------------------------------------------- #


# do the analyses for each outcome
results.ic50.censored.glm <- crunch.glm (data.features.lr, data.outcomes.binary[, 1])
results.sens.resis.glm <- crunch.glm (data.features.lr, data.outcomes.binary[, 2])
results.ic50.lm <- crunch.lm (data.features.lr, data.outcomes.continuous[, 1])
results.ic80.lm <- crunch.lm (data.features.lr, data.outcomes.continuous[, 2])
results.slope.lm <- crunch.lm (data.features.lr, data.outcomes.continuous[, 3])

# now do our multiplicity adjustment
results.ic50.censored.glm$q.val <- p.adjust (results.ic50.censored.glm$p.val, method="BH")
results.ic50.censored.glm$fwer.p.val <- p.adjust (results.ic50.censored.glm$p.val, method="holm")
results.sens.resis.glm$q.val <- p.adjust (results.sens.resis.glm$p.val, method="BH")
results.sens.resis.glm$fwer.p.val <- p.adjust (results.sens.resis.glm$p.val, method="holm")
results.ic50.lm$q.val <- p.adjust (results.ic50.lm$p.val, method="BH")
results.ic50.lm$fwer.p.val <- p.adjust (results.ic50.lm$p.val, method="holm")
results.ic80.lm$q.val <- p.adjust (results.ic80.lm$p.val, method="BH")
results.ic80.lm$fwer.p.val <- p.adjust (results.ic80.lm$p.val, method="holm")
results.slope.lm$q.val <- p.adjust (results.slope.lm$p.val, method="BH")
results.slope.lm$fwer.p.val <- p.adjust (results.slope.lm$p.val, method="holm")

# merge with our other results
results.merged <- merge (results.merged, results.ic50.censored.glm, by=1, all=T)
results.merged <- merge (results.merged, results.sens.resis.glm, by=1, all=T)
results.merged <- merge (results.merged, results.ic50.lm, by=1, all=T)
results.merged <- merge (results.merged, results.ic80.lm, by=1, all=T)
results.merged <- merge (results.merged, results.slope.lm, by=1, all=T)
names (results.merged)[41:65] <- c ("lm.ic50.censored.coef", "lm.ic50.censored.tval", "lm.ic50.censored.pval", "lm.ic50.censored.qval", "lm.ic50.censored.fwer.pval",
                                    "lm.sens.resis.coef", "lm.sens.resis.tval", "lm.sens.resis.pval", "lm.sens.resis.qval", "lm.sens.resis.fwer.pval",
                                    "lm.ic50.coef", "lm.ic50.tval", "lm.ic50.pval", "lm.ic50.qval", "lm.ic50.fwer.pval",
                                    "lm.ic80.coef", "lm.ic80.tval", "lm.ic80.pval", "lm.ic80.qval", "lm.ic80.fwer.pval",
                                    "lm.slope.coef", "lm.slope.tval", "lm.slope.pval", "lm.slope.qval", "lm.slope.fwer.pval")

# el finito, save our results and bail
setwd (path.data)
write.csv (results.merged, file="varimport_omnibus_v4.csv", row.names=F)
setwd (path.rdata)
save.image ("varimport_omnibus_final.Rdata")
#load ("varimport_omnibus_final.Rdata")


# ---------------------------------------------------------------------------- #
#                                    - 30 -
# ---------------------------------------------------------------------------- #



