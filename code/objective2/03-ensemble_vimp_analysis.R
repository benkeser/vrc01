##------------------------------------------------------------------------
## This file relies on output created from 01-get_fits_from_objective_1.R
## and on output from both 02-second_stage*.R files,
## so make sure to run that code prior to running this
## analysis.
## 
## Due to the intense computational requirements of the aforementioned 
## scripts, we do not call "source" from within this file,
## and instead rely on the user to have run them prior to running this code.
##
## The purpose of this file is to calculate the ensemble-based variable
## importance estimates for all individual features and groups of features.
## Outputs of this file are plots and .csv files displaying the results.
##------------------------------------------------------------------------

## load required libraries -- package vimp version >= 1.1.4
## note that this can be installed via 
## devtools::install_github("bdwilliamson/vimp@v1.1.4")
## or via install.packages("vimp") 
library("vimp")

## define directory for code
codeDir <- "./code/"

## define directory to get SuperLearner results from
resDir <- "./"

## define directory to save plots to
plotDir <- "./plots/"

## define directory to save tables to
tableDir <- "./tables/"

## naming convention for fitted values, predicted values
predNms <- c("preds_", "_set", ".rds")
reducedIndPredNms <- c("reduced_preds_", "_dataset_", "_ind_", ".rds")
reducedGrpPredNms <- c("reduced_preds_", "_dataset_", "_group_", ".rds")

fitNms <- c("fits_", "_set", ".rds")

## run makeDataAndFunctions R script
## and other user-defined functions
source(paste0(codeDir, "objective1/00-prelims.R"))
source(paste0(codeDir, "objective2/00-ensemble_vim_helpers.R"))

## specify the level for confidence intervals
level <- 0.95

## set up number of individual features, feature groups
num_indiv <- 797
num_grp <- 13

##---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## Load in the predicted values from the full data regressions
## on each outcome and each train/test split
##---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full_mat <- expand.grid(o = c("ic50", "ic80", "slope_mod", "sens.resis", "cens"),
    s = 1:2, stringsAsFactors = FALSE)
full_list <- as.list(paste0(resDir, predNms[1], full_mat$o, predNms[2], full_mat$s, predNms[3]))

## read in the data
tmp <- lapply(full_list, readRDS)
fulls <- lapply(lapply(tmp, function(x) x$pred), rev) # need to reverse the inner order due to cvma fitting; see get_preds.R comments for more detail
folds <- lapply(tmp, function(x) x$folds)

##---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## Load in the reduced regression fitted values
## on each outcome, dataset, feature set/individual feature
##---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## set up the individuals
ind_mat <- expand.grid(o = c("ic50", "ic80", "slope_mod", "sens.resis", "cens"),
    f = 1:num_indiv, s = 1:2, stringsAsFactors = FALSE)
ind_mat_chr <- ind_mat
ind_mat_chr$f <- as.character(ind_mat$f)
ind_mat_chr$s <- as.character(ind_mat$s)
## set up the groups
grp_mat <- expand.grid(o = c("ic50", "ic80", "slope_mod", "sens.resis", "cens"),
    f = 1:num_grp, s = 1:2, stringsAsFactors = FALSE)
grp_mat_chr <- grp_mat
grp_mat_chr$f <- as.character(grp_mat$f)
grp_mat_chr$s <- as.character(grp_mat$s)

## names of the individual and group fitted values
red_ind_list <- as.list(paste0(resDir, reducedIndPredNms[1], ind_mat$o, reducedIndPredNms[2], ind_mat$s, reducedIndPredNms[3], ind_mat$f, reducedIndPredNms[4]))
red_grp_list <- as.list(paste0(resDir, reducedGrpPredNms[1], grp_mat$o, reducedGrpPredNms[2], grp_mat$s, reducedGrpPredNms[3], grp_mat$f, reducedGrpPredNms[4]))

## read them in, give them good names for object creation
red_inds_rev <- lapply(red_ind_list, readRDS)
ind_nms <- apply(ind_mat_chr, 1, function(x) paste0("ind_", paste(x, collapse = "_")))

## same for groups
red_grps_rev <- lapply(red_grp_list, readRDS)
grp_nms <- apply(grp_mat_chr, 1, function(x) paste0("grp_", paste(x, collapse = "_")))

## reverse them all; the 10th-fold-fit is currently in the first position for each list
red_inds <- lapply(red_inds_rev, rev)
red_grps <- lapply(red_grps_rev, rev)

## create lists with the appropriate full fits, same length as both group and individual ones
fulls_ind <- c(rep(fulls[1:length(unique(ind_mat$o))], dim(ind_mat)[1]/length(unique(ind_mat$o))/2),
    rep(fulls[(length(unique(ind_mat$o)) + 1):length(fulls)], dim(ind_mat)[1]/length(unique(ind_mat$o))/2))
fulls_grp <- c(rep(fulls[1:length(unique(grp_mat$o))], dim(grp_mat)[1]/length(unique(grp_mat$o))/2),
    rep(fulls[(length(unique(grp_mat$o)) + 1):length(fulls)], dim(grp_mat)[1]/length(unique(grp_mat$o))/2))

## lists of all of the Y's
ys_1 <- list(Y[!is.na(Y)], Y.80[!is.na(Y.80)], Y.slope[!is.na(Y.slope)], Y.sens.resis[!is.na(Y.sens.resis)], Y.cens[!is.na(Y.cens)])
ys_2 <- list(Y2[!is.na(Y2)], Y2.80[!is.na(Y2.80)], Y2.slope[!is.na(Y2.slope)], Y2.sens.resis[!is.na(Y2.sens.resis)], Y2.cens[!is.na(Y2.cens)])

## lists with copied Y's 
ys_ind <- apply(ind_mat, 1, match_y, y1 = ys_1, y2 = ys_2, folds1 = folds[1:5], folds2 = folds[6:10])
ys_grp <- apply(grp_mat, 1, match_y, y1 = ys_1, y2 = ys_2, folds1 = folds[1:5], folds2 = folds[6:10])

##---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##
## VARIABLE IMPORTANCE ANALYSIS
##
##---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

## Groups
grp_indx <- min(which(grp_mat$s == 2)) - 1
for (i in 1:length(grp_nms)) {
    eval(parse(text = paste0(grp_nms[[i]], 
        " <- cv_vim_nodonsker(Y = ys_grp[[i]]$y, f1 = fulls_grp[[i]], f2 = red_grps[[i]], y = ys_grp[[i]], indx = rep(rep(1:num_grp, each = 5), 2)[i], V = 10, folds = ys_grp[[i]]$folds, type = 'regression', run_regression = FALSE, na.rm = TRUE, alpha = 1 - level)")))
}


## Individual ones
ind_indx <- min(which(ind_mat$s == 2)) - 1
for (i in 1:length(ind_nms)) {
    eval(parse(text = paste0(ind_nms[[i]], 
        " <- cv_vim_nodonsker(Y = ys_ind[[i]]$y, f1 = fulls_ind[[i]], f2 = red_inds[[i]], y = ys_ind[[i]], indx = rep(rep(1:num_ind, each = 5), 2)[i], V = 10, folds = ys_ind[[i]]$folds, type = 'regression', run_regression = FALSE, na.rm = TRUE, alpha = 1 - level)")))
}

##---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## Get the average across the two datasets
## for each feature set and individual feature
## along with SEs and CIs
##---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# want groups first, in alphabetical order (by actual name), followed by individual features in alphabetical order (by actual name)
nms_group <- c("VRC01 contact sites (Set 1)", "CD4 binding sites (Set 2)", "ESA (Set 3)", 
               "Glycosylation sites (Set 4)", "Covarying sites (Set 5)", "PNG sites (Set 6)",
               "N-linked glycosylation (Set 7)", "Subtype (Set 8)", "Sequons (Set 9)",
               "Viral geometry (Set 10)", "Cysteines (Set 11)", "Steric bulk (Set 12)")
nms_ind <- unique(predictors)
## groups
for (i in 1:length(nms_group)) {
  ## extract the feature set
  f_set_nms <- get_feature_list(grp_nms, i, grp_mat)
  ## average the results across datasets
  for (j in 1:length(unique(full_mat$o))) {
    ## logical vector for which to select
    logivec <- unlist(lapply(f_set_nms, function(x) grepl(unique(full_mat$o)[j], x)))
    ## comma separated vector of names
    nmvec <- paste(unlist(f_set_nms[logivec]), collapse = ", ")
    eval(parse(text = paste0("grp_", unique(full_mat$o)[j], "_set_", i, 
                             " <- average_vim(", nmvec, ", weights = c(1/2, 1/2))")))
  }
}
## combine estimates for each outcome
for (i in 1:length(unique(grp_mat_chr$o))) {
  eval(parse(text = paste0("grp_", unique(grp_mat_chr$o)[i], "_avg",
                                  " <- merge_vim(", paste(paste0("grp_", unique(grp_mat_chr$o)[i], "_set_", 1:12), collapse = ", "),
                                  ")")))
  eval(parse(text = paste0("grp_", unique(grp_mat_chr$o)[i], "_avg$s",
                           " <- unlist(lapply(strsplit(grp_", unique(grp_mat_chr$o)[i], "_avg$s, '_', fixed = TRUE), function(x) tail(x, n = 2)[1]))")))
}

## do the same for individual
for (i in 1:length(nms_ind)) {
  ## extract the feature set
  f_set_nms <- get_feature_list(ind_nms, i, ind_mat)
  ## average the results across datasets
  for (j in 1:length(unique(full_mat$o))) {
    ## logical vector for which to select
    logivec <- unlist(lapply(f_set_nms, function(x) grepl(unique(full_mat$o)[j], x)))
    ## comma separated vector of names
    nmvec <- paste(unlist(f_set_nms[logivec]), collapse = ", ")
    eval(parse(text = paste0("ind_", unique(full_mat$o)[j], "_set_", i, 
                             " <- average_vim(", nmvec, ", weights = c(1/2, 1/2))")))
  }
}
## combine estimates for each outcome
for (i in 1:length(unique(ind_mat_chr$o))) {
  eval(parse(text = paste0("ind_", unique(ind_mat_chr$o)[i], "_avg",
                           " <- merge_vim(", paste(paste0("ind_", unique(ind_mat_chr$o)[i], "_set_", 1:493), collapse = ", "),
                           ")")))
  eval(parse(text = paste0("ind_", unique(ind_mat_chr$o)[i], "_avg$s",
                           " <- unlist(lapply(strsplit(ind_", unique(ind_mat_chr$o)[i], "_avg$s, '_', fixed = TRUE), function(x) tail(x, n = 2)[1]))")))
}

##---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## Make the plots
##---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------\

fig_width <- 2590
fig_height <- fig_width
cex_points <- 1.5
cex_labels <- 2.5
cex_main <- 3

## combine all of the estimates, for each outcome
for (j in 1:2) {
  for (i in 1:length(unique(grp_mat_chr$o))) {
    eval(parse(text = paste0("grp_", unique(grp_mat_chr$o)[i], "_", j,
                             " <- merge_vim(", paste(grp_nms[(1:grp_indx + grp_indx*(j-1))][grepl(unique(grp_mat_chr$o)[i], grp_nms[(1:grp_indx + grp_indx*(j-1))])], collapse = ", "),
                             ")")))
  }
}

for (j in 1:2) {
  for (i in 1:length(unique(ind_mat_chr$o))) {
    eval(parse(text = paste0("ind_", unique(ind_mat_chr$o)[i], "_", j,
                             " <- merge_vim(", paste(ind_nms[(1:ind_indx + ind_indx*(j-1))][grepl(unique(ind_mat_chr$o)[i], ind_nms[(1:ind_indx + ind_indx*(j-1))])], collapse = ", "),
                             ")")))
  }
}

## vector of plot-ready names
# nms_pub <- c("ic50", "ic80", "neutralization slope", "ic50 censored and ic50 < 1", "ic50 censored")
nms_pub_datasets <- matrix(c(expression(paste("Quantitative ", IC[50], ", dataset 1", sep = "")), 
                    expression(paste("Quantitative ", IC[50], ", dataset 2", sep = "")),
                    expression(paste("Quantitative ", IC[80], ", dataset 1", sep = "")),
                    expression(paste("Quantitative ", IC[80], ", dataset 2", sep = "")),
                    "Neutralization Slope, dataset 1",
                    "Neutralization Slope, dataset 2",
                    "Sensitive/Resistant Only, dataset 1",
                    "Sensitive/Resistant Only, dataset 2",
                    expression(paste(IC[50], " Censored, dataset 1", sep = "")),
                    expression(paste(IC[50], " Censored, dataset 2", sep = ""))), byrow = TRUE, ncol = 2)
## plot of group importance for each outcome and each dataset
for (j in 1:2) {
  for (i in 1:length(unique(grp_mat_chr$o))) {
    # make a png
    png(paste0(plotDir, "vim_grp_outcome_", unique(grp_mat_chr$o)[i], "_dataset_", j, ".png"), width = fig_width, height = fig_height, units = "px", res = 300)
    # names
    if (unique(grp_mat_chr$o)[i] == "slope_mod") {
      eval(parse(text = paste0("nms_tmp <- unique(grp_mat_chr$f)[as.numeric(unlist(lapply(strsplit(rownames(grp_", unique(grp_mat_chr$o)[i], "_", j , "$mat), '_', fixed = TRUE), function(x) x[4])))]")))
    } else {
      eval(parse(text = paste0("nms_tmp <- unique(grp_mat_chr$f)[as.numeric(unlist(lapply(strsplit(rownames(grp_", unique(grp_mat_chr$o)[i], "_", j , "$mat), '_', fixed = TRUE), function(x) x[3])))]")))  
    }
    nms <- rev(nms_tmp)
    # plot it
    eval(parse(text = paste0("plot(grp_", unique(grp_mat_chr$o)[i], "_", j , ", nms, mar = c(4, 4, 4, 1), cex.axis = cex_labels)")))
    # add labels
    title(main = nms_pub_datasets[i, j], cex = cex_main)
    title(ylab = "Feature Group", cex = cex_labels)
    title(xlab = "Estimated variable importance", cex = cex_labels)
    # close it off
    dev.off()
  }
}

## plot that combines results for same outcome into one figure (makes 5 total figures)
## each plot has results for dataset 1 (red circles) and dataset 2 (blue diamonds),
## along with the average across the two (black squares), with CIs
nms_pub <- c(expression(paste("Quantitative ", IC[50], sep = "")),
             expression(paste("Quantitative ", IC[80], sep = "")),
             "Neutralization Slope",
             "Sensitive/Resistant Only",
             expression(paste(IC[50], " Censored", sep = "")))
for (i in 1:length(unique(grp_mat_chr$o))) {
  # make a png
  png(paste0(plotDir, "vim_grp_combined_outcome_", unique(grp_mat_chr$o)[i], ".png"), width = fig_width, height = fig_height, units = "px", res = 300)
  # names
  if (unique(grp_mat_chr$o)[i] == "slope_mod") {
    eval(parse(text = paste0("nms_tmp <- unique(grp_mat_chr$f)[as.numeric(unlist(lapply(strsplit(rownames(grp_", unique(grp_mat_chr$o)[i], "_avg$mat), '_', fixed = TRUE), function(x) x[5])))]")))
  } else {
    eval(parse(text = paste0("nms_tmp <- unique(grp_mat_chr$f)[as.numeric(unlist(lapply(strsplit(rownames(grp_", unique(grp_mat_chr$o)[i], "_avg$mat), '_', fixed = TRUE), function(x) x[4])))]")))  
  }
  nms <- rev(nms_tmp)
  # plot it
  tmp1 <- eval(parse(text = paste0("grp_", unique(grp_mat_chr$o)[i], "_avg")))
  plot(tmp1, nms, mar = c(4, 6, 4, 1), col = 'black', pch = 15, cex.axis = cex_labels, cex = cex_points, xlim = c(0, 0.4))
  # plot the individual ones
  cols <- c("red", "blue")
  pchs <- c(16, 18)
  corrections <- c(-0.2, -0.4)
  for (j in 1:2) {
    tmp <- eval(parse(text = paste0("grp_", unique(grp_mat_chr$o)[i], "_", j)))
    ord_tmp <- tmp$mat[match(as.numeric(tmp1$s), tmp$s), ]
    ord_tmp_2 <- ord_tmp[nrow(ord_tmp):1, ]
    points(ord_tmp_2$est, 1:12 + corrections[j], col = cols[j], pch = pchs[j], cex = cex_points)
    arrows(unlist(ord_tmp_2$cil), 1:dim(ord_tmp_2)[1] + corrections[j], unlist(ord_tmp_2$ciu), 1:dim(ord_tmp_2)[1] + corrections[j],
           length = 0, angle = 90, lwd = 2, col = cols[j])
  }
  # add labels
  title(main = nms_pub[i], cex.main = cex_main)
  # bump out the y-axis label, slightly
  # title(ylab = "Feature Group", cex.lab = cex_labels)
  mtext("Feature Group", side = 2, line = 4, cex = cex_labels)
  title(xlab = "Estimated variable importance", cex.lab = cex_labels)
  ## add a legend, only for the first one
  if (i == 5) {
    legend("bottomright", legend = c("Average", "Dataset 1", "Dataset 2"), col = c("black", "red", "blue"),
           pch = c(15, pchs), cex = cex_labels)
  }
  # close it off
  dev.off()
  
}


## plot of individual importance for each outcome and each dataset
fig_height_ind <- 4000
for (j in 1:2) {
  for (i in 1:length(unique(ind_mat_chr$o))) {
    # make a png
    png(paste0(plotDir, "vim_ind_outcome_", unique(ind_mat_chr$o)[i], "_dataset_", j, ".png"),height = fig_height_ind, width = fig_width, units = "px", res = 300)
    # names
    if (unique(ind_mat_chr$o)[i] == "slope_mod") {
      eval(parse(text = paste0("nms_tmp <- unique(ind_mat_chr$f)[as.numeric(unlist(lapply(strsplit(rownames(ind_", unique(ind_mat_chr$o)[i], "_", j , "$mat), '_', fixed = TRUE), function(x) x[4])))]")))
    } else {
      eval(parse(text = paste0("nms_tmp <- unique(ind_mat_chr$f)[as.numeric(unlist(lapply(strsplit(rownames(ind_", unique(ind_mat_chr$o)[i], "_", j , "$mat), '_', fixed = TRUE), function(x) x[3])))]")))  
    }
    nms <- rev(nms_tmp)
    # plot it
    eval(parse(text = paste0("plot(ind_", unique(ind_mat_chr$o)[i], "_", j , ", nms, mar = c(4, 4, 4, 1))")))
    # add labels
    title(main = paste0(toupper(nms_pub[i]), ", dataset ", j))
    title(ylab = "Feature set", line = -1)
    title(xlab = "Estimated variable importance")
    # close it off
    dev.off()
  }
}

##---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## Make the tables
##---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------\
# want groups first, in alphabetical order (by actual name), followed by individual features in alphabetical order (by actual name)
nms_group <- c("VRC01 contact sites (Set 1)", "CD4 binding sites (Set 2)", "ESA (Set 3)", 
               "Glycosylation sites (Set 4)", "Covarying sites (Set 5)", "PNG sites (Set 6)",
               "N-linked glycosylation (Set 7)", "Subtype (Set 8)", "Sequons (Set 9)",
               "Viral geometry (Set 10)", "Cysteines (Set 11)", "Steric bulk (Set 12)")
nms_ind <- unique(predictors)

# make the column names
col_nms <- paste0("Dataset ", rep(1:2, each = 3*5), ": ", rep(rep(c("cens", "sens.resis", "ic50", "ic80", "slope_mod"), each = 3), 2), ";", rep(c("Est.", "CIL", "CIU"), 5*2))

# make a giant matrix with estimate, ci for each outcome in columns and feature (set) in rows
mat <- create_table(grp_nms, ind_nms, nms_group, nms_ind, grp_mat, ind_mat, col_nms, TRUE)

# save off the giant table
write.csv(mat, paste0(tableDir, "vimp_table_with_groups.csv"))

# now make a giant matrix ordered by importance rather than by alphabetical
mat2 <- create_table(grp_nms, ind_nms, nms_group, nms_ind, grp_mat, ind_mat, col_nms, FALSE)
# save off the giant table
write.csv(mat2, paste0(tableDir, "vimp_table_with_groups_by_importance.csv"))

# now make a table for each outcome
# ordered by the average (groups first, then individual)
# average, set 1, set 2
# can choose a cutoff for publication ready table
col_nms_2 <- paste0(rep(c("Avg: ", "Data set 1: ", "Data set 2: "), each = 3), rep(c("Est.", "CIL", "CIU"), 3))
for (i in 1:length(unique(full_mat$o))) {
  eval(parse(text = paste0(unique(full_mat$o)[i], " <- create_outcome_table(grp_", unique(full_mat$o)[i], "_avg",
                           ", grp_", unique(full_mat$o)[i], "_1", ", grp_", unique(full_mat$o)[i], "_2",
                           ", nms_group , ind_", unique(full_mat$o)[i], "_avg", ", ind_", unique(full_mat$o)[i], "_1",
                           ", ind_", unique(full_mat$o)[i], "_2", ", nms_ind, col_nms_2)")))
  eval(parse(text = paste0("write.csv(", unique(full_mat$o)[i], ", paste0(tableDir, 'results/tables/vimp_table_by_outcome_", unique(full_mat$o)[i], ".csv'))")))
}
