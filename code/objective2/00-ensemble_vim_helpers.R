##---------------------------------------------------------
## This file contains helper functions for use in the
## ensemble-based variable importance analysis.
## All downstream analyses use this code.
##---------------------------------------------------------

## get predictions on the test set, using the outer layer for cvma
## ARGS: object - the cvma object
##            x - the matrix of predictors
##            v - the number of folds
## RETURNS: list, with
##          fits - list of the fitted values, averaged over the not v folds for v in 1:V
##         preds - list of the predictions, averaged over the not v folds for v in 1:V
##         folds - the folds, for use in computation downstream
get_preds <- function(object, x, V) {
  # make a list of training data
  trainDataList <- sapply(10:1, function(f){ return(x[fit$folds != f,,drop = FALSE])}, simplify = FALSE)
  # make a list of validation data 
  validDataList <- sapply(10:1, function(f){ return(x[fit$folds == f,,drop=FALSE]) }, simplify = FALSE)
  
  # call predict with outer = FALSE in a loop over this list; this might be slow because predict.cvma with inner = TRUE is currently predicting using ALL super learners
  fit_list_long <- lapply(trainDataList, function(train){
    predict(fit, newdata = train, outer = FALSE)
  })
  fit_list <- sapply(1:10, function(i){ fit_list_long[[i]][[i]]$sl_pred })
  # the first object in fit_list is the predictions on folds 1:9 for the super learner
  # fit in folds 1:9
  # the second object in fit_list is the predictions of folds 1:8,10 for the super learner
  # fit in folds 1:8,10
  # ...
  # the 10th object in fit_list is the predictions of folds 2:10 for the super learner 
  # fit in folds 2:10

  # call predict with outer = FALSE in a loop over this list; this might be slow because predict.cvma with inner = TRUE is currently predicting using ALL super learners
  pred_list_long <- lapply(validDataList, function(valid){
    predict(fit, newdata = valid, outer = FALSE)
  })
  pred_list <- sapply(1:10, function(i){ pred_list_long[[i]][[i]]$sl_pred })
  # the first object in fit_list is the predictions on fold 10 for the super learner
  # fit in folds 1:9. This is what should be fed to your one step function later
  # the second object in fit_list is the predictions of fold 9 for the super learner
  # fit in folds 1:8,10. 
  # ...
  # the 10th object in fit_list is the predictions of fold 10 for the super learner 
  # fit in folds 2:10. 

  return(list(fits = fit_list, preds = pred_list, folds = object$folds))
}

# turn each vector in the y list into a list, based on V folds
make_y_lst <- function(y, folds) {
  ## break it up into a list
  y_lst <- list()
  flds <- sort(unique(folds))
  for (v in 1:max(flds)) {
    y_lst[[v]] <- y[folds == flds[v]]
  }
  return(y_lst)
}

# match an outcome, e.g. "Y", to a character value, e.g. "ic50"
match_chr_y <- function(o, s) {
    ## set up the vectors of outcome names
    outs <- c("Y", "Y2", "Y.80", "Y2.80", "Y.slope", "Y2.slope", "Y.sens.resis", "Y2.sens.resis", "Y.cens", "Y2.cens")
    ord <- c("ic50", "ic80", "slope_mod", "sens.resis", "cens")
    # get which one it is
    indx <- which(o == ord)
    # make a vector of repeats to choose from based on o and s
    reps <- rep(1:5, each = 2)
    # return the one based on o and s
    ret_indx <- which(indx == reps)[s]
    ret <- outs[ret_indx]
    return(ret)
}

# match the correct outcome to a given row in the table
match_y <- function(row, y1, y2, folds1, folds2) {
  ord <- c("ic50", "ic80", "slope_mod", "sens.resis", "cens")
  if (as.numeric(row[3]) == 1) {
    tmp_y <- y1[[which(as.character(row[1]) == ord)]]  
    ret_y <- make_y_lst(tmp_y, folds1[[which(as.character(row[1]) == ord)]])
  } else {
    tmp_y <- y2[[which(as.character(row[1]) == ord)]]
    ret_y <- make_y_lst(tmp_y, folds2[[which(as.character(row[1]) == ord)]])
  }
  return(ret_y)
}

# create a table of results for each outcome, with the average as well
create_sub_table <- function(avg, set1, set2, nms) {
  # order set 1 and set 2 based on avg
  ord_1 <- match(avg$s, set1$s)
  ord_2 <- match(avg$s, set2$s)
  
  # now combine them together based on the matching
  ret <- cbind(avg$mat[, c(1, 3, 4)], set1$mat[ord_1, c(1, 3, 4)], set2$mat[ord_2, c(1, 3, 4)])
  colnames(ret) <- c("avg_est", "avg_cil", "avg_ciu",
                     "1_est", "1_cil", "1_ciu",
                     "2_est", "2_cil", "2_ciu")
  rownames(ret) <- nms[as.numeric(avg$s)]
  return(ret)
}

create_outcome_table <- function(avg_grp, set1_grp, set2_grp, nms_grp, avg_ind, set1_ind, set2_ind, nms_ind, col_nms) {
  # sub table for groups
  grp <- create_sub_table(avg_grp, set1_grp, set2_grp, nms_grp)
  # sub table for individual
  ind <- create_sub_table(avg_ind, set1_ind, set2_ind, nms_ind)
  # row bind together
  ret <- rbind(grp, ind)
  colnames(ret) <- col_nms
  return(ret)
}

# create a giant matrix with results
# and alphabetize the groups, then the individual features

create_table <- function(nms_grp, nms_ind, p_nms_grp, p_nms_ind, g_mat, i_mat, col_nms, alphabetize = TRUE) {
  # first make a matrix with the group ones
  grp <- matrix(0, nrow = length(p_nms_grp), ncol = 3*5*2)
  # for each feature set, create the matrix
  for (i in 1:length(p_nms_grp)) {
    # get the feature set names
    f_set_nms <- get_feature_list(nms_grp, i, g_mat)
    # pass them in to get the row
    grp[i, ] <- make_matrix(f_set_nms, g_mat)
  }
  # now slap on row names
  rownames(grp) <- p_nms_grp
  # alphabetize
  if (alphabetize) {
    ret_grp <- grp[order(rownames(grp)), ]  
  } else { # do it by order of the estimate for cens
    ret_grp <- grp[order(grp[, 1], decreasing = TRUE), ]
  }
  
  
  # do the same thing for independent
  ind <- matrix(0, nrow = length(p_nms_ind), ncol = 3*5*2)
  for (i in 1:length(p_nms_ind)) {
    # get feature set names
    f_set_nms <- get_feature_list(nms_ind, i, i_mat)
    # make a row
    ind[i, ] <- make_matrix(f_set_nms, i_mat)
  }
  # row names
  rownames(ind) <- p_nms_ind
  # alphabetize
  if (alphabetize) {
    ret_ind <- ind[order(rownames(ind)), ]  
  } else {
    ret_ind <- ind[order(ind[, 1], decreasing = TRUE), ]  
  }
  
  
  # make a big matrix
  ret <- rbind(ret_grp, ret_ind)
  colnames(ret) <- col_nms
  return(ret)
}

# helper function that takes in a name and returns the estimate and CI formatted nicely
return_est_ci <- function(nm) {
  eval(parse(text = paste0("ret <- c(", nm, "$est, ", nm, "$ci)")))
  return(ret)
}

# helper function that takes in a list of names and makes a matrix with columns
# est, ci for each outcome
make_matrix <- function(lst, mat) {
  # get the estimates for each name in the list, combine it into a vector
  tmp <- lapply(lst, return_est_ci)
  ret <- c(tmp[[5]], tmp[[4]], tmp[[1]], tmp[[2]], tmp[[3]])
  return(ret)
}

# helper function to extract a list the desired feature/feature set from a list of names
get_feature_list <- function(lst, num, mat) {
  indx <- which(mat$f == num)
  ret <-  as.list(lst[indx])
}

# calculate the two R^2's separately

calculate_r2 <- function(f, data = NULL, y = data[, 1], n = length(y), indx = 1, standardized = TRUE, two_phase = FALSE, tmle = FALSE, na.rm = FALSE, alpha = 0.05, SL.library = NULL, tol = .Machine$double.eps, max_iter = 500, V = 10, ...) {
  ## check to make sure they are the same length as y
  if (is.null(y)) stop("Y must be entered.")
  if (length(f) != length(y)) stop("The number of folds from the full regression must be the same length as the number of folds in y.")

  ## set up the fitted value objects (both are lists!)
  fhat <- f
  
  full <- reduced <- NA
  
  ## loop over the folds
  naive_cv <- vector("numeric", V)
  # updates_full <- vector("numeric", V)
  # updates_reduced <- vector("numeric", V)
  # ses_full <- vector("numeric", V)
  # ses_reduced <- vector("numeric", V)
  for (v in 1:V) {
    if (standardized) {
      naive_cv[v] <- mean((y[[v]] - fhat[[v]])^2, na.rm = na.rm)/mean((y[[v]] - mean(y[[v]], na.rm = na.rm))^2, na.rm = na.rm)
    } else {
      naive_cv[v] <- mean((y[[v]] - fhat[[v]])^2, na.rm = na.rm)
    }
    
    # updates[v] <- mean(variableImportanceIC(fhat[[v]], fhat[[v]], y[[v]], standardized = standardized, na.rm = na.rm), na.rm = na.rm)
    # ses[v] <- mean(variableImportanceIC(fhat[[v]], fhat[[v]], y[[v]], standardized = standardized, na.rm = na.rm)^2, na.rm = na.rm)
  }
  est <- mean(naive_cv) # for now, only return the naive
  # est <- mean(naive_cv) + mean(updates)
  ## calculate the standard error
  # se <- mean(ses)/sqrt(n)
  # ## calculate the confidence interval
  # ci <- variableImportanceCI(est, se, n, 1 - alpha)
  se <- NA
  ci <- c(NA, NA)
  
  ## get the call
  cl <- match.call()
  
  ## create the output and return it
  output <- list(call = cl, full.f = f, red.f = NA, data = data, s = indx,
                 SL.library = SL.library,
                 full.fit = fhat, red.fit = NA, est = est,
                 se = se, ci = ci, full.mod = full, red.mod = reduced,
                 alpha = alpha)
  
  ## make it also an vim object
  tmp.cls <- class(output)
  class(output) <- c("vim", tmp.cls)
  return(output)
}

# get the naive estimator
get_naive <- function(full, reduced, y, na.rm = FALSE) {
  V <- length(full)
  naives <- vector("numeric", V)
  for (v in 1:V) {
    naives[v] <- mean((full[[v]] - reduced[[v]])^2, na.rm = na.rm)/mean((y[[v]] - mean(y[[v]], na.rm = na.rm))^2, na.rm = na.rm)
  }
  ret <- mean(naives) 
  return(ret)
}

# get the correction
get_correction <- function(full, reduced, y, na.rm = FALSE) {
  V <- length(full)
  updates <- vector("numeric", V)
  for (v in 1:V) {
    updates[v] <- 2*mean((y[[v]] - full[[v]])*(full[[v]] - reduced[[v]]), na.rm = TRUE)/mean((y[[v]] - mean(y[[v]], na.rm = TRUE))^2, na.rm = TRUE)
  }
  ret <- mean(updates)
  return(ret)
}