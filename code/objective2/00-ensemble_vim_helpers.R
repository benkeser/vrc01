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