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