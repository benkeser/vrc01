# Prediction of VRC01 neutralization sensitivity by HIV-1 gp160 sequence features

**Authors:** Craig A. Magaret, David C. Benkeser, Brian D. Williamson, Bhavesh R. Borate, Lindsay N. Carpp, Ivelin S. Georgiev, Ian Setliff, Adam S. Dingens, Noah Simon, Marco Carone, David Montefiori, Galit Alter, Wen-Han Yu, Michal Juraska, Paul T. Edlefsen, Shelly Karuna, Nyaradzo M. Mgodi, Srilatha Edugupanti Peter B. Gilbert 

-----

## Description 

This repository contains code to reproduce the analyses of "Prediction of VRC01 neutralization sensitivity by HIV-1 gp160 sequence features" by Margaret, Williamson, Benkeser, et al (2018). This README provides a road map to the code available in the repository. The analysis was implemented in the freely available [`R`](https://cran.r-project.org/web/checks/check_results_drtmle.html) programming language. 

-----

## Data directory

The `data` directory contains two .csv files corresponding to the two separate data sets that were analyzed in the paper. These may be downloaded and locally read into `R` via `read.csv`, or, alternatively can be sourced directly from GitHub as follows.

```r
# RCurl can read directly from GitHub
library(RCurl)
# load data set 1
data1 <- read.csv(text = getURL("https://raw.githubusercontent.com/benkeser/vrc01/master/data/data1.csv"), header = TRUE)
# load data set 2
data2 <- read.csv(text = getURL("https://raw.githubusercontent.com/benkeser/vrc01/master/data/data2.csv"), header = TRUE)
```

-----

## Code directory

We have separated our code into two folders based on analysis objective, where the two objectives are restated here: 

1. Model selection -- to develop a best model or best few models for predicting TZM-bl neutralization sensitivity to VRC01 and advance the predicted values from this model or models as proteomic resistance scores for use in the primary AA sequence sieve analysis.
2. Feature selection -- to rank AA sequence features by their importance for predicting TZM-bl neutralization sensitivity to VRC01 and select the most important features to advance to the primary sieve analysis.
    a. `R` scripts are named according to the naming convention below.
    b. Shell scripts used to run the ensemble-based variable importance analysis are grouped into two sub-folders: `shell_scripts_hpc` contains example shell scripts for running all analyses on a high-performance computing cluster; `shell_scripts_local` contains shell scripts for running all analyses on a local machine.

Within each folder, we have adopted a naming convention for the `R` scripts therein. Files starting with 00- are considered preliminary files, that must be `source`'ed in to `R` to execute subsequent files. For example, `code/objective1/00-prelims.R` [is sourced](https://github.com/benkeser/vrc01/blob/0f46cbb3887d7d2247b7783bb63aee2793c1cec5/code/objective1/01-run_superlearner.R#L24) by `code/objective1/01-run_superlearner.R`. We further assume that lower number scripts are executed before higher number scripts in the same directory (`01-script.R` is run before `02-script.R`, etc...).

-----

## Issues

If you encounter any bugs or have any specific questions about the analysis, please
[file an issue](https://github.com/benkeser/vrc01/issues).

