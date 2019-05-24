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

Within each folder, we have adopted a naming convention for the `R` scripts
therein. Files starting with 00- are considered preliminary files, that are
generally `source`'ed in to `R` to execute subsequent files. For example,
`code/objective1/00-superlearner_prelims.R` is sourced by `code/objective1/01-run_superlearner.R`. We further assume that lower number scripts are executed before higher number scripts in the same directory (`01-script.R` is run before `02-script.R`, etc...).

Users will also find this line appearing in multiple `R` scripts: 

```r
path.home <- "/repository/home/path"
```

The `path.home` variable should be set to the directory of the repository on
the user's local machine. This enables subsequent scripts to find relevant
files. 

The workflow also relies on the `cvma` `R` package, which is not available on
CRAN. The version of the package used in the analysis may be downloaded from
GitHub. 

```r
devtools::install_github("benkeser/cvma", ref = "fd8008d0c9bb88cee4884aa62da8998216ac3463")
```

-----

## Issues

If you encounter any bugs or have any specific questions about the analysis, please
[file an issue](https://github.com/benkeser/vrc01/issues).

