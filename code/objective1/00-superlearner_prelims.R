#----------------------------------------------------------------------
# This script reads in data, and writes wrapper functions to interface
# with the cvma package in the Super Learner portion of the analysis
#----------------------------------------------------------------------

library(SuperLearner)
library(data.table)
library(nlme)

path.home <- "/repository/home/path"
setwd(path.home)

# set directory where data are located
dataDir <- "./data/"

# load the set 1 data set
data <- read.csv(paste0(dataDir,"data1.csv"))
data <- as.data.table(data, stringsAsFactors=FALSE)

# Names of variables considered as predictors:
geogregionvars <- c('geographic.region.of.origin.is.Asia',
                    'geographic.region.of.origin.is.Europe.Americas',
                    'geographic.region.of.origin.is.N.Africa',
                    'geographic.region.of.origin.is.S.Africa')
subtypevars <- c('subtype.is.01_AE','subtype.is.02_AG','subtype.is.07_BC',
                 'subtype.is.A1','subtype.is.A1C','subtype.is.A1D',
                 'subtype.is.B','subtype.is.C','subtype.is.D','subtype.is.O',
                 'subtype.is.Other')

# Pull out the VRC01 contact sites and the paratope + covariability sites separately
indbegin <- which( colnames(data)=='hxb2.46.E.1mer')
indend <- which(colnames(data)=='hxb2.477.N.1mer')
AAcharactervars <- names(data)[indbegin:indend]
AAposns <- unlist (lapply (strsplit (AAcharactervars, split=".", fixed=T), function (x) x[2]))

VRC01contactsites <- c(97, 123, 124, 198, 276, 278, 279, 280, 281, 282, 365, 366, 367, 368, 371, 427, 428, 429, 430, 455, 456, 457, 458, 459, 460, 461, 463, 465, 466, 467, 469, 472, 473, 474, 476)
keepVRC01contactsites <- AAposns %in% VRC01contactsites
AAVRC01contactsitescharactervars <- AAcharactervars[keepVRC01contactsites]

CD4bindingsites <- c(124, 125, 126, 127, 196, 198, 279, 280, 281, 282, 283, 365, 366, 367, 368, 369, 370, 374, 425, 426, 427, 428, 429, 430, 431, 432, 455, 456, 457, 458, 459, 460, 461, 469, 471, 472, 473, 474, 475, 476, 477)
keepCD4bindingsites <- AAposns %in% CD4bindingsites
AACD4bindingsitescharactervars <- AAcharactervars[keepCD4bindingsites]

ESAsites <- c(97, 198, 276, 278, 279, 280, 281, 282, 365, 366, 367, 368, 371, 415, 428, 429, 430, 455, 457, 458, 459, 460, 461, 467, 469, 473, 474, 476)
keepESAsites <- AAposns %in% ESAsites
AAESAsitescharactervars <- AAcharactervars[keepESAsites]

GLYCOsites <- c(61, 64, 197, 276, 362, 363, 386, 392, 462, 463)
keepGLYCOsites <- AAposns %in% GLYCOsites
AAGLYCOsitescharactervars <- AAcharactervars[keepGLYCOsites]

COVARsites <- c(46, 132, 138, 144, 150, 179, 181, 186, 190, 290, 321, 328, 354, 389, 394, 396, 397, 406)
keepCOVARsites <- AAposns %in% COVARsites
AACOVARsitescharactervars <- AAcharactervars[keepCOVARsites]

PNGsites <- c(130, 139, 143, 156, 187, 197, 241, 262, 289, 339, 355, 363, 406, 408, 410, 442, 448, 460, 462)
keepPNGsites <- AAposns %in% PNGsites
AAPNGsitescharactervars <- AAcharactervars[keepPNGsites]

OtherVRC01relevantsites <- sort(c(CD4bindingsites, ESAsites,GLYCOsites,COVARsites,PNGsites))
OtherVRC01relevantsites <- OtherVRC01relevantsites[!OtherVRC01relevantsites %in% VRC01contactsites]
keepOtherVRC01relevantsites <- AAposns %in% OtherVRC01relevantsites
AAOtherVRC01relevantsitescharactervars <- AAcharactervars[keepOtherVRC01relevantsites]

gp41sites <- c(544, 569, 582, 589, 655, 668, 675, 677, 680, 681, 683, 688, 702)
keepgp41sites <- AAposns %in% gp41sites
AAgp41sitescharactervars <- AAcharactervars[keepgp41sites] 

# Get N-glycosylation sites for all gp160 that are not included in VRC01 contact sites or paratope or sites with covariability
indbegin <- which( colnames(data)=='hxb2.29.sequon_actual.1mer' )  # The first AA residue variable in predictors
indend <- which( colnames(data)=='hxb2.824.sequon_actual.1mer' )   # The last AA residue variable in predictors
AAglycosylationgp160vars <- names(data)[indbegin:indend]
# No VRC01 or related sites
AAposns <- unlist (lapply (strsplit (AAglycosylationgp160vars, split=".", fixed=T), function (x) x[2]))
keepVRC01contactsites <- AAposns %in% VRC01contactsites
keepOtherVRC01relevantsites <- AAposns %in% OtherVRC01relevantsites
AAglycosylationgp160NoVRC01vars <- AAglycosylationgp160vars[!keepVRC01contactsites & !keepOtherVRC01relevantsites]

all_glycosylationvars <- c('sequons.total.subset','sequons.total.env','sequons.total.gp120','sequons.total.v5','sequons.total.loop.d','sequons.total.loop.e','sequons.total.vrc01','sequons.total.cd4','sequons.total.sj.fence','sequons.total.sj.trimer')
glycosylationvars <- all_glycosylationvars[all_glycosylationvars %in% colnames(data)]
all_cysteinesvars <- c('cysteines.total.env','cysteines.total.gp120','cysteines.total.v5','cysteines.total.loop.d','cysteines.total.loop.e','cysteines.total.vrc01','cysteines.total.cd4')
cysteinesvars <- all_cysteinesvars[all_cysteinesvars %in% colnames(data)]
all_viralgeometryvars <- c('length.env','length.gp120','length.v5','length.v5.outliers','length.loop.d','length.loop.e','length.loop.e.outliers')
viralgeometryvars <- all_viralgeometryvars[all_viralgeometryvars %in% colnames(data)]
all_stericbulkvars <- c('taylor.small.total.v5','taylor.small.total.loop.d','taylor.small.total.cd4')
stericbulkvars <- all_stericbulkvars[all_stericbulkvars %in% colnames(data)]

predictors <- c(geogregionvars,
                 subtypevars,
                 AAVRC01contactsitescharactervars,
                 AACD4bindingsitescharactervars,
                 AAESAsitescharactervars,
                 AAGLYCOsitescharactervars,
                 AACOVARsitescharactervars,
                 AAPNGsitescharactervars,
                 AAgp41sitescharactervars,
                 AAglycosylationgp160NoVRC01vars,
                 glycosylationvars,
                 viralgeometryvars,
                 cysteinesvars,
                 stericbulkvars)

# X is the vector of input variables, Y the quantitative response variable (outcome)
X <- as.data.frame(subset(data, select = unique(predictors)))
Y <- data$ic50.geometric.mean.imputed.log10
Y.cens <- as.numeric(data$ic50.censored)
Y.sens.resis <- data$binding.dichotomous.sens.resis
Y.80 <- data$ic80.geometric.mean.imputed.log10
Y.slope <- data$neutralization.slope

# load the set 2 data set
data2 = read.csv(paste0(dataDir,"/data2.csv"))
data2 = as.data.table(data2,stringsAsFactors=FALSE)
X2 <- as.data.frame(subset(data2,select=unique(predictors)))
Y2 <- data2$ic50.geometric.mean.imputed.log10
Y2.cens <- as.numeric(data2$ic50.censored)
Y2.sens.resis <- data2$binding.dichotomous.sens.resis
Y2.80 <- data2$ic80.geometric.mean.imputed.log10
Y2.slope <- data2$neutralization.slope
Yord <- ifelse(10^Y <1,0,ifelse(10^Y<=10,1,2))
Y2ord <- ifelse(10^Y2 <1,0,ifelse(10^Y2<=10,1,2))

# Define screening rules, which determine the set of input variables that are 
# considered by learning algorithms- a 'learning algorithm' is a method for
# estimating a conditional mean

#####################################################################################################
# Set up screening algorithms.  
# The screens consider the following different sets of variables:
# A. geogregion of origin (for confounding adjustment)
# B. AA residues at AA positions VRC01 footprint only
# C. AACD4bindingsitescharactervars
# D. AAESAsitescharactervars
# E. AAGLYCOsitescharactervars
# F. AACOVARsitescharactervars
# G. AAPNGsitescharactervars
# H. AAgp41sitescharactervars
# I. N-linked glycosylation for all gp160 that are not included in VRC01 contact sites or paratope or sites with covariability
# J. subtype of the virus
# K. glycosylation information
# L. Viral geometry
# M. Cysteine counts information
# N. Steric Bulk information
# O. All variables included

# All of the utilized screens include A. (geogregion of origin) for confounding adjustment.
# The screens consider certain unions of the variables above, as specified here: 
# 1. A
# 2. A+B
# 3. A+C  
# 4. A+D
# 5. A+E
# 6. A+F
# 7. A+G
# 8. A+H
# 9. A+I
# 10. A+J
# 11. A+K
# 12. A+L
# 13. A+M
# 14. A+N
# 15. All A+B+C+D+E+F+G+H+I+J+K+L+M+N

screen.geogregion.1 <- function(Y, X, family, obsWeights, id, ...) {
  # Set all variables to false
  vars <- rep(FALSE, ncol(X))
  # Set all variables to always adjust (i.e., potential confounders) for as true
  vars[names(X) %in% c('geographic.region.of.origin.is.Asia','geographic.region.of.origin.is.Europe.Americas','geographic.region.of.origin.is.N.Africa','geographic.region.of.origin.is.S.Africa')] <- TRUE
  vars
}


screen.geogregion.AAcharVRC01.2 <- function(Y, X, family, obsWeights, id, ...) {
  # Set all variables to false
  vars <- rep(FALSE, ncol(X))
  # Set all variables to always adjust (i.e., potential confounders) for as true
  vars[names(X) %in% c('geographic.region.of.origin.is.Asia','geographic.region.of.origin.is.Europe.Americas','geographic.region.of.origin.is.N.Africa','geographic.region.of.origin.is.S.Africa')] <- TRUE
  # Set AA residue information as true, only keeping residue variables with at least 3 1s and at least 3 0s
  # (i.e., the residue is present in >=3 and absent in >= 3)
  AAcharactervars <- names(X[,AAVRC01contactsitescharactervars])
  keeps <- apply(X[,AAVRC01contactsitescharactervars],2,function(col) length(col[col==1])>2 & length(col[col==0]) >2)
  AAcharactervars <- AAcharactervars[keeps]
  vars[names(X) %in% AAcharactervars] <- TRUE
  vars
}


screen.geogregion.AAcharCD4bs.3 <- function(Y, X, family, obsWeights, id, ...) {
  # Set all variables to false
  vars <- rep(FALSE, ncol(X))
  # Set all variables to always adjust (i.e., potential confounders) for as true
  vars[names(X) %in% c('geographic.region.of.origin.is.Asia','geographic.region.of.origin.is.Europe.Americas','geographic.region.of.origin.is.N.Africa','geographic.region.of.origin.is.S.Africa')] <- TRUE
  # Set AA residue information as true, only keeping residue variables with at least 3 1s and at least 3 0s
  # (i.e., the residue is present in >=3 and absent in >= 3)
  AAcharactervars <- names(X[,AACD4bindingsitescharactervars])
  keeps <- apply(X[,AACD4bindingsitescharactervars],2,function(col) length(col[col==1])>2 & length(col[col==0]) >2)
  AAcharactervars <- AAcharactervars[keeps]
  vars[names(X) %in% AAcharactervars] <- TRUE
  vars
}


screen.geogregion.AAcharESA.4 <- function(Y, X, family, obsWeights, id, ...) {
  # Set all variables to false
  vars <- rep(FALSE, ncol(X))
  # Set all variables to always adjust (i.e., potential confounders) for as true
  vars[names(X) %in% c('geographic.region.of.origin.is.Asia','geographic.region.of.origin.is.Europe.Americas','geographic.region.of.origin.is.N.Africa','geographic.region.of.origin.is.S.Africa')] <- TRUE
  # Set AA residue information as true, only keeping residue variables with at least 3 1s and at least 3 0s
  # (i.e., the residue is present in >=3 and absent in >= 3)
  AAcharactervars <- names(X[,AAESAsitescharactervars])
  keeps <- apply(X[,AAESAsitescharactervars],2,function(col) length(col[col==1])>2 & length(col[col==0]) >2)
  AAcharactervars <- AAcharactervars[keeps]
  vars[names(X) %in% AAcharactervars] <- TRUE
  vars
}


screen.geogregion.AAcharGLYCO.5 <- function(Y, X, family, obsWeights, id, ...) {
  # Set all variables to false
  vars <- rep(FALSE, ncol(X))
  # Set all variables to always adjust (i.e., potential confounders) for as true
  vars[names(X) %in% c('geographic.region.of.origin.is.Asia','geographic.region.of.origin.is.Europe.Americas','geographic.region.of.origin.is.N.Africa','geographic.region.of.origin.is.S.Africa')] <- TRUE
  # Set AA residue information as true, only keeping residue variables with at least 3 1s and at least 3 0s
  # (i.e., the residue is present in >=3 and absent in >= 3)
  AAcharactervars <- names(X[,AAGLYCOsitescharactervars])
  keeps <- apply(X[,AAGLYCOsitescharactervars],2,function(col) length(col[col==1])>2 & length(col[col==0]) >2)
  AAcharactervars <- AAcharactervars[keeps]
  vars[names(X) %in% AAcharactervars] <- TRUE
  vars
}


screen.geogregion.AAcharCOVAR.6 <- function(Y, X, family, obsWeights, id, ...) {
  # Set all variables to false
  vars <- rep(FALSE, ncol(X))
  # Set all variables to always adjust (i.e., potential confounders) for as true
  vars[names(X) %in% c('geographic.region.of.origin.is.Asia','geographic.region.of.origin.is.Europe.Americas','geographic.region.of.origin.is.N.Africa','geographic.region.of.origin.is.S.Africa')] <- TRUE
  # Set AA residue information as true, only keeping residue variables with at least 3 1s and at least 3 0s
  # (i.e., the residue is present in >=3 and absent in >= 3)
  AAcharactervars <- names(X[,AACOVARsitescharactervars])
  keeps <- apply(X[,AACOVARsitescharactervars],2,function(col) length(col[col==1])>2 & length(col[col==0]) >2)
  AAcharactervars <- AAcharactervars[keeps]
  vars[names(X) %in% AAcharactervars] <- TRUE
  vars
}



screen.geogregion.AAcharPNGS.7 <- function(Y, X, family, obsWeights, id, ...) {
  # Set all variables to false
  vars <- rep(FALSE, ncol(X))
  # Set all variables to always adjust (i.e., potential confounders) for as true
  vars[names(X) %in% c('geographic.region.of.origin.is.Asia','geographic.region.of.origin.is.Europe.Americas','geographic.region.of.origin.is.N.Africa','geographic.region.of.origin.is.S.Africa')] <- TRUE
  # Set AA residue information as true, only keeping residue variables with at least 3 1s and at least 3 0s
  # (i.e., the residue is present in >=3 and absent in >= 3)
  AAcharactervars <- names(X[,AAPNGsitescharactervars])
  keeps <- apply(X[,AAPNGsitescharactervars],2,function(col) length(col[col==1])>2 & length(col[col==0]) >2)
  AAcharactervars <- AAcharactervars[keeps]
  vars[names(X) %in% AAcharactervars] <- TRUE
  vars
}


screen.geogregion.AAchargp41.8 <- function(Y, X, family, obsWeights, id, ...) {
  # Set all variables to false
  vars <- rep(FALSE, ncol(X))
  # Set all variables to always adjust (i.e., potential confounders) for as true
  vars[names(X) %in% c('geographic.region.of.origin.is.Asia','geographic.region.of.origin.is.Europe.Americas','geographic.region.of.origin.is.N.Africa','geographic.region.of.origin.is.S.Africa')] <- TRUE
  # Set AA residue information as true, only keeping residue variables with at least 3 1s and at least 3 0s
  # (i.e., the residue is present in >=3 and absent in >= 3)
  AAcharactervars <- names(X[,AAgp41sitescharactervars])
  keeps <- apply(X[,AAgp41sitescharactervars],2,function(col) length(col[col==1])>2 & length(col[col==0]) >2)
  AAcharactervars <- AAcharactervars[keeps]
  vars[names(X) %in% AAcharactervars] <- TRUE
  vars
}


screen.geogregion.AAcharNlinkGlyGP160.9 <- function(Y, X, family, obsWeights, id, ...) {
  # Set all variables to false
  vars <- rep(FALSE, ncol(X))
  # Set all variables to always adjust (i.e., potential confounders) for as true
  vars[names(X) %in% c('geographic.region.of.origin.is.Asia','geographic.region.of.origin.is.Europe.Americas','geographic.region.of.origin.is.N.Africa','geographic.region.of.origin.is.S.Africa')] <- TRUE
  # Set AA residue information as true, only keeping residue variables with at least 3 1s and at least 3 0s
  # (i.e., the residue is present in >=3 and absent in >= 3)
  AAcharactervars <- names(X[,AAglycosylationgp160NoVRC01vars])
  keeps <- apply(X[,AAglycosylationgp160NoVRC01vars],2,function(col) length(col[col==1])>2 & length(col[col==0]) >2)
  AAcharactervars <- AAcharactervars[keeps]
  vars[names(X) %in% AAcharactervars] <- TRUE
  vars
}

screen.geogregion.subtypes.10 <- function(Y, X, family, obsWeights, id, ...) {
  # Set all variables to false
  vars <- rep(FALSE, ncol(X))
  # Set all variables to always adjust (i.e., potential confounders) for as true
  vars[names(X) %in% c('geographic.region.of.origin.is.Asia','geographic.region.of.origin.is.Europe.Americas','geographic.region.of.origin.is.N.Africa','geographic.region.of.origin.is.S.Africa')] <- TRUE
  vars[names(X) %in% c('subtype.is.01_AE','subtype.is.02_AG','subtype.is.07_BC','subtype.is.A1','subtype.is.A1C','subtype.is.A1D','subtype.is.B','subtype.is.C','subtype.is.D','subtype.is.O','subtype.is.Other')] <- TRUE
  vars
}


# !!!! DB NOTE: Updated to reflect what's in current data set
screen.geogregion.glycosylation.11 <- function(Y, X, family, obsWeights, id, ...) {
  # Set all variables to false
  vars <- rep(FALSE, ncol(X))
  # Set all variables to always adjust (i.e., potential confounders) for as true
  vars[names(X) %in% c('geographic.region.of.origin.is.Asia','geographic.region.of.origin.is.Europe.Americas','geographic.region.of.origin.is.N.Africa','geographic.region.of.origin.is.S.Africa')] <- TRUE
  # Set glycosylation/sequon information as true:
  vars[names(X) %in% c("sequons.total.env", "sequons.total.gp120", "sequons.total.v5", "sequons.total.loop.d", "sequons.total.loop.e", "sequons.total.vrc01", "sequons.total.cd4", "sequons.total.sj.fence", "sequons.total.sj.trimer")] <- TRUE
  vars
}


screen.geogregion.viralgeometry.12 <- function(Y, X, family, obsWeights, id, ...) {
  # Set all variables to false
  vars <- rep(FALSE, ncol(X))
  # Set all variables to always adjust (i.e., potential confounders) for as true
  vars[names(X) %in% c('geographic.region.of.origin.is.Asia','geographic.region.of.origin.is.Europe.Americas','geographic.region.of.origin.is.N.Africa','geographic.region.of.origin.is.S.Africa')] <- TRUE
  # Set cysteines information as true:
  vars[names(X) %in% c("length.env", "length.gp120", "length.v5", "length.v5.outliers", "length.loop.e", "length.loop.e.outliers")] <- TRUE
  vars
}


screen.geogregion.cysteines.13 <- function(Y, X, family, obsWeights, id, ...) {
  # Set all variables to false
  vars <- rep(FALSE, ncol(X))
  # Set all variables to always adjust (i.e., potential confounders) for as true
  vars[names(X) %in% c('geographic.region.of.origin.is.Asia','geographic.region.of.origin.is.Europe.Americas','geographic.region.of.origin.is.N.Africa','geographic.region.of.origin.is.S.Africa')] <- TRUE
  # Set cysteines information as true:
  vars[names(X) %in% c("cysteines.total.env", "cysteines.total.gp120", "cysteines.total.v5", "cysteines.total.vrc01")] <- TRUE
  vars
}


# Steric Bulk 
screen.geogregion.StericBulk.14 <- function(Y, X, family, obsWeights, id, ...) {
  # Set all variables to false
  vars <- rep(FALSE, ncol(X))
  # Set all variables to always adjust (i.e., potential confounders) for as true
  vars[names(X) %in% c('geographic.region.of.origin.is.Asia','geographic.region.of.origin.is.Europe.Americas','geographic.region.of.origin.is.N.Africa','geographic.region.of.origin.is.S.Africa')] <- TRUE
  # Set cysteines information as true:
  vars[names(X) %in% c("taylor.small.total.v5", "taylor.small.total.loop.d", "taylor.small.total.cd4")] <- TRUE
  vars
}

screen.all <- function(Y, X, family, obsWeights, id, ...) {
  # Set all to true
  vars <- rep(TRUE, ncol(X))
  return(vars)
}

screen.geog.corP <- function(Y, X, family, obsWeights, id, ...) {
  # Set all variables to false
  vars <- rep(FALSE, ncol(X))
  vars <- screen.corP(Y, X, family, obsWeights, id, minPvalue = 0.05/ncol(X), ...)
  # Set all variables to always adjust (i.e., potential confounders) 
  # for as true (countries/regions)
  vars[names(X) %in% c('geographic.region.of.origin.is.Asia','geographic.region.of.origin.is.Europe.Americas','geographic.region.of.origin.is.N.Africa','geographic.region.of.origin.is.S.Africa')] <- TRUE
  vars
}

screen.geog.glmnet <- function(Y, X, family, obsWeights, id, alpha = 1, minscreen = 2, nfolds = 10, nlambda = 100, ...) {
  # Set all variables to false
  vars <- rep(FALSE, ncol(X))
  vars <- screen.glmnet(Y, X, family, obsWeights, id, alpha = 1, minscreen = 2, nfolds = 10, nlambda=100, ...)
  vars[names(X) %in% c('geographic.region.of.origin.is.Asia','geographic.region.of.origin.is.Europe.Americas','geographic.region.of.origin.is.N.Africa','geographic.region.of.origin.is.S.Africa')] <- TRUE
  vars
}

# Short names for plotting 
screen.geog <- screen.geogregion.1
screen.geog.AAchVRC01 <- screen.geogregion.AAcharVRC01.2
screen.geog.AAchCD4bs <- screen.geogregion.AAcharCD4bs.3 
screen.geog.AAchESA <- screen.geogregion.AAcharESA.4
screen.geog.AAchGLYCO<- screen.geogregion.AAcharGLYCO.5 
screen.geog.AAchCOVAR <- screen.geogregion.AAcharCOVAR.6 
screen.geog.AAchPNGS <- screen.geogregion.AAcharPNGS.7 
screen.geog.AAchgp41 <- screen.geogregion.AAchargp41.8
screen.geog.AAchGlyGP160 <- screen.geogregion.AAcharNlinkGlyGP160.9
screen.geog.st <- screen.geogregion.subtypes.10
screen.geog.sequonCt <- screen.geogregion.glycosylation.11
screen.geog.geom <- screen.geogregion.viralgeometry.12
screen.geog.cys <- screen.geogregion.cysteines.13
screen.geog.sbulk <- screen.geogregion.StericBulk.14 

screens <- c("screen.geog","screen.geog.AAchVRC01","screen.geog.AAchCD4bs","screen.geog.AAchESA","screen.geog.AAchGLYCO","screen.geog.AAchCOVAR",
             "screen.geog.AAchPNGS","screen.geog.AAchgp41","screen.geog.AAchGlyGP160","screen.geog.sequonCt","screen.geog.geom","screen.geog.cys",
             "screen.geog.sbulk","screen.all","screen.geog.corP","screen.geog.glmnet") 

screens.vimp <- c("screen.geog.corP", "screen.geog.glmnet")
regular_screens <- c("screen.geog", "screen.geog.sequonCt","screen.geog.geom","screen.geog.cys","screen.geog.corP","screen.geog.glmnet") 
regular_screens <- c(regular_screens, "screen.geog.st")
adaptive_screens <- screens

# Define the learning algorithms/methods.
# Create wrappers that are less memory-intensive than the standard
# glm fits, but that we can still use to predict later. 
SL.glm.skinny <- function(Y, X, newX, family, obsWeights, ...){
  SL.glm.fit <- SL.glm(Y = Y, X = X, newX = newX, family = family, obsWeights = obsWeights, ...)
  SL.glm.fit$fit$object$y <- NULL
  SL.glm.fit$fit$object$model <- NULL
  SL.glm.fit$fit$object$residuals <- NULL
  SL.glm.fit$fit$object$fitted.values <- NULL
  SL.glm.fit$fit$object$effects <- NULL
  SL.glm.fit$fit$object$qr$qr <- NULL
  SL.glm.fit$fit$object$linear.predictors <- NULL
  SL.glm.fit$fit$object$weights <- NULL
  SL.glm.fit$fit$object$prior.weights <- NULL
  SL.glm.fit$fit$object$data <- NULL
  SL.glm.fit$fit$object$family$variance <- NULL
  SL.glm.fit$fit$object$family$dev.resids <- NULL
  SL.glm.fit$fit$object$family$aic <- NULL
  SL.glm.fit$fit$object$family$validmu <- NULL
  SL.glm.fit$fit$object$family$simulate <- NULL
  attr(SL.glm.fit$fit$object$terms, ".Environment") <- NULL
  attr(SL.glm.fit$fit$object$formula, ".Environment") <- NULL
  return(SL.glm.fit)
}

SL.step.interaction.skinny <- function(Y, X, newX, family, obsWeights, ...){
  SL.step.interaction.fit <- SL.step.interaction(Y = Y, X = X, newX = newX, family = family, 
                                                 obsWeights = obsWeights, direction = "forward", ...)
  SL.step.interaction.fit$fit$object$y <- NULL
  SL.step.interaction.fit$fit$object$model <- NULL
  SL.step.interaction.fit$fit$object$residuals <- NULL
  SL.step.interaction.fit$fit$object$fitted.values <- NULL
  SL.step.interaction.fit$fit$object$effects <- NULL
  SL.step.interaction.fit$fit$object$qr$qr <- NULL
  SL.step.interaction.fit$fit$object$linear.predictors <- NULL
  SL.step.interaction.fit$fit$object$weights <- NULL
  SL.step.interaction.fit$fit$object$prior.weights <- NULL
  SL.step.interaction.fit$fit$object$data <- NULL
  SL.step.interaction.fit$fit$object$family$variance <- NULL
  SL.step.interaction.fit$fit$object$family$dev.resids <- NULL
  SL.step.interaction.fit$fit$object$family$aic <- NULL
  SL.step.interaction.fit$fit$object$family$validmu <- NULL
  SL.step.interaction.fit$fit$object$family$simulate <- NULL
  attr(SL.step.interaction.fit$fit$object$terms, ".Environment") <- NULL
  attr(SL.step.interaction.fit$fit$object$formula, ".Environment") <- NULL
  return(SL.step.interaction.fit)
}

SL.step.skinny <- function(Y, X, newX, family, obsWeights, ...){
  SL.step.fit <- SL.step(Y = Y, X = X, newX = newX, family = family, obsWeights = obsWeights, direction = "forward", ...)
  SL.step.fit$fit$object$y <- NULL
  SL.step.fit$fit$object$model <- NULL
  SL.step.fit$fit$object$residuals <- NULL
  SL.step.fit$fit$object$fitted.values <- NULL
  SL.step.fit$fit$object$effects <- NULL
  SL.step.fit$fit$object$qr$qr <- NULL
  SL.step.fit$fit$object$linear.predictors <- NULL
  SL.step.fit$fit$object$weights <- NULL
  SL.step.fit$fit$object$prior.weights <- NULL
  SL.step.fit$fit$object$data <- NULL
  SL.step.fit$fit$object$family$variance <- NULL
  SL.step.fit$fit$object$family$dev.resids <- NULL
  SL.step.fit$fit$object$family$aic <- NULL
  SL.step.fit$fit$object$family$validmu <- NULL
  SL.step.fit$fit$object$family$simulate <- NULL
  attr(SL.step.fit$fit$object$terms, ".Environment") <- NULL
  attr(SL.step.fit$fit$object$formula, ".Environment") <- NULL
  return(SL.step.fit)
}

# skinny gam object
SL.gam.skinny <- function(Y, X, newX, family, obsWeights, ...){
  SL.gam.fit <- SL.gam(Y = Y, X = X, newX = newX, family = family, obsWeights = obsWeights, ...)
  SL.gam.fit$fit$object$y <- NULL
  SL.gam.fit$fit$object$data <- NULL
  SL.gam.fit$fit$object$deviance <- NULL
  SL.gam.fit$fit$object$aic <- NULL
  SL.gam.fit$fit$object$effects <- NULL
  SL.gam.fit$fit$object$model <- NULL
  SL.gam.fit$fit$object$fitted.values <- NULL
  SL.gam.fit$fit$object$effects <- NULL
  SL.gam.fit$fit$object$qr$qr <- NULL
  SL.gam.fit$fit$object$nl.df <- NULL
  SL.gam.fit$fit$object$df.residual <- NULL
  SL.gam.fit$fit$object$var <- NULL
  SL.gam.fit$fit$object$additive.predictors <- NULL
  SL.gam.fit$fit$object$R <- NULL
  SL.gam.fit$fit$object$rank <- NULL
  SL.gam.fit$fit$object$prior.weights <- NULL
  SL.gam.fit$fit$object$data <- NULL
  SL.gam.fit$fit$object$control <- NULL
  SL.gam.fit$fit$object$method <- NULL
  SL.gam.fit$fit$object$contrasts <- NULL
  SL.gam.fit$fit$object$nl.chisq <- NULL
  SL.gam.fit$fit$object$formula <- NULL
  SL.gam.fit$fit$object$family$variance <- NULL
  SL.gam.fit$fit$object$family$dev.resids <- NULL
  SL.gam.fit$fit$object$family$aic <- NULL
  SL.gam.fit$fit$object$family$validmu <- NULL
  SL.gam.fit$fit$object$family$dev.resids <- NULL
  SL.gam.fit$fit$object$family$aic <- NULL
  return(SL.gam.fit)
}

# boosted decision stumps
SL.stumpboost <- function(Y, X, newX, family, obsWeights, ...){
  fit <- SL.xgboost(Y = Y, X = X, newX = newX, family = family, obsWeights = obsWeights, 
                    max_depth = 1, # so it's only a stump
                    ...)
  return(fit)
}

# naive bayes wrapper
SL.naivebayes <- function(Y, X, newX, family, obsWeights, laplace = 0, ...){
  SuperLearner:::.SL.require("e1071")
  if(family$family == "gaussian"){
    stop("SL.naivebayes only works with binary outcomes")
  }else{
    nb <- naiveBayes(y = Y, x = X, laplace = laplace)
    pred <- predict(nb, newX, type = "raw")[,2]
    out <- list(fit = list(object = nb), pred = pred)
    class(out$fit) <- "SL.naivebayes"
    return(out)
  }
}

# predict method for naive bayes wrapper
predict.SL.naivebayes <- function(object, newdata, ...){
  pred <- predict(object$object, newdata = newdata, type = "raw")[,2]
  return(pred)
}

regular_methods <- c("SL.glm.skinny", "SL.step.interaction.skinny", "SL.step.skinny")
adaptive_methods <- c("SL.glmnet","SL.randomForest",
                      "SL.stumpboost", # maybe the source of bad memory behavior?
                      "SL.naivebayes")

#' This function takes a super learner method wrapper and a super learner
#' screen wrapper and combines them into a single wrapper and makes that 
#' wrapper available in the specified environment. It also makes a predict
#' method available in the specified environment.
#' @param method A super learner method wrapper. See ?SuperLearner::listWrappers(what = "method").
#' @param screen A super learner method wrapper. See ?SuperLearner::listWrappers(what = "screen").
#' @param envir The environment to assign the functions to (default is global environment)
#' @param verbose Print a message with the function names confirming their assignment?
assign_combined_function <- function(method, screen, envir = .GlobalEnv,
                                     verbose = TRUE){
  fn <- eval(parse(text = 
          paste0("function(Y, X, newX, obsWeights, family, ...){ \n",
                    "screen_call <- ", screen, "(Y = Y, X = X, newX = newX, obsWeights = obsWeights, family = family, ...) \n",
                    "method_call <- ", method, "(Y = Y, X = X[,screen_call,drop=FALSE], newX = newX[,screen_call,drop = FALSE], obsWeights = obsWeights, family = family, ...) \n",
                    "pred <- method_call$pred \n",
                    "fit <- list(object = method_call$fit$object, which_vars = screen_call) \n",
                    "class(fit) <- paste0('", screen, "', '_', '", method, "') \n",
                    "out <- list(fit = fit, pred = pred) \n",
                    "return(out) \n",
                    "}")))
  fn_name <- paste0(screen,"_",method)
  assign(x = fn_name, value = fn, envir = envir)
  if(verbose){
    message(paste0("Function ", fn_name, " now available in requested environment."))
  }
  if (method == "SL.glmnet") {
      pred_fn <- eval(parse(text = 
              paste0("function(object, newdata, ...){ \n",
                        "screen_newdata <- newdata[,object$which_vars,drop = FALSE] \n",
                        "pred <- predict(object$object, type = 'response', newx = as.matrix(screen_newdata), s = 'lambda.min', ...) \n",
                        "return(pred) \n",
                     "}")))
    } else if (method == "SL.stumpboost") {
      pred_fn <- eval(parse(text = 
              paste0("function(object, newdata, ...){ \n",
                        "screen_newdata <- newdata[,object$which_vars,drop = FALSE] \n",
                        "screen_newdata_2 <- matrix(unlist(lapply(screen_newdata, as.numeric)), nrow=nrow(screen_newdata), ncol=ncol(screen_newdata)) \n",
                        "pred <- predict(object$object, newdata = screen_newdata_2, ...) \n",
                        "return(pred) \n",
                     "}")))
    } else if (method == "SL.naivebayes") {
      pred_fn <- eval(parse(text = 
              paste0("function(object, newdata, ...){ \n",
                        "screen_newdata <- newdata[,object$which_vars,drop = FALSE] \n",
                        'pred <- predict(object$object, newdata = screen_newdata, type = "raw", ...)[,2] \n',
                        "return(pred) \n",
                     "}")))
    } else if (method == "SL.randomForest") {
      pred_fn <- eval(parse(text = 
              paste0("function(object, newdata, ...){ \n",
              "screen_newdata <- newdata[,object$which_vars,drop = FALSE] \n",
              "if (object$object$type != 'classification') {
                                    pred <- predict(object$object, newdata = screen_newdata, type = 'response')
                                }else {
                                    pred <- predict(object$object, newdata = screen_newdata, type = 'vote')[, 
                                        2]
                                }
                                pred",
                     "}")))
    }else {
      pred_fn <- eval(parse(text = 
              paste0("function(object, newdata, ...){ \n",
                        "screen_newdata <- newdata[,object$which_vars,drop = FALSE] \n",
                        "pred <- predict(object$object, type = 'response', newdata = screen_newdata, ...) \n",
                        "return(pred) \n",
                     "}")))
    }
  
  pred_fn_name <- paste0("predict.",screen,"_",method)
  assign(x = pred_fn_name, value = pred_fn, envir = envir)
  if(verbose){
    message(paste0("Function ", pred_fn_name, " now available in requested environment."))
  }
}

# Make a data frame of all the method/screen combinations needed
adaptive_screen_method_frame <- expand.grid(screen = adaptive_screens, 
                                            method = adaptive_methods)
regular_screen_method_frame <- expand.grid(screen = regular_screens, 
                                            method = regular_methods)
# Assign screen + method wrappers to global environment
apply(adaptive_screen_method_frame, 1, function(x){ assign_combined_function(screen = x[1], method = x[2]) })
apply(regular_screen_method_frame, 1, function(x){ assign_combined_function(screen = x[1], method = x[2]) })

# Generate library names 
SL.library <- c(apply(regular_screen_method_frame, 1, paste0, collapse = "_"), 
                apply(adaptive_screen_method_frame, 1, paste0, collapse = "_"), 
                "SL.mean")

SL.library.continuous <- SL.library[!grepl("SL.naivebayes", SL.library)]
SL.library.binary <- SL.library

# Same operations for vimp analysis
screen_method_frame_vimp <- expand.grid(screen = screens.vimp, 
                                   method = c(regular_methods, adaptive_methods))

# Assign screen + method wrappers to global environment
apply(screen_method_frame_vimp, 1, function(x){ assign_combined_function(screen = x[1], method = x[2]) })

# Generate library names 
SL.library.vimp <- c(apply(screen_method_frame_vimp, 1, paste0, collapse = "_"), "SL.mean")