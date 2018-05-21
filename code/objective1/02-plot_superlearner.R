# ---------------------------------------------------------------------------- #
# This code creates the figures and tables for the manuscript
# ---------------------------------------------------------------------------- #

rm(list=ls(all=TRUE))
libs <-c("SuperLearner", "parallel", "data.table", "ggplot2", "stringr", "xtable", "future", "cvma", "miscTools", "dplyr", "glmnet", "randomForest", "grid", "xgboost",
         "naivebayes", "e1071", "cvAUC", "ROCR", "gridExtra", "cowplot", "gtools", "tidyr")
lapply(libs, require, character.only = TRUE)

require(cowplot)

# define directory to source makeDataAndFunctions
codeDir <- "~/code/objective1/"
# define director to save results
outDir <- "~"
# run makeDataAndFunctions.R script
source(paste0(codeDir, "00-superlearner_prelims.R"))
source(paste0(codeDir, "00-plot_prelims.R"))


#################################################################################################################
# FIGURES
#################################################################################################################
# Create figure to show distribution of outcomes
dat1 = read.csv(file = "~/data/data1.csv")
dat1$Dataset = "Dataset 1"
dat2 = read.csv(file = "~/data/data2.csv")
dat2$Dataset = "Dataset 2"

dat = rbind(dat1, dat2) %>% select(Dataset, ic50.geometric.mean.imputed.log10, ic80.geometric.mean.imputed.log10, neutralization.slope, ic50.censored, binding.dichotomous.sens.resis)

# Only Continuous outcomes: IC50 and IC80
dat_long = gather(dat[,c("Dataset", "ic50.geometric.mean.imputed.log10", "ic80.geometric.mean.imputed.log10")], outcome, value, ic50.geometric.mean.imputed.log10:ic80.geometric.mean.imputed.log10, factor_key=TRUE)
dat_long$outcome = ifelse(dat_long$outcome=="ic50.geometric.mean.imputed.log10", "log[10]*' IC'[50]", 
                          ifelse(dat_long$outcome=="ic80.geometric.mean.imputed.log10", "log[10]*' IC'[80]", dat_long$outcome))
# create proportional stacked bars
ntext <- dat_long[!is.na(dat_long$value),] %>% group_by(Dataset, outcome) %>% dplyr::summarise(n = n())
dat_long = merge(dat_long, ntext, by=c("Dataset", "outcome"))
mid=0
high=0.5
low=-0.5

set.seed(20180430)
p1 = ggplot(dat_long, aes(x=Dataset, y=value, color=value)) + facet_grid(.~outcome, labeller = label_parsed) + 
  geom_jitter(size=3, width = 0.25) + xlab("")  + 
  scale_y_continuous(limits=c(-2,3.2)) +
  geom_boxplot(outlier.colour=NA, fill="gray70",alpha=0.2) + 
  scale_color_gradient2(midpoint=mid, low="blue", mid="gray90", high="red", guide = "colourbar", space="Lab") + 
  theme(legend.position="top", legend.title = element_blank(), strip.text = element_text(face="bold", size=18), axis.text.y = element_text(size=17), axis.title.y = element_text(size=18), axis.text.x = element_text(size=17)) + 
  ylab("Value") + 
  geom_text(aes(label = paste0("n = ", n), y = 2.8), size=7, vjust = 1.5, color = "black")

# Only Continuous outcomes: Slope
dat_long = gather(dat[,c("Dataset", "neutralization.slope")], outcome, value, neutralization.slope, factor_key=TRUE)
dat_long$outcome = ifelse(dat_long$outcome=="neutralization.slope", "Neutralization*' Slope'", dat_long$outcome)
# create proportional stacked bars
ntext <- dat_long[!is.na(dat_long$value),] %>% group_by(Dataset, outcome) %>% dplyr::summarise(n = n())
dat_long = merge(dat_long, ntext, by=c("Dataset", "outcome"))
mid=1
high=quantile(dat_long[!is.na(dat_long$value),]$value, 0.75)
low=quantile(dat_long[!is.na(dat_long$value),]$value, 0.25)

set.seed(20180430)
p2 = ggplot(dat_long, aes(x=Dataset, y=value, color=value)) + facet_grid(.~outcome, labeller = label_parsed) + 
  geom_jitter(size=3, width = 0.25) + xlab("")  +
  scale_y_continuous(limits=c(-2,3.2)) +
  geom_boxplot(outlier.colour=NA, fill="gray70",alpha=0.2) + 
  scale_color_gradient2(midpoint=mid, low="red", mid="gray90", high="blue", guide = "colourbar", space="Lab") + 
  theme(legend.position="top", legend.title = element_blank(), strip.text = element_text(face="bold", size=18), axis.text.x = element_text(size=17), axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.line.y = element_blank(), plot.margin = unit( c(0,4,0,0) , "mm" )) + 
  ylab("Value") + geom_text(aes(label = paste0("n = ", n), y = 2.8), size=7, vjust = 1.5, color = "black")

# Only Binary outcomes
dat_long = gather(dat[,c("Dataset", "ic50.censored", "binding.dichotomous.sens.resis")], outcome, value, ic50.censored:binding.dichotomous.sens.resis, factor_key=TRUE) 
dat_long$outcome = ifelse(dat_long$outcome=="ic50.censored", "IC[50]*' Censored'", 
                          ifelse(dat_long$outcome=="binding.dichotomous.sens.resis", "Sensitive/Resistant*' Only'", dat_long$outcome))
# create proportional stacked bars
proportion <- dat_long %>%
  group_by(Dataset, outcome, value) %>%
  dplyr::summarise(n = n()) %>% dplyr::mutate(freq = n * 100/ sum(n)) 

proportion$value = ifelse(is.na(proportion$value), as.integer(2), proportion$value)
proportion$freqPlace = ifelse(proportion$value==0 & proportion$outcome=="IC[50]*' Censored'", 84, 
                              ifelse(proportion$value==1, 100, 
                                     ifelse(proportion$value==0 & proportion$outcome=="Sensitive/Resistant*' Only'", 60, 83)))
proportion$value = factor(proportion$value, levels = c("1", "2", "0"))

set.seed(20180430)
p3 = ggplot(proportion, aes(factor(Dataset), freq, fill = value)) + facet_grid(~outcome, labeller = label_parsed) +
  geom_bar(stat = "identity", color = "grey40") +
  scale_fill_manual(values = c("red", "gray50", "blue"), labels=c("Resistant", "NA", "Sensitive")) +
  labs(fill = "value") +
  geom_text(aes(label = paste0(round(freq, 2), "%"), y = freqPlace), size=7, vjust = 1.5, color = "white") +
  geom_text(aes(label = paste0("n = ", n), y = freqPlace-7), size=7, vjust = 1.5, color = "white") + 
  ylab("Percentage") + xlab("") + 
  theme(legend.position="top", legend.title = element_blank(), legend.text = element_text(size=18), strip.text = element_text(face="bold", size=18), axis.text.y = element_text(size=17), axis.title.y = element_text(size=18), axis.text.x = element_text(size=17)) + 
  scale_y_continuous(labels = function(x) paste0(x, "%"))

pdf("figures/distribution of outcomes.pdf", width = 20, height=6)
plot_grid(p3, p1, p2, labels=c('', '', ''), align="h", rel_widths=c(1,1,0.5), nrow=1)
grid.text("Resistant", x = unit(0.525, "npc"), y = unit(c(0.95), "npc"), gp=gpar(fontsize=18, col="black", fontface="bold"))
grid.text("Sensitive", x = unit(0.415, "npc"), y = unit(c(0.95), "npc"), gp=gpar(fontsize=18, col="black", fontface="bold"))
grid.text(".", x = unit(0.82, "npc"), y = unit(c(1.92), "npc"), gp=gpar(fontsize=1250, col="white", fontface="bold")) # Add white patch to hide colourbar
dev.off()

##############################################################################################################
# Figures for ic50_censored outcome
##############################################################################################################
# ic50_censored: cross-validation forest plots top 5 plus SL 
rm(fit)
# Load the cvma fit on data set 1 along with the dataset
load(file = "fit_cens_set1_v11_newest.RData")
first_row = create_forest_plots(fit, Y.cens, X, "dichotomous", "Dataset 1", "ic50.censored", "CV-AUC", "A", 0.5, 1)

rm(fit)
load(file = "fit_cens_set2_v11_newest.RData")
second_row = create_forest_plots(fit, Y2.cens, X2, "dichotomous", "Dataset 2", "ic50.censored", "CV-AUC", "B", 0.5, 1)

pdf("figures/ic50cens_forest_top5plusSL_crossVal.pdf", width = 20, height=15)
plot_grid(first_row, second_row, labels=c('', ''), ncol=1)
grid.text("Algorithm", x = unit(0.12, "npc"), y = unit(c(0.95,0.45), "npc"), gp=gpar(fontsize=25, col="black", fontface="bold"))
grid.text("Screen", x = unit(0.26, "npc"), y = unit(c(0.95,0.45), "npc"), gp=gpar(fontsize=25, col="black", fontface="bold"))
grid.text("AUC (95% CI)", x = unit(0.43, "npc"), y = unit(c(0.95,0.45), "npc"), gp=gpar(fontsize=25, col="black", fontface="bold"))
dev.off()

# ic50_censored: cross-validation CV-ROC plots AND predicted probability plots
rm(fit)
load(file = "fit_cens_set1_v11_newest.RData")
tbl_forPredProb_set1 = create_CVROC_plots("Dataset 1", fit, SL.library, Y.cens, X, "figures/ic50cens_CVROC_top3_crossVal.pdf")
tab = dataprep(fit)
p1 = predicted_Probability_plot_crossVal(as.data.table(tab), tbl_forPredProb_set1)

rm(fit)
load(file = "fit_cens_set2_v11_newest.RData")
tbl_forPredProb_set2 = create_CVROC_plots("Dataset 2", fit, SL.library, Y2.cens, X2, "figures/ic50cens_CVROC_top3_crossVal.pdf")
tab2 = dataprep(fit)
p2 = predicted_Probability_plot_crossVal(as.data.table(tab2), tbl_forPredProb_set2)

pdf("figures/ic50cens_predProb_top3_crossVal.pdf", width = 13, height=15)
require(cowplot)
plot_grid(p1, p2, labels = c("A", "B"), ncol = 1, align = 'v', label_size=30)
dev.off()

# ic50_censored: cross-validation Forest plots with all results 
rm(fit)
load(file = "fit_cens_set1_v11_newest.RData")
tab2 = dataprep(fit)
forestplot_allmodels_dichotomousOutcome(tab2, "Dataset 1", "figures/cens_set1_forestplot_CVAUCs_crossval.pdf", 0.3, 1)

rm(fit)
load(file = "fit_cens_set2_v11_newest.RData")
tab2 = dataprep(fit)
forestplot_allmodels_dichotomousOutcome(tab2, "Dataset 2", "figures/cens_set2_forestplot_CVAUCs_crossval.pdf", 0.3, 1)


########################################################################################################################################################################################################################
# Figures for binding.dichotomous.sensitivity.resistant outcome
##############################################################################################################
# binding.dichotomous.sensitivity.resistant: cross-validation forest plots top 5 plus SL
rm(fit)
# Load the cvma fit on data set 1 along with the dataset
load(file = "fit_sens.resis_set1_v11_newest.RData")
first_row = create_forest_plots(fit, Y.sens.resis, X, "dichotomous", "Dataset 1", "ic50.sens.resis", "CV-AUC", "A", 0.5, 1)

rm(fit)
load(file = "fit_sens.resis_set2_v11_newest.RData")
second_row = create_forest_plots(fit, Y2.sens.resis, X2, "dichotomous", "Dataset 2", "ic50.sens.resis", "CV-AUC", "B", 0.5, 1)

require(cowplot)
pdf("figures/ic50sens.resis_forest_top5plusSL_crossVal.pdf", width = 20, height=15)
plot_grid(first_row, second_row, labels=c('', ''), ncol=1)
grid.text("Algorithm", x = unit(0.12, "npc"), y = unit(c(0.95,0.45), "npc"), gp=gpar(fontsize=25, col="black", fontface="bold"))
grid.text("Screen", x = unit(0.26, "npc"), y = unit(c(0.95,0.45), "npc"), gp=gpar(fontsize=25, col="black", fontface="bold"))
grid.text("AUC (95% CI)", x = unit(0.43, "npc"), y = unit(c(0.95,0.45), "npc"), gp=gpar(fontsize=25, col="black", fontface="bold"))
dev.off()

# binding.dichotomous.sensitivity.resistant: CV-ROC plots
rm(fit)
load(file = "fit_sens.resis_set1_v11_newest.RData")
tbl_forPredProb_set1 = create_CVROC_plots("Dataset 1", fit, SL.library, Y.sens.resis, X, "figures/ic50sens.resis_CVROC_top3_crossVal.pdf")
tab = dataprep(fit)
p1 = predicted_Probability_plot_crossVal(as.data.table(tab), tbl_forPredProb_set1)

rm(fit)
load(file = "fit_sens.resis_set2_v11_newest.RData")
tbl_forPredProb_set2 = create_CVROC_plots("Dataset 2", fit, SL.library, Y2.sens.resis, X2, "figures/ic50sens.resis_CVROC_top3_crossVal.pdf")
tab2 = dataprep(fit)
p2 = predicted_Probability_plot_crossVal(as.data.table(tab2), tbl_forPredProb_set2)

pdf("figures/ic50sens.resis_predProb_top3_crossVal.pdf", width = 13, height=15)
require(cowplot)
plot_grid(p1, p2, labels = c("A", "B"), ncol = 1, align = 'v', label_size=30)
dev.off()

# binding.dichotomous.sensitivity.resistant: cross-validation Forest plots with all results 
rm(fit)
load(file = "fit_sens.resis_set1_v11_newest.RData")
tab2 = dataprep(fit)
forestplot_allmodels_dichotomousOutcome(tab2, "Dataset 1", "figures/sens.resis_set1_forestplot_CVAUCs_crossval.pdf", 0.3, 1)

rm(fit)
load(file = "fit_sens.resis_set2_v11_newest.RData")
tab2 = dataprep(fit)
forestplot_allmodels_dichotomousOutcome(tab2, "Dataset 2", "figures/sens.resis_set2_forestplot_CVAUCs_crossval.pdf", 0.3, 1)

############################################################################################################
# Figures for IC50 outcome
############################################################################################################
# IC50: correlation plots
rm(fit)
load(file = "fit_ic50_set1_v11_newest.RData")
create_corplots_CV_Validate(fit, Y, X, "figures/ic50_corPlot.pdf", "Dataset 1", "CV") # Get set1 cv predictions
create_corplots_CV_Validate(fit, Y2, X2, "figures/ic50_corPlot.pdf", "Dataset 1", "Validation") # Get set1 validation predictions
rm(fit)
load(file = "fit_ic50_set2_v11_newest.RData")
create_corplots_CV_Validate(fit, Y2, X2, "figures/ic50_corPlot.pdf", "Dataset 2", "CV") # Get set2 cv predictions
create_corplots_CV_Validate(fit, Y, X, "figures/ic50_corPlot.pdf", "Dataset 2", "Validation") # Get set2 validation predictions

# IC50: cross-validation forest plots top 5 plus SL 
rm(fit)
load(file = "fit_ic50_set1_v11_newest.RData")
first_row = create_forest_plots(fit, Y, X, "continuous", "Dataset 1", "ic50", "CV-R2", "A", 0, 1)

rm(fit)
load(file = "fit_ic50_set2_v11_newest.RData")
second_row = create_forest_plots(fit, Y2, X2, "continuous", "Dataset 2", "ic50", "CV-R2", "B", 0, 1)

require(cowplot)
pdf("figures/ic50_forest_top5plusSL_crossVal.pdf", width = 20, height=15)
plot_grid(first_row, second_row, labels=c('', ''), ncol=1)
grid.text("Algorithm", x = unit(0.12, "npc"), y = unit(c(0.95,0.45), "npc"), gp=gpar(fontsize=25, col="black", fontface="bold"))
grid.text("Screen", x = unit(0.26, "npc"), y = unit(c(0.95,0.45), "npc"), gp=gpar(fontsize=25, col="black", fontface="bold"))
grid.text("R2 (95% CI)", x = unit(0.43, "npc"), y = unit(c(0.95,0.45), "npc"), gp=gpar(fontsize=25, col="black", fontface="bold"))
dev.off()

# IC50: cross-validation Forest plots with all results 
rm(fit)
load(file = "fit_ic50_set1_v11_newest.RData")
tab2 = dataprep(fit)
forestplot_allmodels_continuousOutcome(tab2, "Dataset 1", "figures/ic50_set1_forestplot_CVAUCs_crossval.pdf", -0.2, 0.6)

rm(fit)
load(file = "fit_ic50_set2_v11_newest.RData")
tab2 = dataprep(fit)
forestplot_allmodels_continuousOutcome(tab2, "Dataset 2", "figures/ic50_set2_forestplot_CVAUCs_crossval.pdf", -0.2, 0.6)

############################################################################################################
# Figures for IC80 outcome
############################################################################################################
# IC80: correlation plots
rm(fit)
load(file = "fit_ic80_set1_v11_newest.RData")
create_corplots_CV_Validate(fit, Y.80, X, "figures/ic80_corPlot.pdf", "Dataset 1", "CV") # Get set1 cv predictions
create_corplots_CV_Validate(fit, Y2.80, X2, "figures/ic80_corPlot.pdf", "Dataset 1", "Validation") # Get set1 validation predictions
rm(fit)
load(file = "fit_ic80_set2_v11_newest.RData")
create_corplots_CV_Validate(fit, Y2.80, X2, "figures/ic80_corPlot.pdf", "Dataset 2", "CV") # Get set2 cv predictions
create_corplots_CV_Validate(fit, Y.80, X, "figures/ic80_corPlot.pdf", "Dataset 2", "Validation") # Get set2 validation predictions

# IC80: cross-validation forest plots top 5 plus SL 
rm(fit)
load(file = "fit_ic80_set1_v11_newest.RData")
first_row = create_forest_plots(fit, Y.80, X, "continuous", "Dataset 1", "ic80", "CV-R2", "A", 0, 1)

rm(fit)
load(file = "fit_ic80_set2_v11_newest.RData")
second_row = create_forest_plots(fit, Y2.80, X2, "continuous", "Dataset 2", "ic80", "CV-R2", "B", 0, 1)

require(cowplot)
pdf("figures/ic80_forest_top5plusSL_crossVal.pdf", width = 20, height=15)
plot_grid(first_row, second_row, labels=c('', ''), ncol=1)
grid.text("Algorithm", x = unit(0.12, "npc"), y = unit(c(0.95,0.45), "npc"), gp=gpar(fontsize=25, col="black", fontface="bold"))
grid.text("Screen", x = unit(0.26, "npc"), y = unit(c(0.95,0.45), "npc"), gp=gpar(fontsize=25, col="black", fontface="bold"))
grid.text("R2 (95% CI)", x = unit(0.43, "npc"), y = unit(c(0.95,0.45), "npc"), gp=gpar(fontsize=25, col="black", fontface="bold"))
dev.off()

# IC80: cross-validation Forest plots with all results 
rm(fit)
load(file = "fit_ic80_set1_v11_newest.RData")
tab2 = dataprep(fit)
forestplot_allmodels_continuousOutcome(tab2, "Dataset 1", "figures/ic80_set1_forestplot_CVAUCs_crossval.pdf", -0.2, 0.6)

rm(fit)
load(file = "fit_ic80_set2_v11_newest.RData")
tab2 = dataprep(fit)
forestplot_allmodels_continuousOutcome(tab2, "Dataset 2", "figures/ic80_set2_forestplot_CVAUCs_crossval.pdf", -0.2, 0.6)


############################################################################################################
# Figures for Slope outcome
############################################################################################################
# Slope: correlation plots
rm(fit)
load(file = "fit_slope_mod_set1_v11_newest.RData")
create_corplots_CV_Validate(fit, Y.slope, X, "figures/slope_corPlot.pdf", "Dataset 1", "CV") # Get set1 cv predictions
create_corplots_CV_Validate(fit, Y2.slope, X2, "figures/slope_corPlot.pdf", "Dataset 1", "Validation") # Get set1 validation predictions
rm(fit)
load(file = "fit_slope_mod_set2_v11_newest.RData")
create_corplots_CV_Validate(fit, Y2.slope, X2, "figures/slope_corPlot.pdf", "Dataset 2", "CV") # Get set2 cv predictions
create_corplots_CV_Validate(fit, Y.slope, X, "figures/slope_corPlot.pdf", "Dataset 2", "Validation") # Get set2 validation predictions

# Slope: cross-validation forest plots top 5 plus SL 
rm(fit)
load(file = "fit_slope_mod_set1_v11_newest.RData")
first_row = create_forest_plots(fit, Y.slope, X, "continuous", "Dataset 1", "slope", "CV-R2", "A", 0, 1)

rm(fit)
load(file = "fit_slope_mod_set2_v11_newest.RData")
second_row = create_forest_plots(fit, Y2.slope, X2, "continuous", "Dataset 2", "slope", "CV-R2", "B", 0, 1)

require(cowplot)
pdf("figures/slope_forest_top5plusSL_crossVal.pdf", width = 20, height=15)
plot_grid(first_row, second_row, labels=c('', ''), ncol=1)
grid.text("Algorithm", x = unit(0.12, "npc"), y = unit(c(0.95,0.45), "npc"), gp=gpar(fontsize=25, col="black", fontface="bold"))
grid.text("Screen", x = unit(0.26, "npc"), y = unit(c(0.95,0.45), "npc"), gp=gpar(fontsize=25, col="black", fontface="bold"))
grid.text("R2 (95% CI)", x = unit(0.43, "npc"), y = unit(c(0.95,0.45), "npc"), gp=gpar(fontsize=25, col="black", fontface="bold"))
dev.off()

# Slope: cross-validation Forest plots with all results 
rm(fit)
load(file = "fit_slope_mod_set1_v11_newest.RData")
tab2 = dataprep(fit)
forestplot_allmodels_continuousOutcome(tab2, "Dataset 1", "figures/slope_set1_forestplot_CVAUCs_crossval.pdf", -0.2, 0.6)

rm(fit)
load(file = "fit_slope_mod_set2_v11_newest.RData")
tab2 = dataprep(fit)
forestplot_allmodels_continuousOutcome(tab2, "Dataset 2", "figures/slope_set2_forestplot_CVAUCs_crossval.pdf", -0.2, 0.6)


#################################################################################################################
#TABLES
#################################################################################################################
##############################################################################################################
# Create Table for Screens and Algorithms 
##############################################################################################################
# Load the cvma fit on data set 1
rm(fit)
load(file = "fit_cens_set1_v11_newest.RData")

# Create table with Input Feature Sets
scr = summary(fit, "learners")$cens %>% mutate(screen_algo=gsub(".skinny|screen.", "", SL_wrap)) %>% 
  mutate(screens=sapply(strsplit(screen_algo, "_"), `[`, 1)) %>%
  select(screens) %>% arrange(screens)
allvarsets <- read.csv(file = "all_variable_sets.csv", stringsAsFactors = FALSE)
scr <- merge(scr, allvarsets, by.x="screens", by.y="Variable.Set.Name", all.x=TRUE, sort=F)
scr = scr[!duplicated(scr$Variables) & !scr$screens %in% c("all", "SL.mean"),]
sortingorder = c("geog", "geog.AAchVRC01", "geog.AAchCD4bs", "geog.AAchESA", "geog.AAchGLYCO", "geog.AAchCOVAR", "geog.AAchPNGS", "geog.AAchGlyGP160",
                 "geog.st", "geog.sequonCt", "geog.geom", "geog.cys", "geog.sbulk", "geog.corP", "geog.glmnet")
scr = scr %>% dplyr::slice(match(sortingorder, screens))
rownames(scr) <- seq(length=nrow(scr)) 
scr$Variable.Set.Name = paste0(rownames(scr), ": ", scr$screens)
scr = scr[,c("Variable.Set.Name", "Variables")]
write.csv(scr, file="tables/screens.csv")

# Create table of Learning Algorithms used for SL
alg = summary(fit, "learners")$cens %>% mutate(screen_algo=gsub(".skinny|screen.", "", SL_wrap)) %>% 
  mutate(Algorithm.Type=sapply(strsplit(screen_algo, "_"), `[`, 2)) %>%
  select(Algorithm.Type) %>% unique() %>% arrange(Algorithm.Type)
alg$Algorithm.Type.Input.Variable.Sets.Screens = "All"
alg$Algorithm.Type = ifelse(is.na(alg$Algorithm.Type), "SL.mean", alg$Algorithm.Type)
alg$Input.Variable.Sets = ifelse(alg$Algorithm.Type %in% c("SL.naivebayes","SL.glmnet","SL.randomForest","SL.stumpboost"), "1,2,3,4,5,6,7,8,9,10,11,12,13,14,15",
                                 ifelse(alg$Algorithm.Type=="SL.mean", "None", "1,9,10,11,12,14,15"))
alg$Algorithm.Type.Input.Variable.Sets.Screens = ifelse(alg$Algorithm.Type=="SL.mean", "None (Base model: No covariates)", alg$Algorithm.Type.Input.Variable.Sets.Screens)

# Determine order to show algorithms
sortingorder <- c("SL.randomForest","SL.glmnet", "SL.stumpboost", "SL.naivebayes", "SL.glm","SL.step", "SL.step.interaction","SL.mean")
alg = alg %>% dplyr::slice(match(sortingorder, Algorithm.Type))
alg = alg[,c("Algorithm.Type", "Input.Variable.Sets", "Algorithm.Type.Input.Variable.Sets.Screens")]
write.csv(alg, file="tables/algos.csv")

##############################################################################################################
# Tables for IC50 outcome for Dataset 1
##############################################################################################################
# Load the cvma fit on data set 1 
options(scipen=999)
rm(fit)
load(file = "fit_ic50_set1_v11_newest.RData")

# Data prep for in-sample cross-validation
tab = dataprep(fit)

# Data prep for out of sample validation
tab2 = dataprep_outValidate(fit, Y2, X2, "continuous")

# Keep only Top 10 models with the best CV-R2 and Validated-R2 estimate
getTOP10models(tab, tab2, "continuous", "tables/ic50_set1_ModelsTop10.csv")

# Get coefficients>0.02 for each algorithm used in SL for dataset1. Coeffs indicate relative weighting of predicted probabilities of neutralization from each algorithm.
getAlgos_withCoeffmorethan2percent(fit, "tables/ic50_dataset1_SL_weights.csv")

##############################################################################################################
# Tables for IC50 outcome for Dataset 2
##############################################################################################################
# Load the cvma fit on data set 2
rm(fit)
load(file = "fit_ic50_set2_v11_newest.RData")

# Data prep for in-sample cross-validation
tab = dataprep(fit)

# Data prep for out of sample validation
tab2 = dataprep_outValidate(fit, Y, X, "continuous")

# Keep only Top 10 models with the best CV-R2 and Validated-R2 estimate
getTOP10models(tab, tab2, "continuous", "tables/ic50_set2_ModelsTop10.csv")

# Get coefficients>0.02 for each algorithm used in SL for dataset1. Coeffs indicate relative weighting of predicted probabilities of neutralization from each algorithm.
getAlgos_withCoeffmorethan2percent(fit, "tables/ic50_dataset2_SL_weights.csv")


##############################################################################################################
# Tables for IC80 outcome for Dataset 1
##############################################################################################################
# Load the cvma fit on data set 1 
rm(fit)
load(file = "fit_ic80_set1_v11_newest.RData")

# Data prep for in-sample cross-validation
tab = dataprep(fit)

# Data prep for out of sample validation
tab2 = dataprep_outValidate(fit, Y2.80, X2, "continuous")

# Keep only Top 10 models with the best CV-R2 and Validated-R2 estimate
getTOP10models(tab, tab2, "continuous", "tables/ic80_set1_ModelsTop10.csv")

# Get coefficients>0.02 for each algorithm used in SL for dataset1. Coeffs indicate relative weighting of predicted probabilities of neutralization from each algorithm.
getAlgos_withCoeffmorethan2percent(fit, "tables/ic80_dataset1_SL_weights.csv")

##############################################################################################################
# Tables for IC80 outcome for Dataset 2
##############################################################################################################
# Load the cvma fit on data set 2
rm(fit)
load(file = "fit_ic80_set2_v11_newest.RData")

# Data prep for in-sample cross-validation
tab = dataprep(fit)

# Data prep for out of sample validation
tab2 = dataprep_outValidate(fit, Y.80, X, "continuous")

# Keep only Top 10 models with the best CV-R2 and Validated-R2 estimate
getTOP10models(tab, tab2, "continuous", "tables/ic80_set2_ModelsTop10.csv")

# Get coefficients>0.02 for each algorithm used in SL for dataset1. Coeffs indicate relative weighting of predicted probabilities of neutralization from each algorithm.
getAlgos_withCoeffmorethan2percent(fit, "tables/ic80_dataset2_SL_weights.csv")


##############################################################################################################
# Tables for Slope outcome for Dataset 1
##############################################################################################################
# Load the cvma fit on data set 1 
rm(fit)
load(file = "fit_slope_mod_set1_v11_newest.RData")

# Data prep for in-sample cross-validation
tab = dataprep(fit)

# Data prep for out of sample validation
tab2 = dataprep_outValidate(fit, Y2.slope, X2, "continuous")

# Keep only Top 10 models with the best CV-R2 and Validated-R2 estimate
getTOP10models(tab, tab2, "continuous", "tables/slope_set1_ModelsTop10.csv")

# Get coefficients>0.02 for each algorithm used in SL for dataset1. Coeffs indicate relative weighting of predicted probabilities of neutralization from each algorithm.
getAlgos_withCoeffmorethan2percent(fit, "tables/slope_dataset1_SL_weights.csv")

##############################################################################################################
# Tables for Slope outcome for Dataset 2
##############################################################################################################
# Load the cvma fit on data set 2
rm(fit)
load(file = "fit_slope_mod_set2_v11_newest.RData")

# Data prep for in-sample cross-validation
tab = dataprep(fit)

# Data prep for out of sample validation
tab2 = dataprep_outValidate(fit, Y.slope, X, "continuous")

# Keep only Top 10 models with the best CV-R2 and Validated-R2 estimate
getTOP10models(tab, tab2, "continuous", "tables/slope_set2_ModelsTop10.csv")

# Get coefficients>0.02 for each algorithm used in SL for dataset1. Coeffs indicate relative weighting of predicted probabilities of neutralization from each algorithm.
getAlgos_withCoeffmorethan2percent(fit, "tables/slope_dataset2_SL_weights.csv")


##############################################################################################################
# Tables for ic50.censored outcome for Dataset 1 
##############################################################################################################
# Load the cvma fit on data set 1 
rm(fit)
load(file = "fit_cens_set1_v11_newest.RData")

# Data prep for in-sample cross-validation
tab = dataprep(fit)

# Data prep for out of sample validation
tab2 = dataprep_outValidate(fit, Y2.cens, X2, "dichotomous")

# Keep only Top 10 models with the best CV-R2 and Validated-R2 estimate
getTOP10models(tab, tab2, "dichotomous", "tables/cens_set1_ModelsTop10.csv")

# Get coefficients>0.02 for each algorithm used in SL for dataset1. Coeffs indicate relative weighting of predicted probabilities of neutralization from each algorithm.
getAlgos_withCoeffmorethan2percent(fit, "tables/cens_dataset1_SL_weights.csv")

##############################################################################################################
# Tables for ic50.censored outcome for Dataset 2
##############################################################################################################
# Load the cvma fit on data set 2
rm(fit)
load(file = "fit_cens_set2_v11_newest.RData")

# Data prep for in-sample cross-validation
tab = dataprep(fit)

# Data prep for out of sample validation
tab2 = dataprep_outValidate(fit, Y.cens, X, "dichotomous")

# Keep only Top 10 models with the best CV-R2 and Validated-R2 estimate
getTOP10models(tab, tab2, "dichotomous", "tables/cens_set2_ModelsTop10.csv")

# Get coefficients>0.02 for each algorithm used in SL for dataset1. Coeffs indicate relative weighting of predicted probabilities of neutralization from each algorithm.
getAlgos_withCoeffmorethan2percent(fit, "tables/cens_dataset2_SL_weights.csv")

##############################################################################################################
# Tables for binding.dichotomous.sens.resis outcome for Dataset 1 
##############################################################################################################
# Load the cvma fit on data set 1 
rm(fit)
load(file = "fit_sens.resis_set1_v11_newest.RData")

# Data prep for in-sample cross-validation
tab = dataprep(fit)

# Data prep for out of sample validation
tab2 = dataprep_outValidate(fit, Y2.sens.resis, X2, "dichotomous")

# Keep only Top 10 models with the best CV-R2 and Validated-R2 estimate
getTOP10models(tab, tab2, "dichotomous", "tables/sens.resis_set1_ModelsTop10.csv")

# Get coefficients>0.02 for each algorithm used in SL for dataset1. Coeffs indicate relative weighting of predicted probabilities of neutralization from each algorithm.
getAlgos_withCoeffmorethan2percent(fit, "tables/sensresis_dataset1_SL_weights.csv")

##############################################################################################################
# Tables for binding.dichotomous.sens.resis outcome for Dataset 2
##############################################################################################################
# Load the cvma fit on data set 2
rm(fit)
load(file = "fit_sens.resis_set2_v11_newest.RData")

# Data prep for in-sample cross-validation
tab = dataprep(fit)

# Data prep for out of sample validation
tab2 = dataprep_outValidate(fit, Y.sens.resis, X, "dichotomous")

# Keep only Top 10 models with the best CV-R2 and Validated-R2 estimate
getTOP10models(tab, tab2, "dichotomous", "tables/sens.resis_set2_ModelsTop10.csv")

# Get coefficients>0.02 for each algorithm used in SL for dataset1. Coeffs indicate relative weighting of predicted probabilities of neutralization from each algorithm.
getAlgos_withCoeffmorethan2percent(fit, "tables/sensresis_dataset2_SL_weights.csv")

#################################################################################################################




