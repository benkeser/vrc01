# ---------------------------------------------------------------------------- #
# This code function creates correlation plots for the continuous outcomes
# ---------------------------------------------------------------------------- #

create_corplots_CV_Validate = function(fit, outcome, covarX, plotFile, dataset, validationType){
  # Get insample predictions
  covarX = covarX[!is.na(outcome),]
  outcome = outcome[!is.na(outcome)]
  if(validationType=="Validation"){
    new_pred <- predict(fit, newdata = covarX)
  }
  
  if(validationType=="CV"){
    covarX = covarX[!is.na(outcome),]
    outcome = outcome[!is.na(outcome)]
    
    # make a list of validation data 
    validDataList <- sapply(10:1, function(f){ return(covarX[fit$folds == f,,drop=FALSE]) }, simplify = FALSE)
    # call predict with inner = TRUE in a loop over this list; this might be slow because predict.cvma with inner = TRUE is currently predicting using ALL super learners
    predict_list <- lapply(validDataList, function(valid){
      predict(fit, newdata = valid, outer = FALSE)
    })
    
    # put predictions into lists for plotting
    PredV_SLlist <- list()
    Ylist <- list()
    for(i in c(1:10)){
      PredV_SLlist[[i]] <- predict_list[[i]][[i]]$sl_pred # superlearner
      Ylist[[i]] <- outcome[fit$folds == (11-i)]
    }
    
    tbl_forPredProb_wide = as.data.frame(cbind(unlist(Ylist), unlist(PredV_SLlist))) 
    colnames(tbl_forPredProb_wide)[which(names(tbl_forPredProb_wide) == "V1")] <- "Y"
    colnames(tbl_forPredProb_wide)[which(names(tbl_forPredProb_wide) == "V2")] <- "SuperLearner"
  }
  
  
  if(dataset=="Dataset 1" & validationType=="CV"){
    pdf(plotFile, width = 20, height=20)
    op<-par(no.readonly=TRUE) # save the default settings 
    par(mfrow=c(2,2),cex.lab=2.8,cex.axis=2.2, mgp = c(4, 1.5, 0))
    par(mai=c(0.9,1.3,0.5,0.5))
    plot(tbl_forPredProb_wide$SuperLearner, tbl_forPredProb_wide$Y, xlab="SL predicted Y values",ylab="Y values", main="",cex=2, xlim=c(-2,2), ylim=c(-2,2))
    abline(0, 1, col='darkblue')
    text(x=1,y=-1.75,cex=2.5,paste("rho = ",round(cor(tbl_forPredProb_wide$SuperLearner, tbl_forPredProb_wide$Y, method="spearman"),3)))
    text(x = -2.6, y = 2.2, labels = "A", xpd = NA, cex=6, font=2)
  }
  
  if(dataset=="Dataset 1" & validationType=="Validation"){
    par(mai=c(0.9,1.3,0.5,0.5))
    plot(new_pred$y_weight$sl_pred, outcome, xlab="SL predicted Y values",ylab="Y values", main="",cex=2, xlim=c(-2,2), ylim=c(-2,2))
    abline(0, 1, col='darkblue')
    text(x=1,y=-1.75,cex=2.5,paste("rho = ",round(cor(new_pred$y_weight$sl_pred, outcome, method="spearman"),3)))
    text(x = -2.6, y = 2.2, labels = "B", xpd = NA, cex=6, font=2)
  }
  
  if(dataset=="Dataset 2" & validationType=="CV"){
    par(mai=c(0.9,1.3,0.5,0.5))
    plot(tbl_forPredProb_wide$SuperLearner, tbl_forPredProb_wide$Y, xlab="SL predicted Y values",ylab="Y values", main="",cex=2, xlim=c(-2,2), ylim=c(-2,2))
    abline(0, 1, col='darkblue')
    text(x=1,y=-1.75,cex=2.5,paste("rho = ",round(cor(tbl_forPredProb_wide$SuperLearner, tbl_forPredProb_wide$Y, method="spearman"),3)))
    text(x = -2.6, y = 2.2, labels = "C", xpd = NA, cex=6, font=2)
  }
  
  if(dataset=="Dataset 2" & validationType=="Validation"){
    par(mai=c(0.9,1.3,0.5,0.5))
    plot(new_pred$y_weight$sl_pred, outcome, xlab="SL predicted Y values",ylab="Y values", main="",cex=2, xlim=c(-2,2), ylim=c(-2,2))
    abline(0, 1, col='darkblue')
    text(x=1,y=-1.75,cex=2.5,paste("rho = ",round(cor(new_pred$y_weight$sl_pred, outcome, method="spearman"),3)))
    text(x = -2.6, y = 2.2, labels = "D", xpd = NA, cex=6, font=2)
    dev.off()
  }
}




# ---------------------------------------------------------------------------- #
# This code function creates CV-ROC plots for the dichotomous outcomes
# ---------------------------------------------------------------------------- #

create_CVROC_plots = function(dataset, fit, SL.library, outcome, covarX, plotFile, ...){
  ForestPlotMatrix <- matrix(NA,ncol=4,nrow=(length(SL.library)))
  ForestPlotMatrix[1:length(SL.library),] = as.matrix(summary(fit, "learners")[[1]][1:4])
  colnames(ForestPlotMatrix)<-c("Screen_Algorithm","CVAUC","Lower","Upper")
  ForestPlotDataset <- data.frame((ForestPlotMatrix), stringsAsFactors = FALSE)
  ForestPlotDataset$CVAUC = as.numeric(ForestPlotDataset$CVAUC)
  ForestPlotDataset$Lower = as.numeric(ForestPlotDataset$Lower)
  ForestPlotDataset$Upper = as.numeric(ForestPlotDataset$Upper)
  
  covarX = covarX[!is.na(outcome),]
  outcome = outcome[!is.na(outcome)]
  
  # make a list of validation data 
  validDataList <- sapply(10:1, function(f){ return(covarX[fit$folds == f,,drop=FALSE]) }, simplify = FALSE)
  # call predict with inner = TRUE in a loop over this list; this might be slow because predict.cvma with inner = TRUE is currently predicting using ALL super learners
  predict_list <- lapply(validDataList, function(valid){
    predict(fit, newdata = valid, outer = FALSE)
  })
  
  # put predictions into lists for plotting
  PredVbestlist <- PredV2list <- PredV3list <- PredV4list <- PredV5list <- PredV_SLlist <- list()
  Ylist <- list()
  for(i in c(1:10)){
    PredVbestlist[[i]] <- predict_list[[i]][[i]]$learner_pred[,paste(ForestPlotDataset$Screen_Algorithm[1])] # best performing candidate algorithm
    PredV2list[[i]] <- predict_list[[i]][[i]]$learner_pred[,paste(ForestPlotDataset$Screen_Algorithm[2])] # second best performing algorithm
    PredV3list[[i]] <- predict_list[[i]][[i]]$learner_pred[,paste(ForestPlotDataset$Screen_Algorithm[3])]
    PredV_SLlist[[i]] <- predict_list[[i]][[i]]$sl_pred # superlearner
    Ylist[[i]] <- outcome[fit$folds == (11-i)]
  }
  
  tbl_forPredProb_wide = as.data.frame(cbind(unlist(Ylist), unlist(PredVbestlist), unlist(PredV2list), unlist(PredV3list), unlist(PredV_SLlist))) 
  colnames(tbl_forPredProb_wide)[which(names(tbl_forPredProb_wide) == "V1")] <- "Y"
  tbl_forPredProb_wide$Sensitivity = ifelse(tbl_forPredProb_wide$Y==1, "Resistant", "Sensitive")
  tbl_forPredProb_wide = tbl_forPredProb_wide %>% dplyr::select(Sensitivity, V2, V3, V4, V5)
  colnames(tbl_forPredProb_wide)[which(names(tbl_forPredProb_wide) == "V2")] <- ForestPlotDataset$Screen_Algorithm[1]
  colnames(tbl_forPredProb_wide)[which(names(tbl_forPredProb_wide) == "V3")] <- ForestPlotDataset$Screen_Algorithm[2]
  colnames(tbl_forPredProb_wide)[which(names(tbl_forPredProb_wide) == "V4")] <- ForestPlotDataset$Screen_Algorithm[3]
  colnames(tbl_forPredProb_wide)[which(names(tbl_forPredProb_wide) == "V5")] <- "SuperLearner"
  tbl_forPredProb = melt(tbl_forPredProb_wide, id.vars=c("Sensitivity"), variable.name="Model", value.name="y")
  tbl_forPredProb$Model = sub("_", "_\n", tbl_forPredProb$Model)
  
  ## Data Set 1 plot data
  predBest <- prediction(PredVbestlist,Ylist)
  perfBest <- performance(predBest,"tpr","fpr")
  pred2 <- prediction(PredV2list,Ylist)
  perf2 <- performance(pred2,"tpr","fpr")
  pred3 <- prediction(PredV3list,Ylist)
  perf3 <- performance(pred3,"tpr","fpr")

  # same for superlearner
  pred.sl <- prediction(PredV_SLlist,Ylist)
  perf.sl <- performance(pred.sl,"tpr","fpr")
  
  if(dataset=="Dataset 1"){
    pdf(plotFile, width = 20, height=12)
    op<-par(no.readonly=TRUE) #this is done to save the default settings 
    par(mfrow=c(1,2),cex.lab=2.2,cex.axis=2.2, mgp=c(4, 1.5, 0))
    par(mai=c(1.2,1.3,0.5,0.5))
    par(xpd = T, mar = par()$mar + c(0,0,14,0))
    plot(perfBest, lwd=3, avg="vertical",xlim=c(0,1),# main="ROC Curves for Best Vaccine Models", 
         col="orange",spread.estimate="stderror", legacy.axes = FALSE,show.spread.at= seq(0.09, 0.89, by=0.1),ylab="Cross-Validated True Positive Rate", cex=1.5) +
      plot(perf2, lwd=3, avg="vertical",xlim=c(0,1), col="red", legacy.axes = FALSE,add=T,spread.estimate="stderror")  +  #,spread.estimate="stderror")
      plot(perf3, lwd=3, avg="vertical",xlim=c(0,1), col="cyan", legacy.axes = FALSE,add=T,spread.estimate="stderror")  + 
      plot(perf.sl, lwd=3, avg="vertical",xlim=c(0,1), col="blue", legacy.axes = FALSE,add=T,spread.estimate="stderror") 
    legend(-0.04, 1.43,
           title=expression(bold("Model (CV-AUC)")),
           c(paste("Best Learner: ",paste(ForestPlotDataset$Screen_Algorithm[1])," (",round(ForestPlotDataset$CVAUC[1],3),")",sep=""),
             paste("2nd: ",paste(ForestPlotDataset$Screen_Algorithm[2])," (",round(ForestPlotDataset$CVAUC[2],3),")",sep=""),
             paste("3rd: ",paste(ForestPlotDataset$Screen_Algorithm[3])," (",round(ForestPlotDataset$CVAUC[3],3),")",sep=""),
             paste("screen.all_SuperLearner: (",round(fit$cv_assoc$cv_measure,3),")",sep="")),
           horiz=FALSE,
           lty=c(1,1,1,1),
           lwd=c(3,3,3,3),
           col=c("orange","red","green","blue"), cex=1.5, box.lty=0)
    text(x = -0.165, y = 1.45, labels = "A", xpd = NA, cex=4, font=2)
    par(xpd=F)
    abline(a=0, b=1,col="grey") 
    par(mar=c(5, 4, 4, 2) + 0.1)
  }
  
  if(dataset=="Dataset 2"){
    par(mai=c(1.2,1.3,0.5,0.5))
    par(xpd = T, mar = par()$mar + c(0,0,14,0))
    plot(perfBest, lwd=3, avg="vertical",xlim=c(0,1),# main="ROC Curves for Best Vaccine Models", 
         col="orange",spread.estimate="stderror", legacy.axes = FALSE,show.spread.at= seq(0.09, 0.89, by=0.1),ylab="Cross-Validated True Positive Rate") +
      plot(perf2, lwd=3, avg="vertical",xlim=c(0,1), col="red", legacy.axes = FALSE,add=T,spread.estimate="stderror")  +  #,spread.estimate="stderror")
      plot(perf3, lwd=3, avg="vertical",xlim=c(0,1), col="cyan", legacy.axes = FALSE,add=T,spread.estimate="stderror")  + 
      plot(perf.sl, lwd=3, avg="vertical",xlim=c(0,1), col="blue", legacy.axes = FALSE,add=T,spread.estimate="stderror") 
    legend(-0.04, 1.43,
           title=expression(bold("Model (CV-AUC)")),
           c(paste("Best Learner: ",paste(ForestPlotDataset$Screen_Algorithm[1])," (",round(ForestPlotDataset$CVAUC[1],3),")",sep=""),
             paste("2nd: ",paste(ForestPlotDataset$Screen_Algorithm[2])," (",round(ForestPlotDataset$CVAUC[2],3),")",sep=""),
             paste("3rd: ",paste(ForestPlotDataset$Screen_Algorithm[3])," (",round(ForestPlotDataset$CVAUC[3],3),")",sep=""),
             paste("screen.all_SuperLearner: (",round(fit$cv_assoc$cv_measure,3),")",sep="")),
           horiz=FALSE,
           lty=c(1,1,1,1),
           lwd=c(3,3,3,3),
           col=c("orange","red","green","blue"), cex=1.5, box.lty=0)
    text(x = -0.165, y = 1.45, labels = "B", xpd = NA, cex=4, font=2)
    par(xpd=F)
    abline(a=0, b=1,col="grey") 
    par(mar=c(5, 4, 4, 2) + 0.1)
    dev.off()
  }
  return(tbl_forPredProb)
}


# ---------------------------------------------------------------------------- #
# This code function creates cross-validation forest plots for top 5 models plus SL 
# ---------------------------------------------------------------------------- #
create_forest_plots = function(fit, outcome, covarX, outcome_type, dataset, outcomeName, ytitle, plotLabel, ymin, ymax, ...) {

  if(ytitle %in% c("CV-AUC", "CV-R2")){
    tab1 = as.data.table(dataprep(fit))
  }
  if(ytitle %in% c("Validated-AUC", "Validated-R2")){
    tab1 = as.data.table(dataprep_outValidate(fit, outcome, covarX, outcome_type))
  }
  tab1 = tab1[order(-tab1$cv_measure)]

  # Choose top 1:5 models
  if("SuperLearner" %in% tab1[1:5,]$algo)
    tab1_top5 <- rbind(tab1[1:6,])
  if(!"SuperLearner" %in% tab1[1:5,]$algo)
    tab1_top5 <- rbind(tab1[1:5,], tab1[tab1$algo=="SuperLearner"])
  
  tab1_top5$screen.algo = as.character(tab1_top5$screen.algo)
  tab1_top5 = tab1_top5[,.(screen.algo, screen, algo, cv_measure, ci_low, ci_high)]
  tab1_top5 = tab1_top5[order(tab1_top5$cv_measure),]
  tab1_top5$screen.algo = as.factor(tab1_top5$screen.algo)
  tab1_top5$dataset = dataset
  tab1_top5$cv = paste0(format(round(tab1_top5$cv_measure,3), nsmall=3), " (", format(round(tab1_top5$ci_low,3), nsmall=3), ", ", format(round(tab1_top5$ci_high,3), nsmall=3), ")")
  
  # Make this change to accomodate similar factor levels in case of slope outcomeName and dataset 2
  if(outcomeName=="ic80" & dataset=="Dataset 2"){
    tab1_top5$cv[1] = "0.143 (0.074, 0.205)"
  }
  
  if(outcomeName=="slope" & dataset=="Dataset 2"){
    tab1_top5$cv[2] = "0.032 (-0.059, 0.114)"
    tab1_top5$cv[4] = "0.055 (-0.064, 0.160)"
  }
  
  tab1_top5$cv = ordered(tab1_top5$cv, levels = tab1_top5$cv)
  
  dat = tab1_top5[,c("dataset","algo","screen","cv")]
  dat$var = 1
  dat = melt(dat, id.vars="var")
  if(dim(tab1_top5)[1]==4){
    dat$V0 = rep(c("A","B","C","D"))
    dat$V05 = rep(c(0,1,2,3),each=4)
  }
  if(dim(tab1_top5)[1]==5){
    dat$V0 = rep(c("A","B","C","D","E"))
    dat$V05 = rep(c(0,1,2,3),each=5)
  }
  if(dim(tab1_top5)[1]==6){
    dat$V0 = rep(c("A","B","C","D","E","F"))
    dat$V05 = rep(c(0,1,2,3),each=6)
  }
  dat$value = ifelse(dat$V05==0, "", dat$value)
  
  p1=ggplot(dat, aes(x = V05, y = V0, label = format(value, nsmall = 1))) +
    geom_text(size=7.5, hjust=0.9, vjust=0.5) +
    theme_bw() + 
    theme(legend.position="", 
          axis.line=element_blank(),
          axis.text=element_blank(),
          text = element_text(size=35),
          axis.title = element_blank(), 
          axis.ticks = element_blank(),
          plot.margin=unit(c(0,0,0,0),"cm"),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank()) 

  p2 = ggplot(tab1_top5, aes(x=cv, y=cv_measure, ymin=ci_low, ymax=ci_high)) +
    geom_pointrange(size=1, stat="identity", col="blue3") +
    geom_errorbar(width=0.2, size=1, stat = "identity", col="blue3") + 
    coord_flip(ylim=c(ymin, ymax)) +  
    xlab("") + 
    ylab(ytitle) +
    theme_bw() + 
    theme(legend.position="", 
          axis.text.y=element_blank(),
          text = element_text(size=35),
          axis.title = element_text(size=25), 
          axis.title.x = element_text(margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0)),
          panel.grid.major = element_line("gray80"),
          plot.margin=unit(c(2,0.5,0.5,0),"cm")) 

  require(cowplot)
  one_row = plot_grid(p1, p2, ncol=2, align="h", labels=plotLabel, label_size=54)
  one_row
  return(one_row)
}


# ---------------------------------------------------------------------------- #
# This code function retrieves out of sample validation scores from fit
# ---------------------------------------------------------------------------- #

dataprep_outValidate <- function(fit=fit, outcome, covarX, outcome_type, ...){
  covarX = covarX[!is.na(outcome),]
  outcome = outcome[!is.na(outcome)]
  
  # Get validations of individual learners
  pred <- predict(fit, newdata = covarX)
  learner_pred <- pred[[1]]$learner_pred
  tab = data.frame(screen.algo=character(), cv_measure=numeric(), ci_low=numeric(), ci_high=numeric(), stringsAsFactors=FALSE) # create empty dataframe
  sl_result = tab # empty dataframe for SL results
  for(j in c(1:length(learner_pred[1,]))){
    if(outcome_type=="continuous"){
      out <- cvma:::cv_risk_sl_r2(input = list(list(valid_folds=1, pred=learner_pred[,j] , Y=outcome)), sl_control = list(alpha = 0.05))
    }
    if(outcome_type=="dichotomous"){
      out <- cvma:::cv_risk_sl_auc(input = list(list(valid_folds=1, pred=learner_pred[,j] , Y=outcome)), sl_control = list(alpha = 0.05))
    }
    tab[j,] = c(attributes(learner_pred)$dimnames[[2]][j], out$cv_measure, out$ci_low, out$ci_high)
  }
  
  # Get validations for SL
  sl_pred <- pred[[1]]$sl_pred
  if(outcome_type=="continuous"){
    out <- cvma:::cv_risk_sl_r2(input = list(list(valid_folds=1, pred=sl_pred, Y=outcome)), sl_control = list(alpha = 0.05))
  }
  if(outcome_type=="dichotomous"){
    out <- cvma:::cv_risk_sl_auc(input = list(list(valid_folds=1, pred=sl_pred, Y=outcome)), sl_control = list(alpha = 0.05))
  }
  sl_result[1,] = c("SuperLearner", out$cv_measure, out$ci_low, out$ci_high)
  tab = rbind(tab, sl_result)
  tab$cv_measure = as.numeric(tab$cv_measure)
  tab$ci_low = as.numeric(tab$ci_low)
  tab$ci_high = as.numeric(tab$ci_high)
  
  tab <- tab %>% dplyr::mutate(screen.algo=gsub(".skinny|screen.", "", screen.algo)) %>% 
    dplyr::mutate(screen=sapply(strsplit(screen.algo, "_"), `[`, 1)) %>%
    dplyr::mutate(algo=sapply(strsplit(screen.algo, "_"), `[`, 2)) %>%
    select(screen.algo, screen, algo, cv_measure, ci_low, ci_high) %>%
    arrange(-cv_measure)
  tab$algo = ifelse(tab$screen.algo == "SuperLearner", "SuperLearner", tab$algo)
  tab$screen = ifelse(tab$screen.algo == "SuperLearner", NA, tab$screen)
  tab$algo = as.character(tab$algo)
  tab$screen = as.character(tab$screen)
  tab = tab[order(tab$screen),]
  tab$algo = ifelse(tab$screen=="SL.mean" & !is.na(tab$screen), "SL.mean", tab$algo)
  tab$screen = ifelse(is.na(tab$screen), "all", tab$screen)
  tab$screen = ifelse(tab$screen=="SL.mean", "none", tab$screen)
  tab$algo = factor(tab$algo, levels = c("SL.mean","SL.step.interaction","SL.step","SL.glm","SL.stumpboost","SL.naivebayes","SL.glmnet","SL.randomForest","SuperLearner"))
  tab$screen = factor(tab$screen, levels = c("none","all","geog.corP","geog.glmnet","geog.sbulk","geog.cys","geog.geom","geog.sequonCt",
                                             "geog.AAchGlyGP160","geog.AAchPNGS","geog.AAchCOVAR","geog.AAchGLYCO","geog.AAchESA",
                                             "geog.AAchCD4bs","geog.AAchVRC01","geog.st","geog"))
  tab$screen.algo = interaction(tab$screen, tab$algo)
  tab$col = ifelse(tab$screen=="geog", "blue", "black")

  return(tab)
}

# ---------------------------------------------------------------------------- #
# This code function extracts in-sample cross-validation scores from fit
# ---------------------------------------------------------------------------- #

dataprep <- function(fit=fit, ...){
  tab <- summary(fit, "learners")[[1]] %>% mutate(SL_wrap = as.character(SL_wrap))  %>% 
    dplyr::mutate(screen_algo=gsub(".skinny|screen.", "", SL_wrap)) %>% 
    dplyr::mutate(screen=sapply(strsplit(screen_algo, "_"), `[`, 1)) %>%
    dplyr::mutate(algo=sapply(strsplit(screen_algo, "_"), `[`, 2)) %>%
    select(screen, algo, cv_measure, ci_low, ci_high)
  tab[dim(tab)[1]+1,] = c(NA, NA, fit$cv_assoc$cv_measure, fit$cv_assoc$ci_low, fit$cv_assoc$ci_high) 
  
  tab$algo = ifelse(tab$screen=="SL.mean", "SL.mean", tab$algo)
  tab$screen = ifelse(tab$algo=="SL.mean", "none", tab$screen)
  
  tab$algo = ifelse(is.na(tab$algo), "SuperLearner", tab$algo)
  tab$screen = ifelse(tab$algo=="SuperLearner", "all", tab$screen)
  
  tab$algo = as.character(tab$algo)
  tab$screen = as.character(tab$screen)
  tab = tab[order(tab$screen),]
  
  tab$algo = factor(tab$algo, levels = c("SL.mean","SL.step.interaction","SL.step","SL.glm","SL.stumpboost","SL.naivebayes","SL.glmnet","SL.randomForest","SuperLearner"))
  tab$screen = factor(tab$screen, levels = c("none","all","geog.corP","geog.glmnet","geog.sbulk","geog.cys","geog.geom","geog.sequonCt",
                                             "geog.AAchGlyGP160","geog.AAchPNGS","geog.AAchCOVAR","geog.AAchGLYCO","geog.AAchESA",
                                             "geog.AAchCD4bs","geog.AAchVRC01","geog.st","geog"))
  
  tab$screen.algo = interaction(tab$screen, tab$algo)
  tab$col = ifelse(tab$screen=="geog", "blue", "black")
  return(tab)
}

# ---------------------------------------------------------------------------- #
# This code function creates forest plot for continuous Outcomes showing all algorithm screen combinations.
# ---------------------------------------------------------------------------- #

forestplot_allmodels_continuousOutcome <- function(data, datasetName, plotFile, plotymin, plotymax, ...){
  
  data$dat = datasetName
  p = ggplot(data, aes(x=screen.algo, y=cv_measure, ymin=ci_low, ymax=ci_high)) +
    geom_pointrange(size=0, aes(colour = col)) +
    geom_errorbar(width=0, size=0, aes(colour = col)) + 
    coord_flip(ylim=c(plotymin, plotymax)) +  # flip coordinates (puts labels on y axis)
    xlab("Screen") + 
    theme_bw() + 
    scale_y_continuous(breaks=seq(plotymin, plotymax, by=0.1), labels=seq(plotymin, plotymax, by=0.1)) 
  
  xticks = ggplot_build(p)$layout$panel_ranges[[1]]$y.labels
  xticks = sapply(strsplit(xticks,".SL"), `[`, 1)
  xticks = ifelse(xticks=="all.SuperLearner", "all", xticks)
  
  p = p + geom_rect(aes(xmin = -Inf, xmax = 1.5, ymin = -Inf, ymax = Inf), fill = "gray43", alpha = 0.006) + 
    geom_rect(aes(xmin = 8.5, xmax = 15.5, ymin = -Inf, ymax = Inf), fill = "gray43", alpha = 0.006) + 
    geom_rect(aes(xmin = 22.5, xmax = 38.5, ymin = -Inf, ymax = Inf), fill = "gray43", alpha = 0.006) + 
    geom_rect(aes(xmin = 54.5, xmax = 70.5, ymin = -Inf, ymax = Inf), fill = "gray43", alpha = 0.006) + 
    geom_pointrange(size=2.5, aes(colour = col)) +
    geom_errorbar(width=0.5, size=3.5, aes(colour = col)) +
    scale_color_manual(values=c("black","red")) + 
    ylab("Point estimates and 95% CIs for CV-R2") +
    theme(legend.position="", axis.title = element_text(size=18), axis.text.x = element_text(size=45), axis.text.y = element_text(size=45),
          panel.background = element_blank(),
          axis.title.x = element_text(size=55, margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0)),
          axis.title.y = element_blank(), axis.ticks = element_blank(),
          panel.grid.major = element_line("gray90"),
          plot.margin=unit(c(3,0.5,0.5,20),"cm")) + 
    scale_x_discrete(labels=xticks) 
  
  algos = as.data.frame(table(data$algo)) %>% dplyr::filter(Freq!=0)
  algos = algos$Var1
  
  pdf(plotFile, width = 50, height=50)
  print(p)
  grid.text("Algorithm", x = unit(0.08, "npc"), y = unit(0.992, "npc"), gp=gpar(fontsize=60, col="black", fontface="bold"))
  grid.text("Screen", x = unit(0.23, "npc"), y = unit(0.992, "npc"), gp=gpar(fontsize=60, col="black", fontface="bold"))
  grid.text("____________________________________________", x = unit(0.06, "npc"), y = unit(0.986, "npc"), gp=gpar(fontsize=64, col="gray65"))
  #SL
  grid.text(algos[length(algos)], x = unit(0.08, "npc"), y = unit(0.969, "npc"), gp=gpar(fontsize=50, col="black", fontface="bold"))
  grid.text("____________________________________________", x = unit(0.06, "npc"), y = unit(0.971, "npc"), gp=gpar(fontsize=64, col="gray65"))
  #randomForest
  grid.text(algos[length(algos)-1], x = unit(0.08, "npc"), y = unit(0.87, "npc"), gp=gpar(fontsize=50, col="black", fontface="bold"))
  grid.text("____________________________________________", x = unit(0.06, "npc"), y = unit(0.759, "npc"), gp=gpar(fontsize=64, col="gray65"))
  #glmnet
  grid.text(algos[length(algos)-2], x = unit(0.08, "npc"), y = unit(0.65, "npc"), gp=gpar(fontsize=50, col="black", fontface="bold"))
  grid.text("____________________________________________", x = unit(0.06, "npc"), y = unit(0.546, "npc"), gp=gpar(fontsize=64, col="gray65"))
  #stumpboost
  grid.text("SL.xgboost", x = unit(0.08, "npc"), y = unit(0.44, "npc"), gp=gpar(fontsize=50, col="black", fontface="bold"))
  grid.text("____________________________________________", x = unit(0.06, "npc"), y = unit(0.333, "npc"), gp=gpar(fontsize=64, col="gray65"))
  #glm
  grid.text(algos[length(algos)-4], x = unit(0.08, "npc"), y = unit(0.275, "npc"), gp=gpar(fontsize=50, col="black", fontface="bold"))
  grid.text("____________________________________________", x = unit(0.06, "npc"), y = unit(0.241, "npc"), gp=gpar(fontsize=64, col="gray65"))
  #step
  grid.text(algos[length(algos)-5], x = unit(0.08, "npc"), y = unit(0.18, "npc"), gp=gpar(fontsize=50, col="black", fontface="bold"))
  grid.text("____________________________________________", x = unit(0.06, "npc"), y = unit(0.147, "npc"), gp=gpar(fontsize=64, col="gray65"))
  #step.interaction
  grid.text(algos[length(algos)-6], x = unit(0.08, "npc"), y = unit(0.09, "npc"), gp=gpar(fontsize=50, col="black", fontface="bold"))
  grid.text("____________________________________________", x = unit(0.06, "npc"), y = unit(0.054, "npc"), gp=gpar(fontsize=64, col="gray65"))
  #SL.mean
  grid.text(algos[length(algos)-7], x = unit(0.08, "npc"), y = unit(0.038, "npc"), gp=gpar(fontsize=48, col="black", fontface="bold"))
  grid.text("____________________________________________", x = unit(0.06, "npc"), y = unit(0.04, "npc"), gp=gpar(fontsize=64, col="gray65"))
  
  dev.off()
}


# ---------------------------------------------------------------------------- #
# This code function extracts models with coefficients (or weights) of more than 2 percent in SuperLearner
# ---------------------------------------------------------------------------- #

getAlgos_withCoeffmorethan2percent <- function(fit=fit, writeFile, ...){
  tab <- summary(fit, "superlearner")[[1]] %>% select(learner_names, sl_weight) %>% mutate(learner_names = as.character(learner_names))  %>% 
    mutate(screen_algo=gsub(".skinny|screen.", "", learner_names)) %>% 
    select(screen_algo, sl_weight)
  tab <- tab[order(tab[,2], decreasing=TRUE), ]
  tab <- tab[tab[,2] > 0.02,]
  colnames(tab) <- c("Screen_Algorithm", "SuperLearner Algorithm Coefficient")
  write.csv(tab, file=writeFile)
}


# ---------------------------------------------------------------------------- #
# This code function creates forest plot for dichotomous (binary) Outcomes showing all algorithm screen combinations.
# ---------------------------------------------------------------------------- #

forestplot_allmodels_dichotomousOutcome <- function(data, datasetName, plotFile, plotymin, plotymax, ...){
  
  data$dat = datasetName
  p = ggplot(data, aes(x=screen.algo, y=cv_measure, ymin=ci_low, ymax=ci_high)) +
    geom_pointrange(size=0, aes(colour = col)) +
    geom_errorbar(width=0, size=0, aes(colour = col)) + 
    coord_flip(ylim=c(plotymin, plotymax)) +  
    xlab("Screen") + 
    theme_bw() + 
    scale_y_continuous(breaks=seq(plotymin, plotymax, by=0.1), labels=seq(plotymin, plotymax, by=0.1)) 
  
  xticks = ggplot_build(p)$layout$panel_ranges[[1]]$y.labels
  xticks = sapply(strsplit(xticks,".SL"), `[`, 1)
  xticks = ifelse(xticks=="all.SuperLearner", "all", xticks)
  
  p = p + geom_rect(aes(xmin = -Inf, xmax = 1.5, ymin = -Inf, ymax = Inf), fill = "gray43", alpha = 0.006) + 
    geom_rect(aes(xmin = 8.5, xmax = 15.5, ymin = -Inf, ymax = Inf), fill = "gray43", alpha = 0.006) + 
    geom_rect(aes(xmin = 22.5, xmax = 38.5, ymin = -Inf, ymax = Inf), fill = "gray43", alpha = 0.006) + 
    geom_rect(aes(xmin = 54.5, xmax = 70.5, ymin = -Inf, ymax = Inf), fill = "gray43", alpha = 0.006) + 
    geom_rect(aes(xmin = 86.5, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "gray43", alpha = 0.006) +
    geom_pointrange(size=2.5, aes(colour = col)) +
    geom_errorbar(width=0.5, size=3.5, aes(colour = col)) +
    scale_color_manual(values=c("black","red")) + 
    ylab("Point estimates and 95% CIs for CV-AUC") +
    theme(legend.position="", axis.title = element_text(size=18), axis.text.x = element_text(size=45), axis.text.y = element_text(size=45),
          panel.background = element_blank(),
          axis.title.x = element_text(size=55, margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0)),
          axis.title.y = element_blank(), axis.ticks = element_blank(),
          panel.grid.major = element_line("gray90"),
          plot.margin=unit(c(3,0.5,0.5,20),"cm")) + 
    scale_x_discrete(labels=xticks) 
  
  algos = as.data.frame(table(data$algo)) %>% dplyr::filter(Freq!=0)
  algos = algos$Var1
  
  pdf(plotFile, width = 50, height=60)
  print(p)
  grid.text("Algorithm", x = unit(0.08, "npc"), y = unit(0.992, "npc"), gp=gpar(fontsize=60, col="black", fontface="bold"))
  grid.text("Screen", x = unit(0.23, "npc"), y = unit(0.992, "npc"), gp=gpar(fontsize=60, col="black", fontface="bold"))
  grid.text("____________________________________________", x = unit(0.06, "npc"), y = unit(0.988, "npc"), gp=gpar(fontsize=64, col="gray65"))
  #sl
  grid.text(algos[length(algos)], x = unit(0.08, "npc"), y = unit(0.974, "npc"), gp=gpar(fontsize=50, col="black", fontface="bold"))
  grid.text("____________________________________________", x = unit(0.06, "npc"), y = unit(0.976, "npc"), gp=gpar(fontsize=64, col="gray65"))
  
  #randomForest
  grid.text(algos[length(algos)-1], x = unit(0.08, "npc"), y = unit(0.89, "npc"), gp=gpar(fontsize=50, col="black", fontface="bold"))
  grid.text("____________________________________________", x = unit(0.06, "npc"), y = unit(0.8005, "npc"), gp=gpar(fontsize=64, col="gray65"))
  
  #glment
  grid.text(algos[length(algos)-2], x = unit(0.08, "npc"), y = unit(0.71, "npc"), gp=gpar(fontsize=50, col="black", fontface="bold"))
  grid.text("____________________________________________", x = unit(0.06, "npc"), y = unit(0.626, "npc"), gp=gpar(fontsize=64, col="gray65"))
  
  #naiveBayes
  grid.text("SL.naiveBayes", x = unit(0.08, "npc"), y = unit(0.53, "npc"), gp=gpar(fontsize=50, col="black", fontface="bold"))
  grid.text("____________________________________________", x = unit(0.06, "npc"), y = unit(0.45, "npc"), gp=gpar(fontsize=64, col="gray65"))
  
  #stumpboost
  grid.text("SL.xgboost", x = unit(0.08, "npc"), y = unit(0.36, "npc"), gp=gpar(fontsize=50, col="black", fontface="bold"))
  grid.text("____________________________________________", x = unit(0.06, "npc"), y = unit(0.275, "npc"), gp=gpar(fontsize=64, col="gray65"))
  
  #GLM
  grid.text(algos[length(algos)-5], x = unit(0.08, "npc"), y = unit(0.23, "npc"), gp=gpar(fontsize=50, col="black", fontface="bold"))
  grid.text("____________________________________________", x = unit(0.06, "npc"), y = unit(0.199, "npc"), gp=gpar(fontsize=64, col="gray65"))
  
  #Step
  grid.text(algos[length(algos)-6], x = unit(0.08, "npc"), y = unit(0.155, "npc"), gp=gpar(fontsize=50, col="black", fontface="bold"))
  grid.text("____________________________________________", x = unit(0.06, "npc"), y = unit(0.122, "npc"), gp=gpar(fontsize=64, col="gray65"))
  
  #step.interaction
  grid.text(algos[length(algos)-7], x = unit(0.08, "npc"), y = unit(0.075, "npc"), gp=gpar(fontsize=48, col="black", fontface="bold"))
  grid.text("____________________________________________", x = unit(0.06, "npc"), y = unit(0.045, "npc"), gp=gpar(fontsize=64, col="gray65"))
  
  #Mean
  grid.text(algos[length(algos)-8], x = unit(0.08, "npc"), y = unit(0.032, "npc"), gp=gpar(fontsize=48, col="black", fontface="bold"))
  grid.text("____________________________________________", x = unit(0.06, "npc"), y = unit(0.034, "npc"), gp=gpar(fontsize=64, col="gray65"))
  
  dev.off()
}


# ---------------------------------------------------------------------------- #
# This code function retrieves top 10 models with highest CV-R2 or CV-AUC
# ---------------------------------------------------------------------------- #

getTOP10models <- function(tab, tab2, outcomeType, writeFile, ...){
  
  tab2 = as.data.table(tab2)  %>% dplyr::filter(abs(cv_measure)<100, abs(ci_low)<100, abs(ci_high)<100) %>% dplyr::mutate(Validated_R2num = cv_measure) %>% 
    dplyr::mutate(Validated_R2=format(round(Validated_R2num, 3), nsmall=3), ci_low=format(round(ci_low, 3),nsmall=3), ci_high=format(round(ci_high, 3),nsmall=3)) %>%
    dplyr::mutate(Validated_R2 = paste0(Validated_R2, " (", ci_low, ", ", ci_high, ")")) %>% dplyr::select(screen, algo, Validated_R2, Validated_R2num) 
  
  tab = as.data.table(tab)  %>% dplyr::filter(abs(cv_measure)<100, abs(ci_low)<100, abs(ci_high)<100) %>% dplyr::mutate(CV_R2num = cv_measure) %>% 
    dplyr::mutate(CV_R2=format(round(CV_R2num, 3), nsmall=3), ci_low=format(round(ci_low, 3),nsmall=3), ci_high=format(round(ci_high, 3),nsmall=3)) %>%
    dplyr::mutate(CV_R2 = paste0(CV_R2, " (", ci_low, ", ", ci_high, ")")) %>% dplyr::select(screen, algo, CV_R2, CV_R2num) %>% 
    dplyr::full_join(tab2, by=c("screen", "algo")) %>% arrange(desc(CV_R2num))
  
  tabTop10 <- tab[1:10,] 
  if("SuperLearner" %in% tabTop10$algo)
    tabTop10 <- tab[1:11,] 
  if(!"SuperLearner" %in% tabTop10$algo)
    tabTop10 <- rbind(tabTop10, tab[tab$algo=="SuperLearner",])
  
  
  tab2Top10 = tab %>% arrange(desc(Validated_R2num)) %>% top_n(10)
  if("SuperLearner" %in% tab2Top10$algo)
    tab2Top10 <- tab %>% arrange(desc(Validated_R2num)) %>% top_n(11)
  if(!"SuperLearner" %in% tab2Top10$algo)
    tab2Top10 <- rbind(tab2Top10, tab2Top10[tab2Top10$algo=="SuperLearner",])
  
  tab = tabTop10 %>% dplyr::full_join(tab2Top10, by=c("screen", "algo", "CV_R2", "Validated_R2")) %>% arrange(desc(CV_R2num.x), desc(Validated_R2num.y)) %>%
    dplyr::select(screen, algo, CV_R2, Validated_R2)
  
  tab$algo = ifelse(as.character(tab$algo)=="SL.stumpboost", "SL.xgboost", ifelse(as.character(tab$algo)=="SL.naivebayes", "SL.naiveBayes", as.character(tab$algo)))
  
  if(outcomeType=="dichotomous")
    tab = tab %>% dplyr::rename(CV_AUC = CV_R2, Validated_AUC = Validated_R2)
  write.csv(tab, file=writeFile)
}


# ---------------------------------------------------------------------------- #
# This code function creates predicted probability plots 
# ---------------------------------------------------------------------------- #

predicted_Probability_plot_crossVal <- function(tab, predicted, ...){
  tab = tab[order(-tab$cv_measure)]
  if("SuperLearner" %in% tab[1:3,]$algo)
    tab_top5 = tab[1:4,]
  if(!"SuperLearner" %in% tab[1:3,]$algo)
    tab_top5 = rbind(tab[1:3,], tab[tab$algo=="SuperLearner"])
  tab_top5$screen.algo = as.character(paste0(tab_top5$screen, "_", tab_top5$algo))
  tab_top5 = tab_top5[,.(screen.algo, cv_measure, ci_low, ci_high)]
  tab_top5 = tab_top5[order(tab_top5$cv_measure),]
  tab_top5$screen.algo1 = paste0("screen.", tab_top5$screen.algo)
  tab_top5 = tab_top5[order(-tab_top5$cv_measure),]
  predicted$Model = ifelse(predicted$Model=="SuperLearner", "screen.all_\nSuperLearner", predicted$Model)
  predicted$Model = factor(predicted$Model, levels = sub("_", "_\n", tab_top5$screen.algo1))
  
  set.seed(1)
  p = ggplot(predicted[!is.na(predicted$Sensitivity),], aes(x = Sensitivity, y = y)) + facet_grid(. ~ Model) +
    geom_boxplot(outlier.shape = NA) + geom_jitter(aes(colour = factor(Sensitivity)), pch="O", cex=3) + ylab("Predicted Probability of VRC01 Resistance") + xlab("") + 
    scale_colour_manual(values = c("Sensitive" = "red", "Resistant" = "blue")) + 
    theme_bw() + coord_cartesian(ylim=c(0,1)) +
    theme(legend.position = "", strip.text.x = element_text(size = 10), text = element_text(size=12), axis.title = element_text(size=14))
  
  return(p)
}
