# this script takes the results from the forward feature selection and fine
#tunes the model
rm(list=ls())
library(caret)
library(CAST,lib.loc="/home/h/hmeyer1/R/")
library(parallel)
library(doParallel)
library(randomForest)
library(gbm)
library(pls)
mainpath <- "/scratch/tmp/hmeyer1/AntAir/"
#mainpath <- "/media/hanna/data/Antarctica/ReModel2017/"
datapath <- paste0(mainpath,"/data/")
rdatapath <- paste0(datapath, "/RData/")
rasterdata <- paste0(datapath,"/raster/")
Shppath <- paste0(datapath,"/ShapeLayers/")
modelpath <- paste0(datapath, "/modeldat/")
trainingDat <- get(load(paste0(modelpath,"trainingDat.RData")))
################################################################################
# Settings
################################################################################
seed <- 100
cores <- 20
k <- length(unique(trainingDat$Station))
variables_within_SE <- FALSE # use only variables from ffs within SE of best model
methods <- c("lm","rf")

################################################################################
# help functions etc
################################################################################
scaling <- function(predictors,scaleStats){
  for (i in 1:ncol(predictors)){
    rowID <- which(row.names(scaleStats)==names(predictors)[i])
    predictors[,i] <- (predictors[,i]-scaleStats$mean[rowID])/scaleStats$sd[rowID]
  }
  return(predictors)
}
cl <- makeCluster(cores)
registerDoParallel(cl)
################################################################################
# Train models
################################################################################


for (method in methods){
  
  
  ffs_model <- tryCatch(get(load(paste0(modelpath,"/ffs_model_",method,".RData"))),
                        error=function(e)e)
  if(inherits(ffs_model,"error")){
    warning("no FFS could be found")
  }
  
  #prepare for leave one station out cv
  set.seed(seed)
  folds <- CreateSpacetimeFolds(trainingDat, spacevar = "Station",
                                k = k)
  ### see which variables are within SE of best model
  if (variables_within_SE&!inherits(ffs_model,"error")){
    tmp <- c()
    for (i in 1:length(ffs_model$selectedvars_perf)){
      tmp <- c(tmp,which(ffs_model$selectedvars_perf< ffs_model$selectedvars_perf[i]-
                           ffs_model$selectedvars_perf_SE[i])[1])
    }
    if (any(!is.na(tmp))){
      cutoff <- max(tmp,na.rm=TRUE)
    }else{
      cutoff <- 1
    }
    predictornames <- ffs_model$selectedvars[1:(cutoff+1)]
    ###
  }else{
    if (!inherits(ffs_model,"error")){
      predictornames <- ffs_model$selectedvars
    } else{
      trainingDat$LSTmean <- (trainingDat$LST_day+trainingDat$LST_night)/2
      LSTmean <- trainingDat$LSTmean
      predictornames <- "LSTmean"
      warning("only LST mean was used as predictor")
    }
  }
  
  predictors <- trainingDat[,which(names(trainingDat)%in%predictornames)]
  
  
  if (method=="gbm"){
    tuneGrid <-  expand.grid(interaction.depth = seq(3,14,2), 
                             n.trees = c(100,200,300,400,500),
                             shrinkage = c(0.01,0.05,0.1),
                             n.minobsinnode = 10)
    
  }
 # if (method=="lm"){
   # predictors <- data.frame(predictors)
  #  names(predictors) <- "LSTmean"
#  }
  if (method=="rf"){
    tuneGrid <-  expand.grid(mtry = c(seq(2,ncol(predictors),2)))
  }
  if (method=="nnet"){
    predictors <- scaling(predictors,ffs_model$scaleStats)
    tuneGrid <- expand.grid(size = seq(2,ncol(predictors),1),
                            decay = seq(0,0.1,0.01))
  }
  
  if (method=="pls"){
    predictors <- scaling(predictors,ffs_model$scaleStats)
    tuneGrid <-  expand.grid(ncomp = c(1:ncol(predictors)))
  }
  response <- trainingDat$Temperature
  
  ctrl <- trainControl(method = "cv",
                       index = folds$index,
                       indexOut = folds$indexOut,
                       savePredictions = TRUE,
                       verboseIter = TRUE,
                       returnResamp = "all")
  
  
  if (method=="lm"){
    model_final <- train(Temperature~LSTmean,
                         data=trainingDat,
                         method = method,
                         trControl = ctrl)
  }else{
  if (method!="nnet"){
    model_final <- train(predictors,
                         response,
                         method = method,
                         tuneGrid = tuneGrid,
                         trControl = ctrl)
  }else{
    model_final <- train(predictors,
                         response,
                         method = method,
                         tuneGrid = tuneGrid,
                         trControl = ctrl,
                         trace = FALSE, #relevant for nnet
                         linout = TRUE)				 
    
  }
  }
  model_final$scaleStats <- ffs_model$scaleStats
  save(model_final,file=paste0(modelpath,"/model_final_",method,".RData"))
  
}

stopCluster(cl)