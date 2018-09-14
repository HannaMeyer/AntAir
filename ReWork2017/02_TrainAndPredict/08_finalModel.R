# this script takes the results from the forward feature selection and fine
#tunes the model
rm(list=ls())
library(caret)
library(CAST)
library(parallel)
library(doParallel)
library(randomForest)
library(gbm)
library(pls)
mainpath <- "/mnt/sd19007/users/hmeyer/Antarctica/ReModel2017/"
#mainpath <- "/media/hanna/data/Antarctica/ReModel2017/"
datapath <- paste0(mainpath,"/data/")
rdatapath <- paste0(datapath, "/RData/")
rasterdata <- paste0(datapath,"/raster/")
Shppath <- paste0(datapath,"/ShapeLayers/")
modelpath <- paste0(datapath, "/modeldat/")
trainingDat <- get(load(paste0(modelpath,"trainingDat.RData")))

bestmodel <- "rf_withVIS"
method <- "rf"
#k <- 10
k <- length(unique(trainingDat$Station))
variables_within_SE <- TRUE # use only variables from ffs within SE of best model

load(paste0(modelpath,"/ffs_model_",bestmodel,".RData"))


#prepare for leave one station out cv
folds <- CreateSpacetimeFolds(trainingDat, spacevar = "Station",
                              k = k)
### see which variables are within SE of best model
if (variables_within_SE){
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
  predictornames <- ffs_model$selectedvars
}

predictors <- trainingDat[,which(names(trainingDat)%in%predictornames)]


if (method=="gbm"){
  tuneGrid <-  expand.grid(interaction.depth = seq(3,14,2), 
                           n.trees = c(100,200,300,400,500),
                           shrinkage = c(0.01,0.05,0.1),
                           n.minobsinnode = 10)
  
}
if (method=="rf"){
  tuneGrid <-  expand.grid(mtry = c(2:ncol(predictors)))
}
if (method=="pls"){
  predictors <- data.frame(scale(predictors))
  tuneGrid <-  expand.grid(ncomp = c(1:ncol(predictors)))
}
response <- trainingDat$Temperature

ctrl <- trainControl(method = "cv",
                     index = folds$index,
                     indexOut = folds$indexOut,
                     savePredictions = TRUE,
                     verboseIter = TRUE,
                     returnResamp = "all")


cl <- makeCluster(10)
registerDoParallel(cl)
if (method=="rf"){
  model_final <- train(predictors,
                       response,
                       method = method,
                       importance = TRUE,
                       tuneGrid = tuneGrid,
                       trControl = ctrl)
}else{
  model_final <- train(predictors,
                       response,
                       method = method,
                       tuneGrid = tuneGrid,
                       trControl = ctrl,
                       distribution="gaussian")				 
  
}
save(model_final,file=paste0(modelpath,"/model_final_",method,".RData"))

stopCluster(cl)
