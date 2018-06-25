# this script takes the results from the forward feature selection and fine 
#tunes the model
rm(list=ls())
library(caret)
library(CAST)
library(parallel)
library(doParallel)
library(randomForest)
mainpath <- "/mnt/sd19007/users/hmeyer/Antarctica/ReModel2017/"
#mainpath <- "/media/hanna/data/Antarctica/ReModel2017/"
datapath <- paste0(mainpath,"/data/")
rdatapath <- paste0(datapath, "/RData/")
rasterdata <- paste0(datapath,"/raster/")
Shppath <- paste0(datapath,"/ShapeLayers/")
modelpath <- paste0(datapath, "/modeldat/")
trainingDat <- get(load(paste0(modelpath,"trainingDat.RData")))

bestmodel <- "rf"
k <- 10

load(paste0(modelpath,"/ffs_model_",bestmodel,".RData"))

folds <- CreateSpacetimeFolds(trainingDat, spacevar = "Station", k = k)
predictors <- trainingDat[,names(ffs_model$trainingData)[-length(
  names(ffs_model$trainingData))]]
response <- trainingDat$Temperature

ctrl <- trainControl(method = "cv", 
                     index = folds$index,
                     indexOut = folds$indexOut,
                     savePredictions = TRUE,
                     verboseIter = TRUE,
                     returnResamp = "all")

if (bestmodel=="nnet"){
  predictors <- data.frame(scale(predictors))
  ctrl$trace = FALSE
  ctrl$linout = TRUE
}


model_final <- train(predictors,
                     response,
                     method = method,
                     importance =TRUE,
                     tuneLength = 15,
                     trControl = ctrl)
save(model_final,file=paste0(modelpath,"/model_final_",bestmodel,".RData"))

