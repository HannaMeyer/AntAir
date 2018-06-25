#This script performs forward feature selection for selected ML algorithms
# to determine the best algorithm and the best combination of predictor variables.
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
################################################################################
k <- 10
methods <- c("rf","gbm","nnet")
################################################################################
folds <- CreateSpacetimeFolds(trainingDat, spacevar = "Station", k = k)


for (i in 1:length(methods)){
  predictors <- trainingDat[,c("LST_day","LST_night",
                               "min_hillsh","mean_hillsh","max_hillsh",
                               "min_altitude","mean_altitude","max_altitude",
                               #"refl_b01","refl_b02","refl_b03","refl_b04","refl_b05","refl_b06","refl_b07",
                               #"min_azimuth","mean_azimuth","max_azimuth",
                               "DEM","ice")]
  
  response <- trainingDat$Temperature
  ctrl <- trainControl(method = "cv", 
                       index = folds$index,
                       indexOut = folds$indexOut,
                       savePredictions = TRUE,
                       verboseIter=TRUE,
                       returnResamp = "all")
  method <- methods[i]
  
  if (method=="nnet"){
    predictors <- data.frame(scale(predictors))
    ctrl$trace = FALSE
    ctrl$linout = TRUE
  }
  
  
  cl <- makeCluster(k)
  registerDoParallel(cl)
  ffs_model <- ffs(predictors,
                   response,
                   method = method,
                   importance =TRUE,
                   tuneLength = 3,
                   trControl = ctrl)
  save(ffs_model,file=paste0(modelpath,"/ffs_model_",method,".RData"))
  stopCluster(cl)
  
}
