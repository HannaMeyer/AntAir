#This script performs forward feature selection for selected ML algorithms
# to determine the best algorithm and the best combination of predictor variables.
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
################################################################################
# MODEL RUN SETTINGS
k <- 5 # number of folds for cross validation
ncores <- 10 # number of cores for parallel computation
#VIS <- TRUE # include visible predictors?
metric <- "RMSE" # Selection of variables made by either Rsquared or RMSE
methods <- c("lm","gbm","pls","nnet","rf")
withinSE <- FALSE # favour models with less variables or not?
################################################################################
trainingDat$timevar <- substr(trainingDat$Date,1,7)
set.seed(100)
folds <- CreateSpacetimeFolds(trainingDat, spacevar = "Station", 
                              #timevar= "timevar",
                              k = k)
response <- trainingDat$Temperature

for (i in 1:length(methods)){
#  predictors <- trainingDat[,c("LST_day","LST_night",
#                               "min_hillsh","mean_hillsh","max_hillsh",
#                               "min_altitude","mean_altitude","max_altitude",
#                               #"refl_b01","refl_b02","refl_b03","refl_b04","refl_b05","refl_b06","refl_b07",
#                               #"min_azimuth","mean_azimuth","max_azimuth",
#                               "DEM","ice")]
#  if(VIS){
		  predictors <- trainingDat[,c("LST_day","LST_night",
                               "refl_b01","refl_b02","refl_b03","refl_b04",
                               "refl_b05","refl_b06","refl_b07",
                               "DEM")]
#  }

  ctrl <- trainControl(method = "cv", 
                       index = folds$index,
                       indexOut = folds$indexOut,
                       savePredictions = TRUE,
                       verboseIter=TRUE,
                       returnResamp = "final")
  method <- methods[i]
  tuneLength <- 2
  tuneGrid <- NULL
  if (method=="gbm"){
    tuneLength <- 10
  }
  if (method=="rf"){
 #   tuneLength <- 1
    tuneGrid <- expand.grid(mtry = 2)
  }
  
  if (method=="pls"){
    predictors <- data.frame(scale(predictors))
    tuneLength <- 10
  }
  if (method=="nnet"){
 #   tuneLength <- 1
    predictors <- data.frame(scale(predictors))
    tuneGrid <- expand.grid(size = seq(2,ncol(predictors),2),
                decay = seq(0,0.1,0.025))
  }
  cl <- makeCluster(ncores)
  registerDoParallel(cl)
  ffs_model <- ffs(predictors,
                   response,
                   metric=metric,
                   withinSE = withinSE,
                   method = method,
                   importance =TRUE,
                   tuneLength = tuneLength,
                   tuneGrid = tuneGrid,
                   trControl = ctrl,
                   trace = FALSE, #relevant for nnet
                   linout = TRUE) #relevant for nnet
#  if(VIS){
  save(ffs_model,file=paste0(modelpath,"/ffs_model_",method,"_withVIS.RData"))
#}else{
#  save(ffs_model,file=paste0(modelpath,"/ffs_model_",method,".RData"))
#}
  stopCluster(cl)
  
}
