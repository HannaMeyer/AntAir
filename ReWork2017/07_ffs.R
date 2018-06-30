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
datapath <- paste0(mainpath,"/data/")
rdatapath <- paste0(datapath, "/RData/")
rasterdata <- paste0(datapath,"/raster/")
Shppath <- paste0(datapath,"/ShapeLayers/")
modelpath <- paste0(datapath, "/modeldat/")
trainingDat <- get(load(paste0(modelpath,"trainingDat.RData")))
################################################################################
# choose models and number of folds for cross validation
k <- 10
VIS <- TRUE
methods <- c("lm","nnet","rf","gbm","pls")
################################################################################
trainingDat$timevar <- substr(trainingDat$Date,1,7)
#initialize LLTO CV
folds <- CreateSpacetimeFolds(trainingDat, spacevar = "Station", 
                              timevar= "timevar",
                              k = k)
response <- trainingDat$Temperature

for (i in 1:length(methods)){
  predictors <- trainingDat[,c("LST_day","LST_night",
                               "min_hillsh","mean_hillsh","max_hillsh",
                               "min_altitude","mean_altitude","max_altitude",
                               "DEM","ice")]
  if(VIS){
    predictors <- trainingDat[,c("LST_day","LST_night",
                                 "min_hillsh","mean_hillsh","max_hillsh",
                                 "min_altitude","mean_altitude","max_altitude",
                                 "refl_b01","refl_b02","refl_b03","refl_b04",
                                 "refl_b05","refl_b06","refl_b07",
                                 "DEM")]
  }
  ctrl <- trainControl(method = "cv", 
                       index = folds$index,
                       indexOut = folds$indexOut,
                       savePredictions = TRUE,
                       verboseIter=TRUE,
                       returnResamp = "all")
  method <- methods[i]
  if (method=="nnet"|method=="pls"){
    predictors <- data.frame(scale(predictors))
  }
  
  
  cl <- makeCluster(k)
  registerDoParallel(cl)
  
  if(method=="nnet"){
    ffs_model <- ffs(predictors,
                     response,
                     method = method,
                     tuneLength = 3,
                     trControl = ctrl,
                     trace = FALSE,
                     linout = TRUE)
    
    
  }
  
  
  if(method=="gbm"){
    ffs_model <- ffs(predictors,
                     response,
                     method = method,
                     tuneLength = 3,
                     trControl = ctrl,
                     distribution = "gaussian"
    )
  }
  
  
  if(method%in%c("rf","lin","pls")){
    
    ffs_model <- ffs(predictors,
                     response,
                     method = method,
                     importance =TRUE,
                     tuneLength = 3,
                     trControl = ctrl)
  }

  
  if(VIS){
    save(ffs_model,file=paste0(modelpath,"/ffs_model_",method,"_withVIS.RData"))
  }else{
    save(ffs_model,file=paste0(modelpath,"/ffs_model_",method,".RData"))
  }
  stopCluster(cl)
  
}
