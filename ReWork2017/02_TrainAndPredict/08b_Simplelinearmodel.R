# this script takes the results from the forward feature selection and fine
#tunes the model
rm(list=ls())
library(caret)
library(CAST)
library(parallel)
library(doParallel)
library(randomForest)
library(gbm)
#mainpath <- "/mnt/sd19007/users/hmeyer/Antarctica/ReModel2017/"
mainpath <- "/media/hanna/data/Antarctica/ReModel2017/"
datapath <- paste0(mainpath,"/data/")
rdatapath <- paste0(datapath, "/RData/")
rasterdata <- paste0(datapath,"/raster/")
Shppath <- paste0(datapath,"/ShapeLayers/")
modelpath <- paste0(datapath, "/modeldat/")
trainingDat <- get(load(paste0(modelpath,"trainingDat.RData")))

methods=c("linear","poly")

k <- length(unique(trainingDat$Station))



#prepare for leave one station out cv
folds <- CreateSpacetimeFolds(trainingDat, spacevar = "Station",
                              k = k)


trainingDat$meanLST <- (trainingDat$LST_day+trainingDat$LST_night)/2
predictors <- data.frame("meanLST"=trainingDat$meanLST)
response <- trainingDat$Temperature

ctrl <- trainControl(method = "cv",
                     index = folds$index,
                     indexOut = folds$indexOut,
                     savePredictions = TRUE,
                     verboseIter = TRUE,
                     returnResamp = "all")

for (method in methods){
if (method=="linear"){
  model_final <- train(predictors,
                       response,
                       method = "lm",
                       trControl = ctrl)				 
  
}


if (method=="poly"){
  model_final <- train(Temperature ~ meanLST + I(meanLST^2) + I(meanLST^3),
                       data=trainingDat,
                       method = "lm",
                       trControl = ctrl)				 
  
}

save(model_final,file=paste0(modelpath,"/model_final_",method,".RData"))

}
