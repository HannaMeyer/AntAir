
rm(list=ls())
library(caret)
library(raster)
library(rgdal)
library(Rsenal)
library(viridis)
#mainpath <- "/mnt/sd19007/users/hmeyer/Antarctica/ReModel2017/"
mainpath <- "/media/hanna/data/Antarctica/ReModel2017/"
datapath <- paste0(mainpath,"/data/")
rdatapath <- paste0(datapath, "/RData/")
rasterdata <- paste0(datapath,"/raster/")
Shppath <- paste0(datapath,"/ShapeLayers/")
modelpath <- paste0(datapath, "/modeldat/")
predpath <- paste0(datapath, "/predictions/")
MODISpath <- paste0(mainpath,"/MODISLST/")
vispath <- datapath <- paste0(mainpath,"/visualizations/")

tmppath <- paste0(mainpath,"/tmp/")
rasterOptions(tmpdir = tmppath)
modeltype <- "rf"


model <- get(load(paste0(modelpath,"model_final_",modeltype,".RData")))
testdat <- get(load(paste0(modelpath,"testingDat.RData")))

testdat$prediction <- predict(model,testdat)
testdat$error <- abs(testdat$prediction-testdat$Temperature)

boxplot(testdat$error~month(testdat$Date))
boxplot(testdat$error~year(testdat$Date),notch=T)

