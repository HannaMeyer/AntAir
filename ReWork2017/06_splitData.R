#06_Splitdata

rm(list=ls())
library(raster)
library(lubridate)
#mainpath <- "/media/memory02/users/hmeyer/Antarctica/ReModel2017/"
mainpath <- "/media/hanna/data/Antarctica/ReModel2017/"
datapath <- paste0(mainpath,"/data/")
rdatapath <- paste0(datapath, "/RData/")
rasterdata <- paste0(datapath,"/raster/")
Shppath <- paste0(datapath,"/ShapeLayers/")
modelpath <- paste0(datapath, "/modeldat/")


dat <- get(load(paste0(modelpath,"full_dataset.RData")))
#set.seed(100)
#trainingYears <- sample(2002:2016,7)
testingYears <- seq(2003,2016,3)
trainingDat <- dat[!year(dat$Date)%in%testingYears,]
testingDat <- dat[year(dat$Date)%in%testingYears,]

save(trainingDat,file=paste0(modelpath,"trainingDat.RData"))
save(testingDat,file=paste0(modelpath,"testingDat.RData"))