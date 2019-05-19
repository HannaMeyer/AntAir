

rm(list=ls())
library(caret)
library(raster)
library(rgdal)
library(Rsenal)
library(viridis)
library(ggplot2)
library(reshape2)
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



external <- get(load(paste0(vispath,"/external_validation_",modeltype,".RData")))
llo <- get(load(paste0(vispath,"/LLO_globalvalidation_",modeltype,".RData")))
llo <- data.frame(llo,"type"="Spatial")
external <- data.frame("obs"=external$Temperature,"pred"=external$prediction,"type"="Temporal")

dat <- rbind(llo,external)


cairo_pdf(paste0(vispath,"/validation_",modeltype,".pdf"),width=8,height=4)
print(ggplot(dat, aes(obs,pred)) +  facet_grid(. ~ type)+
        stat_binhex(bins=100)+
        xlim(min(dat[,1:2]),max(dat[,1:2]))+ylim(min(dat[,1:2]),max(dat[,1:2]))+
        xlab("Measured air temperature (°C)")+
        ylab("Predicted air temperature (°C)")+
        geom_abline(slope=1, intercept=0,lty=2)+
        scale_fill_gradientn(name = "data points", 
                             #trans = "log", 
                             #breaks = 10^(0:3),
                             colors=viridis(10)))
dev.off()
