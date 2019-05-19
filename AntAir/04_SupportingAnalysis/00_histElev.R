rm(list=ls())
library(caret)
library(raster)
library(rgdal)
library(sptm)
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

modeldat <- get(load(paste0(modelpath,"/full_dataset.RData")))

subs <- unique(modeldat[,c("Station","DEM")])


pdf(paste0(vispath,"/hist_elev.pdf"),width=6,height=5)
qplot(subs$DEM, geom="histogram",
      fill=I("grey"), xlab="Elevation (m)",
      ylab="Number of Stations",
      col=I("black")) + theme_bw()+
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0, 0),
                                                           breaks=c(2,4,6,8),
                                                           limits=c(0,10))
dev.off()

