
rm(list=ls())
library(caret)
library(raster)
library(rgdal)
library(viridis)
library(sptm)
library(mapview)
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

models <- c("rf","gbm","pls","nnet")
year <- "2016"
day <- "033" #33
#cropext <- c(290000,528000,-1400000,-1200000)
cropext <- c(492126.9, 1956452, -2052473,-997012.7)

preds <- stack()
for (i in 1:length(models)){
  preds <- stack(preds,paste0(predpath,"/",models[i],"/",year,"/prediction_",year,"_",day,".tif"))
}
names(preds) <- toupper(models)
#dat <- stack(dat,preds/10)
dat <- preds/10

maxv <- max(values(dat[[1]]),na.rm=T)
minv <- min(values(dat[[1]]),na.rm=T)

dat_crop <- crop(dat,cropext)

minmax <- seq(min(values(dat[[1]]),na.rm=T),
              max(values(dat[[1]]),na.rm=T),0.5)
minmax_crop <- seq(min(values(dat_crop[[1]]),na.rm=T),
              max(values(dat_crop[[1]]),na.rm=T),0.5)


png(paste0(vispath,"/spatialComp_full.png"),
    width=14,height=14,units="cm",res = 500,type = "cairo")
spplot(dat,col.regions=mapviewPalette("mapviewSpectralColors")(400),
       colorkey = list(at=minmax),
       par.settings= list(strip.background=list(col="grey")),
       maxpixels=500000)
dev.off()

png(paste0(vispath,"/spatialComp.png"),
    width=16,height=12,units="cm",res = 400,type = "cairo")
spplot(dat_crop,col.regions=mapviewPalette("mapviewSpectralColors")(400),
       colorkey = list(at=minmax_crop),zlim=c(-32,-5),
       par.settings= list(strip.background=list(col="grey")),
       maxpixels=500000,scales=list(draw=TRUE))
dev.off()