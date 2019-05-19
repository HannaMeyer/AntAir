
rm(list=ls())
library(caret)
library(raster)
library(rgdal)
library(viridis)
library(sptm)
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

modeltype <- "gbm"
temporal <- get(load(paste0(vispath,"/external_validation_",modeltype,".RData")))
spatial <- get(load(paste0(vispath,"/LLO_globalvalidation_",modeltype,".RData")))
temporal <- temporal[,c("prediction","Temperature")]
names(temporal)<- c("pred","obs")

spatial$type <- "Spatial"
temporal$type <- "Temporal"

dat <- rbind(spatial,temporal)

minv <- min(dat[,c("obs","pred")])
maxv <- max(dat[,c("obs","pred")])


pdf(paste0(vispath,"/validation_",modeltype,".pdf"),width=8,height=4)
ggplot(dat, aes(obs,pred)) + 
  stat_binhex(bins=100)+
  xlim(minv,maxv)+ylim(minv,maxv)+
  xlab("Measured Tair (°C)")+
  ylab("Predicted Tair (°C)")+
  geom_abline(slope=1, intercept=0,lty=2)+
  scale_fill_gradientn(name = "data points", 
                       #trans = "log", 
                       #breaks = 10^(0:3),
                       colors=viridis(10))+
  theme_bw()+
  facet_grid(.~ type)
dev.off()
