
rm(list=ls())
library(caret)
library(raster)
library(rgdal)
library(Rsenal)
library(viridis)
library(lubridate)
library(tidyverse)
library(mapview)
library(latticeExtra)
library(Orcs)
library(gridExtra)
library(grid)

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

yearlyrasters <- list.files(paste0(predpath,"/yearly"),full.names = TRUE,pattern=".grd")
longyeartemp <- yearlyrasters[grepl("_yearly_",yearlyrasters)]%>%stack%>%mean(na.rm=T)





#scico(10, palette = "batlow")


cropext1 <- c(-2686990 ,2868305,-2236302,2258437)
longyeartemp <- crop(longyeartemp,extent(cropext1))
cropext <- c(290000,528000,-1400000,-1200000)
minmax <- seq(min(values(longyeartemp/10),na.rm=T),
              max(values(longyeartemp/10),na.rm=T),0.15)
p1 <- spplot(longyeartemp/10, scales=list(draw=TRUE),at=minmax,
       col.regions=mapviewPalette("mapviewSpectralColors")(600),
       maxpixels=500000,
       colorkey=FALSE)+
 spplot(as(extent(cropext), 'SpatialPolygons'),col.regions="transparent" )+
  layer(panel.text(x=-2400000,y=2e+06,labels="(a)"))


longyeartemp_dv <- crop(longyeartemp,cropext)
minmax_dv <- seq(min(values(longyeartemp_dv/10),na.rm=T),
              max(values(longyeartemp_dv/10),na.rm=T),0.15)
p2 <- spplot(longyeartemp_dv/10,
       scales=list(draw=TRUE),
       maxpixels=500000,
       colorkey=FALSE,
       #at=minmax,
       col.regions=mapviewPalette("mapviewSpectralColors")(600))+
  layer(panel.text(x=300000,y=-1210000,labels="(b)"))


png(paste0(vispath,"/spatialPred_yearlyAgg.png"),
    width=22,height=15,units="cm",res = 600,type = "cairo")
grid.newpage()
latticeCombineGrid(list(p1,p2),layout=c(2,1))
downViewport(trellis.vpname(name = "figure"))
vp1 <- viewport(x = 0.041, y = 1.05, 
                height = 0.1, width = 0.455,
                just = c("left", "bottom"),
                name = "key1.vp")
pushViewport(vp1)
key1 <- draw.colorkey(key = list(col = mapviewPalette("mapviewSpectralColors")(600),
                                 at = minmax,space="top",
                                 width=1), draw = TRUE)
upViewport(1)
vp2 <- viewport(x = 0.5448, y = 1.05, 
                height = 0.1, width = 0.455,
                just = c("left", "bottom"),
                name = "key2.vp")
pushViewport(vp2)
key2 <- draw.colorkey(key = list(col = mapviewPalette("mapviewSpectralColors")(600),
                                 at = minmax_dv,space="top",
                                 width=1), draw = TRUE)


dev.off()

