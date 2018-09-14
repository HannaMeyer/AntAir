
rm(list=ls())
library(caret)
library(raster)
library(rgdal)
library(Rsenal)
library(viridis)
library(lubridate)

mainpath <- "/mnt/sd19007/users/hmeyer/Antarctica/ReModel2017/"
#mainpath <- "/media/hanna/data/Antarctica/ReModel2017/"
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

years <- 2003:2017

for (year in years){

setwd(paste0(predpath,year,"/"))
files <- list.files(pattern=".tif$")
dates_files <- substr(files,17,19)
dates_files <- as.Date(as.numeric(dates_files), origin=as.Date(paste0(substr(files[1],12,15),"-01-01")))
  

AntAir <- stack()
for (i in 1:12){
tmp <- stack(files[month(dates_files)==i])
AntAir <- stack(AntAir,round(mean(tmp,na.rm=TRUE)))
rm(tmp)
gc()
print(i)
}
Sys.setlocale(locale = "en_US.UTF-8")
names(AntAir) <- month(1:12,label=T)

writeRaster(AntAir,paste0(predpath,"/yearly/AntAir_monthly_",year,".grd"),
            overwrite=T,datatype='INT2S')
AntAir_year <- mean(AntAir,na.rm=TRUE)
writeRaster(AntAir_year,paste0(predpath,"/yearly/AntAir_yearly_",year,".grd"),
            overwrite=T,datatype='INT2S')
rm(AntAir)
gc()
}