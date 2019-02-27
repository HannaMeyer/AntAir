###prediction

rm(list=ls())
library(caret)
library(raster)
library(rgdal)
mainpath <- "/scratch/tmp/hmeyer1/AntAir/"
#mainpath <- "/media/hanna/data/Antarctica/ReModel2017/"
datapath <- paste0(mainpath,"/data/")
rdatapath <- paste0(datapath, "/RData/")
rasterdata <- paste0(datapath,"/raster/")
Shppath <- paste0(datapath,"/ShapeLayers/")
modelpath <- paste0(datapath, "/modeldat/")
predpath <- paste0(datapath, "/predictions/")
#MODISpath <- paste0(mainpath,"/MODISLST/")
MODISpath <- paste0(datapath,"/MODISLST/")
#MODISpath_VIS <- "/mnt/sd19006/data/users/fdetsch/R-Server/data/MODIS_ARC/PROCESSED/mod09ga-antarctica/"
tmppath <- paste0(mainpath,"/tmp/")


###### Settings
methods <- c("rf","nnet","pls","lm","gbm")
years <- c(2016)
###### 
for (i in 1:length(methods)){
  #  predpath <- paste0(predpath, "/",methods[i],"/")
  #  dir.create(predpath)
  tmp <- get(load(paste0(modelpath,"model_final_",methods[i],".RData")))
  assign(paste0("model_",methods[i]),tmp)
}

template <-raster(paste0(rasterdata,"/template.tif"))
DEM <- raster(paste0(rasterdata,"/dem.tif"))
names(DEM) <- "DEM"
ice <- raster(paste0(rasterdata,"/ice.tif"))
DEM <- resample(DEM,template)
ice <- resample(ice,template)

predictors <- c()
for (i in 1:length(methods)){
  assign("tmp2",eval(parse(text=paste0("model_",methods[i]))))
  predictors <- c(predictors,names(tmp2$trainingData))
}
predictors <- unique (predictors)
predictors <- predictors[-which(predictors==".outcome")]
################################################################################

for (year in years){ 
  tmppath <- paste0(tmppath,"/",year)
  dir.create(tmppath)
  rasterOptions(tmpdir = tmppath)
  for (i in 1:length(methods)){
    dir.create(paste0(predpath, "/",methods[i],"/",year))
  }
  
  MODISdat_terra <- list.files(paste0(MODISpath,"/terra/",year,"/"),
                               recursive = TRUE,pattern=".tif",full.names = TRUE)
  MODISdat_aqua <- list.files(paste0(MODISpath,"/aqua/",year,"/"),
                              recursive = TRUE,pattern=".tif$",full.names = TRUE)
  
  aqua_day <- MODISdat_aqua[grep(pattern="LST_LST_Day",MODISdat_aqua)]
  aqua_night <- MODISdat_aqua[grep(pattern="LST_LST_Night",MODISdat_aqua)]
  terra_day <- MODISdat_terra[grep(pattern="LST_LST_Day",MODISdat_terra)]
  terra_night <- MODISdat_terra[grep(pattern="LST_LST_Night",MODISdat_terra)]
  
  if(any(grepl("refl", predictors))){
    MODIS_VIS <-   list.files(paste0(MODISpath_VIS,"/",year),full.names = TRUE,pattern=".tif$")
    MODIS_VIS <- MODIS_VIS[grepl("sur_refl",MODIS_VIS)]
    VIS_dates <- substr(MODIS_VIS,nchar(MODIS_VIS)-25,nchar(MODIS_VIS)-19)
  }
  
  if(any(grepl("hillsh", predictors)|grepl("solar", predictors))){ 
    hillshades <- list.files(paste0(rasterdata,"/hillshade/"),pattern=".tif$",full.names = TRUE)
    solarprops <- list.files(paste0(rasterdata,"/solarinfo/"),pattern=".tif$",full.names = TRUE)
  }
  for (i in 1:365){
    doy <- sprintf("%03d", i)
    
    if(length(aqua_day[substr(aqua_day,nchar(aqua_day)-6,nchar(aqua_day)-4)==doy])==0&
       length(terra_day[substr(terra_day,nchar(terra_day)-6,nchar(terra_day)-4)==doy])>0){
      LST_day <-  raster(terra_day[substr(terra_day,nchar(terra_day)-6,nchar(terra_day)-4)==doy])
     # print("LST day only based on Terra")
    }else{
      if(length(terra_day[substr(terra_day,nchar(terra_day)-6,nchar(terra_day)-4)==doy])==0&
         length(aqua_day[substr(aqua_day,nchar(aqua_day)-6,nchar(aqua_day)-4)==doy])>0){
        LST_day <-  raster(aqua_day[substr(aqua_day,nchar(aqua_day)-6,nchar(aqua_day)-4)==doy])
       # print("LST day only based on Aqua")
        
      }else{
        LST_day <- tryCatch(
          mean(stack(aqua_day[substr(aqua_day,nchar(aqua_day)-6,nchar(aqua_day)-4)==doy],
                     terra_day[substr(terra_day,nchar(terra_day)-6,nchar(terra_day)-4)==doy]),
               na.rm=T),
          error=function(e)e)
       # print("LST day based on both or none")
      }
    }
    
    
    
    if(length(aqua_night[substr(aqua_night,nchar(aqua_night)-6,nchar(aqua_night)-4)==doy])==0&
       length(terra_night[substr(terra_night,nchar(terra_night)-6,nchar(terra_night)-4)==doy])>0){
      LST_night <-  raster(terra_night[substr(terra_night,nchar(terra_night)-6,
                                              nchar(terra_night)-4)==doy])
      print("LST night only based on Terra")
    }else{
      if(length(terra_night[substr(terra_night,nchar(terra_night)-6,nchar(terra_night)-4)==doy])==0&
         length(aqua_night[substr(aqua_night,nchar(aqua_night)-6,nchar(aqua_night)-4)==doy])>0){
        LST_night <-  raster(aqua_night[substr(aqua_night,nchar(aqua_night)-6,nchar(aqua_night)-4)==doy])
        print("LST night only based on Aqua")
      }else{
        
        LST_night <-  tryCatch(mean(stack(aqua_night[substr(aqua_night,nchar(aqua_night)-6,nchar(aqua_night)-4)==doy],
                                          terra_night[substr(terra_night,nchar(terra_night)-6,nchar(terra_night)-4)==doy]),
                                    na.rm=T),
                               error=function(e)e)
        print("LST night based on both or none")
        if(inherits(LST_day,"error")|inherits(LST_night,"error")){
          next
        }
      }
    }
    preds <- stack(LST_day,LST_night)
    names(preds) <- c("LST_day","LST_night")
    if(any(grepl("refl", predictors))){
      VISdats <- MODIS_VIS[substr(VIS_dates,5,7)==doy]
      nme <- substr(VISdats,nchar(VISdats)-13,nchar(VISdats)-6)
      VISdats <- VISdats[nme%in%predictors]
      nme <- nme[nme%in%predictors]
      VIS <- tryCatch(
        stack(VISdats),error=function(e)e)
      if(inherits(VIS,"error")){
        next
      }
      rclmat <- matrix(c(NA, 0, 0,-990,0,0), ncol=3, byrow=TRUE)
      VIS <- reclassify(VIS, rclmat)
      
      VIS <- resample(VIS,template)
      names(VIS)<- nme
      preds <- stack(preds,VIS)
    }
    if(inherits(LST_night,"error")|inherits(LST_day,"error")){
      next
    }
    
    if(any(grepl("hillsh", predictors)|grepl("solar", predictors))){ 
      hillshade <- stack( hillshades[grep(pattern=paste0(doy,".tif$"),hillshades)])
      proj4string(hillshade) <- proj4string(template)
      names(hillshade) <- c("min_hillsh","mean_hillsh","max_hillsh")
      solarprop <- stack(solarprops[grep(solarprops,pattern=paste0(doy,".tif$"))])
      proj4string(solarprop) <- proj4string(template)
      names(solarprop) <- c("min_altitude","mean_altitude","max_altitude",
                            "min_azimuth","mean_azimuth","max_azimuth")
      solarprop <- resample(solarprop,template)
      hillshade <- resample(hillshade,template)
      preds <- stack(preds,hillshade,solarprop)
    }
    
    
    preds <- stack(preds,DEM,ice)
    
    ################################################################################    
    # Scaling and predictions    
    ################################################################################     
    for (i in 1:length(methods)){
      model <- eval(parse(text=paste0("model_",methods[i])))
      if (methods[i]=="nnet"|methods[i]=="pls"){
        preds_scaled <- preds
        scaleStats <- model$scaleStats
        
        for (k in 1:nlayers(preds_scaled)){
          if(!any(row.names(scaleStats)==names(preds_scaled)[k])){
            next
          }
          rowID <- which(row.names(scaleStats)==names(preds_scaled)[k])
          preds_scaled[[k]] <- (preds_scaled[[k]]-scaleStats$mean[rowID])/scaleStats$sd[rowID]
        }
        spatialpred <- predict(preds_scaled,model)
      }else{
        if (methods[i]=="lm"){
          LSTmean <- (preds$LST_day+preds$LST_night)/2
          names(LSTmean) <- "LSTmean"
          spatialpred <- predict(LSTmean,model)
        }else{
          spatialpred <- predict(preds,model)
        }
      }
      writeRaster(spatialpred*10,paste0(predpath,"/",methods[i],"/",year,
                                        "/prediction_",year,"_",doy,".tif"),
                  overwrite=TRUE,datatype='INT2S')
    }
  }
  print(i)
  file.remove(list.files(tmppath,full.names = TRUE))
}
