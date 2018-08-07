#09_evaluateModel


#07 run ffs
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

models <- c("rf","gbm","linear","poly","pls")

validdat <- list()
acc <- 0
for (modeltype in models){
  acc <- acc+1
  model <- get(load(paste0(modelpath,"model_final_",modeltype,".RData")))
  dat <- model$resample
  if (modeltype=="rf"){
    dat <- dat[dat$mtry==model$bestTune$mtry,]
  }
  if (modeltype=="gbm"){
    dat <- dat[dat$shrinkage==model$bestTune$shrinkage&
                 dat$interaction.depth==model$bestTune$interaction.depth&
                 dat$n.minobsinnode==model$bestTune$n.minobsinnode&
                 dat$n.trees==model$bestTune$n.trees,]
  }
  dat <- dat[,c("RMSE","Rsquared","MAE")]
  dat$model <- modeltype
  validdat[[acc]] <- dat
}

validdat <- do.call("rbind",validdat)
validdat <- melt(validdat)


ggplot(data = validdat, aes(x = model, y = value)) +
  geom_boxplot(notch = TRUE)+ facet_grid(variable ~ .,scales="free")


