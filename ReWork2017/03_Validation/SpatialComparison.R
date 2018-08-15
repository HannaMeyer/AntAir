
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

models <- c("rf","gbm","linear","pls","poly")

validdat <- list()
acc <- 0
results_global <- c()
results_LLO <- c()
for (modeltype in models){
  acc <- acc+1
  model <- get(load(paste0(modelpath,"model_final_",modeltype,".RData")))
  dat <- model$resample
  preds <- model$pred
  if (modeltype=="rf"){
    dat <- dat[dat$mtry==model$bestTune$mtry,]
    preds <- preds[preds$mtry==model$bestTune$mtry,]
  }
  if (modeltype=="pls"){
    dat <- dat[dat$ncomp==model$bestTune$ncomp,]
    preds <- preds[preds$ncomp==model$bestTune$ncomp,]
  }
  if (modeltype=="gbm"){
    dat <- dat[dat$shrinkage==model$bestTune$shrinkage&
                 dat$interaction.depth==model$bestTune$interaction.depth&
                 dat$n.minobsinnode==model$bestTune$n.minobsinnode&
                 dat$n.trees==model$bestTune$n.trees,]
    preds <- preds[preds$shrinkage==model$bestTune$shrinkage&
                     preds$interaction.depth==model$bestTune$interaction.depth&
                     preds$n.minobsinnode==model$bestTune$n.minobsinnode&
                     preds$n.trees==model$bestTune$n.trees,]
  }
  dat <- dat[,c("RMSE","Rsquared","MAE")]
  dat$model <- modeltype
  validdat[[acc]] <- dat
  preds <- preds[,c("pred","obs")]
  results_LLO_mean <- apply(dat[,1:3],2,mean)
  results_LLO_sd <- apply(dat[,1:3],2,sd)
  results_LLO_tmp <- data.frame("Model"=modeltype,
                                "MAE"=round(results_LLO_mean[3],2),
                                "MAE_SD"=round(results_LLO_sd[3],2),
                                "RMSE"=round(results_LLO_mean[1],2),
                                "RMSE_SD"=round(results_LLO_sd[1],2),
                                "Rsquared"=round(results_LLO_mean[2],2),
                                "Rsquared_SD"=round(results_LLO_sd[2],2))
  results_LLO <- rbind(results_LLO,results_LLO_tmp)
  
  save(preds,file=paste0(vispath,"/LLO_globalvalidation_",modeltype,".RData"))
  
  pdf(paste0(vispath,"/LLO_globalvalidation_",modeltype,".pdf"),width=6,height=5)
  print(ggplot(preds, aes(obs,pred)) + 
          stat_binhex(bins=100)+
          xlim(min(preds),max(preds))+ylim(min(preds),max(preds))+
          xlab("Measured Tair (°C)")+
          ylab("Predicted Tair (°C)")+
          geom_abline(slope=1, intercept=0,lty=2)+
          scale_fill_gradientn(name = "data points", 
                               #trans = "log", 
                               #breaks = 10^(0:3),
                               colors=viridis(10)))
  dev.off()
  
  results_global <- rbind(results_global, 
                            data.frame("model"=modeltype,
                                       regressionStats(prd = preds$pred,obs=preds$obs)))
  
  
  
}

validdat <- do.call("rbind",validdat)
validdat <- melt(validdat)




pdf(paste0(vispath,"/LLO_Validation.pdf"),width=6,height=5)
ggplot(data = validdat, aes(x = model, y = value)) +
  geom_boxplot(notch = TRUE)+ facet_grid(variable ~ .,scales="free")
dev.off()

results_LLO$RMSE_global <- round(results_global$RMSE,2)
results_LLO$MAE_global <- round(results_global$MAE,2)
results_LLO$Rsquared_global <- round(results_global$Rsq,2)

write.csv(results_LLO,
          paste0(vispath,"/LLO_validation.csv"),
          row.names = FALSE)

#write.csv(results_global,
#          paste0(vispath,"/LLO_global_validation.csv"))
