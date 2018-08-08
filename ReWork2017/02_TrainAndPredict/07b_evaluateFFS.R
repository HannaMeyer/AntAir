library(Rsenal)
library(ggplot2)
library(viridis)
load("/media/hanna/data/Antarctica/ReModel2017/data/modeldat/ffs_model_gbm_withVIS.RData")
predictions_gbm <- ffs_model$pred[ffs_model$pred$shrinkage==ffs_model$bestTune$shrinkage&
                 ffs_model$pred$interaction.depth==ffs_model$bestTune$interaction.depth&
                 ffs_model$pred$n.trees==ffs_model$bestTune$n.trees&
                 ffs_model$pred$n.minobsinnode==ffs_model$bestTune$n.minobsinnode,
                 c("pred","obs")]
regressionStats(predictions_gbm$pred,predictions_gbm$obs)

ggplot(predictions_gbm, aes(obs,pred)) + 
  stat_binhex(bins=100)+
  xlim(min(predictions_gbm),max(predictions_gbm))+ylim(min(predictions_gbm),max(predictions_gbm))+
  xlab("Measured Tair (°C)")+
  ylab("Predicted Tair (°C)")+
  geom_abline(slope=1, intercept=0,lty=2)+
  scale_fill_gradientn(name = "data points", colors=viridis(10))



load("/media/hanna/data/Antarctica/ReModel2017/data/modeldat/ffs_model_rf_withVIS.RData")
predictions_rf <- ffs_model$pred[ffs_model$pred$mtry==ffs_model$bestTune$mtry,c("pred","obs")]
regressionStats(predictions_rf$pred,predictions_rf$obs)

ggplot(predictions_rf, aes(obs,pred)) + 
  stat_binhex(bins=100)+
  xlim(min(predictions_rf),max(predictions_rf))+ylim(min(predictions_rf),max(predictions_rf))+
  xlab("Measured Tair (°C)")+
  ylab("Predicted Tair (°C)")+
  geom_abline(slope=1, intercept=0,lty=2)+
  scale_fill_gradientn(name = "data points", colors=viridis(10))


load("/media/hanna/data/Antarctica/ReModel2017/data/modeldat/ffs_model_pls_withVIS.RData")
predictions_pls <- ffs_model$pred[ffs_model$pred$ncomp==ffs_model$bestTune$ncomp,c("pred","obs")]
regressionStats(predictions_pls$pred,predictions_pls$obs)


ggplot(predictions_pls, aes(obs,pred)) + 
  stat_binhex(bins=100)+
  xlim(min(predictions_pls),max(predictions_pls))+ylim(min(predictions_pls),max(predictions_pls))+
  xlab("Measured Tair (°C)")+
  ylab("Predicted Tair (°C)")+
  geom_abline(slope=1, intercept=0,lty=2)+
  scale_fill_gradientn(name = "data points", colors=viridis(10))
