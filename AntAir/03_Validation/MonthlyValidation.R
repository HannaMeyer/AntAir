
rm(list=ls())
library(caret)
library(raster)
library(rgdal)
library(Rsenal)
library(viridis)
library(lubridate)
library(gridExtra)
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
modeltype <- "gbm"


model <- get(load(paste0(modelpath,"model_final_",modeltype,".RData")))
testdat <- get(load(paste0(modelpath,"testingDat.RData")))
testdat$LSTmean <- (testdat$LST_day+testdat$LST_night)/2

if(modeltype=="nnet"||modeltype=="pls"){
  scaling <- function(predictors,scaleStats){
    for (i in 1:ncol(predictors)){
      rowID <- which(row.names(scaleStats)==names(predictors)[i])
      predictors[,i] <- (predictors[,i]-scaleStats$mean[rowID])/scaleStats$sd[rowID]
    }
    return(predictors)
  }
  testdat[,which(names(testdat)%in%names(model$trainingData))] <- data.frame(
    scaling(testdat[,which(names(testdat)%in%names(model$trainingData))],model$scaleStats))
}



testdat$prediction <- predict(model,testdat)
testdat$year <- year(testdat$Date)
testdat$month <- month(testdat$Date)
testdat$error <- abs(testdat$prediction-testdat$Temperature)
testdat_agg_month <- aggregate(testdat[,c("Temperature","prediction")],
                         by=list(testdat$Station,testdat$month),mean)
testdat_agg_month$error <- abs(testdat_agg_month$prediction-testdat_agg_month$Temperature)

testdat_agg_year <- aggregate(testdat[,c("Temperature","prediction")],
                               by=list(testdat$Station,testdat$year),mean)
testdat_agg_year$error <- abs(testdat_agg_year$prediction-testdat_agg_year$Temperature)





testdat_melt <- rbind(data.frame("error"=testdat$error,"agg"="Day"),
                      data.frame("error"=testdat_agg_month$error,"agg"="Month"),
                      data.frame("error"=testdat_agg_year$error,"agg"="Year"))


annotations <- data.frame(xpos=-Inf,ypos=Inf,annotateText = c("(a)"),
                          hjustvar = -0.3,vjustvar = 1.5)
pdf(paste0(vispath,"/error_agg.pdf"),width=4,height=3)
p1 <- ggplot(data = testdat_melt, aes(x = agg, y = error)) +
  geom_boxplot(outlier.shape = NA)+
  xlab("Aggregation")+ylim (0,8.5)+
  ylab("Absolute Error (°C)")+
  theme_bw()+
  geom_text(data=annotations,aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label=annotateText),
            size = 3)
dev.off()

Sys.setlocale("LC_TIME", "en_US.UTF-8")
testdat$month <- factor(months(testdat$Date,abbreviate = TRUE),
                        levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
#testdat$month <- factor(testdat$month)


annotations <- data.frame(xpos=-Inf,ypos=Inf,annotateText = c("(b)"),
                          hjustvar = -0.3,vjustvar = 1.5)
pdf(paste0(vispath,"/monthlyerror.pdf"),width=4,height=3)
p2 <- ggplot(data = testdat, aes(x = month, y = error)) +
  geom_boxplot(notch = TRUE,outlier.shape = NA)+
  xlab("Month")+ylim (0,12)+
  ylab("Absolute Error (°C)")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme_bw()+
  geom_text(data=annotations,aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label=annotateText),
            size=3)
dev.off()





pdf(paste0(vispath,"/monthly_yearly_error.pdf"),width=7,height=3)
grid.arrange(p1,p2,ncol=2)
dev.off()
#testdat$DEM_agg <- factor(round(testdat$DEM,-2))
#pdf(paste0(vispath,"/eleverror.pdf"),width=6,height=5)
#ggplot(data = testdat, aes(x = DEM_agg, y = error)) +
#  geom_boxplot(notch = TRUE,outlier.shape = NA)+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#  xlab("Elevation (m)")+ylim (0,15)+
#  ylab("Absolute Error (°C)")
#dev.off()

#testdat$year<-factor(testdat$year)
#pdf(paste0(vispath,"/yearlyerror.pdf"),width=6,height=5)
#ggplot(data = testdat, aes(x = year, y = error)) +
#  geom_boxplot(notch = TRUE)
#dev.off()
