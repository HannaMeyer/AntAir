
rm(list=ls())
library(caret)
library(raster)
library(rgdal)
library(Rsenal)
library(viridis)
library(lubridate)
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
modeltype <- "rf"


model <- get(load(paste0(modelpath,"model_final_",modeltype,".RData")))
testdat <- get(load(paste0(modelpath,"testingDat.RData")))

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


pdf(paste0(vispath,"/error_agg.pdf"),width=4,height=3)
ggplot(data = testdat_melt, aes(x = agg, y = error)) +
  geom_boxplot(outlier.shape = NA)+
  xlab("Aggregation")+ylim (0,8.5)+
  ylab("Absolute Error (°C)")
dev.off()

Sys.setlocale("LC_TIME", "en_US.UTF-8")
testdat$month <- factor(months(testdat$Date,abbreviate = TRUE),
                        levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
#testdat$month <- factor(testdat$month)
pdf(paste0(vispath,"/monthlyerror.pdf"),width=4,height=3)
ggplot(data = testdat, aes(x = month, y = error)) +
  geom_boxplot(notch = TRUE,outlier.shape = NA)+
  xlab("Month")+ylim (0,12)+
  ylab("Absolute Error (°C)")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
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
