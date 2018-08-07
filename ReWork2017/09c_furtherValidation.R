rm(list=ls())
library(lubridate)
library(Rsenal)

load("/media/hanna/data/Antarctica/ReModel2017/data/modeldat/testingDat.RData")
load("/media/hanna/data/Antarctica/ReModel2017/data/modeldat/model_final_rf.RData")
load("/media/hanna/data/Antarctica/ReModel2017/data/modeldat/trainingDat.RData")


testingDat$pred <- predict(model_final,testingDat)
stationid <- 12

stat1 <- testingDat[testingDat$Station==unique(testingDat$Station)[stationid]&
                   year(testingDat$Date)%in%c(2013),]

stat1 <- stat1[order(stat1$Date),]

plot(stat1$Date,stat1$Temperature,type="l")
lines(stat1$Date,stat1$pred,col="green")
lines(stat1$Date,stat1$LST_day,col="red")
lines(stat1$Date,stat1$LST_night,col="blue")

############################


modelpred <- model_final$pred[model_final$pred$mtry==2,]
trainingDat$pred <- modelpred$pred[order(modelpred$rowIndex)]


stationid <- 66
CVstat1 <- trainingDat[trainingDat$Station==unique(trainingDat$Station)[stationid]&
                      year(trainingDat$Date)%in%c(2009),]
CVstat1 <- CVstat1[order(CVstat1$Date),]

plot(CVstat1$Date,CVstat1$Temperature,type="l")
lines(CVstat1$Date,CVstat1$pred,col="green")
lines(CVstat1$Date,CVstat1$LST_day,col="red")
lines(CVstat1$Date,CVstat1$LST_night,col="blue")

