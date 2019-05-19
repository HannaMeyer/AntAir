rm(list=ls())
library(lubridate)
library(Rsenal)

load("/media/hanna/data/Antarctica/ReModel2017/data/modeldat/testingDat.RData")
model_rf <- get(load("/media/hanna/data/Antarctica/ReModel2017/data/modeldat/model_final_rf.RData"))
model_lin <- get(load("/media/hanna/data/Antarctica/ReModel2017/data/modeldat/model_final_linear.RData"))
load("/media/hanna/data/Antarctica/ReModel2017/data/modeldat/trainingDat.RData")

testingDat$meanLST <- (testingDat$LST_day+testingDat$LST_night)/2
testingDat$pred_rf <- predict(model_rf,testingDat)
testingDat$pred_lin <- predict(model_lin,testingDat)
stationid <- 30

stat1 <- testingDat[testingDat$Station==unique(testingDat$Station)[stationid]&
                   year(testingDat$Date)%in%c(2012),]

stat1 <- stat1[order(stat1$Date),]

plot(stat1$Date,stat1$Temperature,type="l")
lines(stat1$Date,stat1$pred_rf,col="green")
lines(stat1$Date,stat1$pred_lin,col="red")


############################

trainingDat$meanLST <- (trainingDat$LST_day+trainingDat$LST_night)/2
modelpred <- model_rf$pred[model_rf$pred$mtry==2,]
trainingDat$pred_rf <- modelpred$pred[order(modelpred$rowIndex)]
trainingDat$pred_lin <- model_lin$pred$pred[order(model_lin$pred$rowIndex)]

stationid <- 22
CVstat1 <- trainingDat[trainingDat$Station==unique(trainingDat$Station)[stationid]&
                      year(trainingDat$Date)%in%c(2011),]
CVstat1 <- CVstat1[order(CVstat1$Date),]

plot(CVstat1$Date,CVstat1$Temperature,type="l")
lines(CVstat1$Date,CVstat1$pred_rf,col="green")
lines(CVstat1$Date,CVstat1$pred_lin,col="red")


