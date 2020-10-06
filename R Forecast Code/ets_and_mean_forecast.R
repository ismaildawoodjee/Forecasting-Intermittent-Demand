library(lubridate)
library(purrr)
library(caret)
library(dplyr)

library(forecast)
library(fpp2)
library(tsintermittent)

df <- read.csv('daily_orders.csv')

df[,'time'] <- map(df['time'], ymd) 

demandPrediction <- list()
#demandPrediction2 <- list()

j = 1
for (i in unique(df[,'itemID'])) {
  
  item <- df[df$itemID == i, 'order']
  f <- findfrequency(item)
  item <- ts(item, frequency = f)
  
  fcast <- forecast(item, h = 14)
  #fcast2 <- tsb(item, h = 14)
  fcast$mean <- pmax(fcast$mean, 0) #make forecasts non-negative, i.e. truncate at 0; do the same for mean forecasts later
  demPred <- sum(fcast$mean)
  
  #tsbsum <- sum(fcast2$frc.out)
  #demPred2 <- (demPred + tsbsum)/2
  
  demandPrediction[[j]] <- demPred
  #demandPrediction2[[j]] <- demPred2
  
  j <- j + 1
  
}

itemID <- unique(df[,'itemID'])

demandPrediction <- unlist(demandPrediction)
pred <- data.frame(itemID, demandPrediction)

#demandPrediction2 <- unlist(demandPrediction2)
#pred2 <- data.frame(itemID, demandPrediction2)

write.csv(pred, 'Uni_APU_2_ets.csv', row.names = FALSE)
#write.csv(pred2, 'Uni_APU_2_mean.csv', row.names = FALSE)



fcast <- forecast(item1[,'order'], h = 14)
facst2 <- tsb(item1[,'order'], h = 14)
