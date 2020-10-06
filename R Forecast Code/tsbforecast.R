library(lubridate)
library(purrr)
library(caret)
library(dplyr)

library(forecast)
library(fpp2)
library(tsintermittent)

df <- read.csv('daily_orders.csv')

# convert time to Date
df[,'time'] <- map(df['time'], ymd) 

# tsb
fcast <- tsb(item1[,'order'], h = 14, outplot = TRUE)

# forecast for all items

demandPrediction <- list()
j = 1
for (i in unique(df[,'itemID'])) {
  
  item <- df[df$itemID == i, 'order']
  f <- findfrequency(item)
  item <- ts(item, frequency = f)
  fcast <- tsb(item, h = 14)
  demPred <- sum(fcast$frc.out)
  
  demandPrediction[[j]] <- demPred
  j <- j + 1
  
}

itemID <- unique(df[,'itemID'])
demandPrediction <- unlist(demandPrediction)
pred <- data.frame(itemID, demandPrediction)

write.csv(pred, 'Uni_APU_2_tsb2.csv', row.names = FALSE)




# random number
i = sample(unique(df[,'itemID']), 1)
item1 <- df[df$itemID == 5117, ]

f <- tsb(item1[,'order'], outplot = TRUE)

# convert to time series for one column
f = findfrequency(item1[,'order'])
item1[,'order'] <- ts(item1[,'order'], frequency = f)

autoplot(item1[,'order']) +
  ggtitle(paste('amount of orders for Item', i)) +
  xlab('time') +
  ylab('amount of orders')
