library(lubridate)
library(purrr)
library(caret)
library(dplyr)

library(forecast)
library(fpp2)
library(tsintermittent)

df <- read.csv('daily_orders.csv')

# convert time to Date
df[,'time'] <- map(df['time'], ymd) # ???
#df$time <- as.POSIXct(df$time, tz = 'UTC')

# random number
i = sample(unique(df[,'itemID']), 1)
item1 <- df[df$itemID == 5117, ]

# convert to time series for one column
f = findfrequency(item1[,'order'])
item1[,'order'] <- ts(item1[,'order'], frequency = 1)


# forecast up to 2018-07-13 23:59:59
target = as.POSIXct.default('2018-07-14', tz = 'UTC')
# example
delta = as.integer(target - last_day_of_item_purchase)
t = 184 - dim(item1)[1]

#
autoplot(item1[,'order']) +
  ggtitle(paste('amount of orders for Item', i)) +
  xlab('time') +
  ylab('amount of orders')

plot(forecast(object = item1[,'order'], h = 14))

# seasonal naive
snaive(item1[,'order'], h = 14)
snaive(item1[,'order'], h = 14) %>% autoplot()

# croston method
croston(item1[,'order'], h = 14)
croston(item1[,'order'], h = 14) %>% autoplot()

# stl forecast
item1[,'order'] %>% stlf(h = 14)
item1[,'order'] %>% stlf(h = 14) %>% autoplot()

# ets
item1[,'order'] %>% forecast(h = 14)
item1[,'order'] %>% forecast(h = 14) %>% autoplot()

# tsb
tsb(item1[,'order'], h = 14)
tsb(item1[,'order'], h = 14, outplot = TRUE)

item1[,'order']
