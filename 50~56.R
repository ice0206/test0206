rm(list = ls())

etf6 <- read.delim('0050-56.txt', header = T)
#
head(etf6)
tail(etf6)

etf6 <- etf6[, -2]
colnames(etf6) <- c('id', 'date', 'price')
head(etf6)

library(pacman)
p_load(reshape2)

etf6.l <- dcast(etf6, date~id)
head(etf6.l)

etf6.l <- na.omit(etf6.l)
head(etf6.l)

str(etf6.l)

# convert into xts 轉成時間序列
etf6.xts <- xts(etf6.l[, -1], order.by = as.Date(as.character(etf6.l$date), format = '%Y%m%d'))
class(etf6.xts)
head(etf6.xts)

# SIT 
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)
#
data <- new.env()
# 1. prices; 2. weight; 3. execution.price
# buy and hold
etf52 <- etf6.xts$`52`
head(etf52)
data$prices = data$weight = data$execution.price = etf52
data$weight[] <- 1
data$execution.price[] <- NA
names(data)
#
model <- list()
#
etf3 <- etf6.xts[, 1:3]
head(etf3)
names(etf3)
colnames(etf3) <- c('etf50','etf52','etf56')
names(etf3)
md = 50
i='etf50'
for (i in names(etf3)) {
  data$prices = data$weight = data$execution.price = etf3[, i]
  data$weight[] <- 1
  data$execution.price[] <- NA
  model[[i]] <- bt.run(data)
  sma <- SMA(data$prices, md)
  data$weight[] <- iif(data$prices >= sma, 1, 0)
  i <- paste(i, '.sam.cross', sep = '')
  model[[i]] <- bt.run(data)
  }
strategy.performance.snapshoot(model, T)  
#
prices <- data$prices
n <- ncol(prices)
model <- list()
model$buy.hold <- bt.run(data)
names(model$buy.hold)

# moving average
md <- 50
sma <- SMA(data$prices, md)
data$weight[] <- NA
data$weight[] <- iif(data$prices >= sma, 1, 0)
data$symbolnames <- 'tw56'
#
model$sma.cross <- bt.run(data, trade.summary = T)
#
strategy.performance.snapshoot(model, T)
