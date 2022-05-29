##----------------------------------------------------code references##

#https://stackoverflow.com/questions/25026944/how-to-ignore-negative-values-while-calculating-statistics-in-r
#https://stackoverflow.com/questions/52638803/how-to-calculate-stocks-daily-returns-in-r-using-data-frame
#https://www.delftstack.com/howto/python/python-max-value-in-list/
#https://www.youtube.com/watch?v=2EpIdnA0pPo
#https://www.marsja.se/how-to-add-a-column-to-dataframe-in-r-with-tibble-dplyr/

#---------------------------------------------------loaded packages#

library(dplyr)
library(ggplot2)
library(lubridate)
library(quantmod)
library(PerformanceAnalytics)

options(digits = 3)


#----------------------------------------------------Question 1a.##

##BTC ROI##

  BTC.sept <- BTC.charts[BTC.charts$date == as.Date("2021-09-01"),c(5)]
  
  BTC.jan <- BTC.charts[BTC.charts$date == as.Date("2016-01-10"),c(5)]
  
  BTC.ROI <- ((BTC.sept - BTC.jan)/BTC.jan) 
  
  rownames(BTC.ROI) <- NULL
  
  colnames(BTC.ROI) <- NULL

##dash roi##

  DASH.sept <- DASH.charts[DASH.charts$date == as.Date("2021-09-01"),c(5)]
  
  DASH.jan <- DASH.charts[DASH.charts$date == as.Date("2016-01-10"),c(5)]

  DASH.ROI <- ((DASH.sept - DASH.jan)/DASH.jan) 
  
  rownames(DASH.ROI) <- NULL
  
  colnames(DASH.ROI) <- NULL

##ETH ROI##

  ETH.sept <- ETH.charts[ETH.charts$date == as.Date("2021-09-01"),c(5)]
  
  ETH.jan <- ETH.charts[ETH.charts$date == as.Date("2016-01-10"),c(5)]

  ETH.ROI <- ((ETH.sept - ETH.jan)/ETH.jan) 
  
  rownames(ETH.ROI) <- NULL
  
  colnames(ETH.ROI) <- NULL

##LTC ROI##

  LTC.sept <- LTC.charts[LTC.charts$date == as.Date("2021-09-01"),c(5)]
  
  LTC.jan <- LTC.charts[LTC.charts$date == as.Date("2016-01-10"),c(5)]

  LTC.ROI <- ((LTC.sept - LTC.jan)/LTC.jan) 
  
  rownames(LTC.ROI) <- NULL
  
  colnames(LTC.ROI) <- NULL

##PPC ROI##

  PPC.sept <- PPC.charts[PPC.charts$date == as.Date("2021-09-01"),c(5)]
  
  PPC.jan <- PPC.charts[PPC.charts$date == as.Date("2016-01-10"),c(5)]

  PPC.ROI <- ((PPC.sept - PPC.jan)/PPC.jan) 
  
  rownames(PPC.ROI) <- NULL
  
  colnames(PPC.ROI) <- NULL

##XLM ROI##

  XLM.sept <- XLM.charts[XLM.charts$date == as.Date("2021-09-01"),c(5)]
  
  XLM.jan <- XLM.charts[XLM.charts$date == as.Date("2016-01-10"),c(5)]

  XLM.ROI <- ((XLM.sept - XLM.jan)/XLM.jan) 
  
  rownames(XLM.ROI) <- NULL
  
  colnames(XLM.ROI) <- NULL

##XRP ROI##

  XRP.sept <- XRP.charts[XRP.charts$date == as.Date("2021-09-01"),c(5)]
  
  XRP.jan <- XRP.charts[XRP.charts$date == as.Date("2016-01-10"),c(5)]
 
  XRP.ROI <- ((XRP.sept - XRP.jan)/XRP.jan) 
  
  rownames(XRP.ROI) <- NULL
  
  colnames(XRP.ROI) <- NULL

##roi matrix try and combine in order to show in one line##

roi.matr <- c(BTC.ROI,DASH.ROI,ETH.ROI, LTC.ROI, PPC.ROI, XLM.ROI, XRP.ROI)

  roi_mat <- matrix(roi.matr, nrow = 1)
  
  coins <- c("BTC","DASH","ETH","LTC","PPC","XLM","XRP")
  
  colnames(roi_mat) <- coins
  
  ROI <- c("ROI")
  
  rownames(roi_mat) <- ROI
  
question1 <- max(roi.matr)*100

question1

##----------------------------------------------------1b greatest mean daily return / 1c lowest sd##

btc.price <- BTC.charts[BTC.charts$date >= "2016-01-10", "close", drop = FALSE]

  n <- nrow(btc.price)
  
  btc.ret <- ((btc.price[2:n,1] - btc.price[1: (n-1), 1]) / btc.price[1: (n-1), 1])

dash.price <- DASH.charts[DASH.charts$date >= "2016-01-10", "close", drop = FALSE]

  n <- nrow(dash.price)
  
  dash.ret <- ((dash.price[2:n,1] - dash.price[1: (n-1), 1]) / dash.price[1: (n-1), 1])
  
ETH.price <- ETH.charts[ETH.charts$date >= "2016-01-10", "close", drop = FALSE]
  
  n <- nrow(ETH.price)
  
  ETH.ret <- ((ETH.price[2:n,1] - ETH.price[1: (n-1), 1]) / ETH.price[1: (n-1), 1])

LTC.price <- LTC.charts[LTC.charts$date >= "2016-01-10", "close", drop = FALSE]
  
  n <- nrow(LTC.price)
  
  LTC.ret <- ((LTC.price[2:n,1] - LTC.price[1: (n-1), 1]) / LTC.price[1: (n-1), 1])
  
PPC.price <- PPC.charts[PPC.charts$date >= "2016-01-10", "close", drop = FALSE]
  
  n <- nrow(PPC.price)
  
  PPC.ret <- ((PPC.price[2:n,1] - PPC.price[1: (n-1), 1]) / PPC.price[1: (n-1), 1])
  
XLM.price <- XLM.charts[XLM.charts$date >= "2016-01-10", "close", drop = FALSE]
  
  n <- nrow(XLM.price)
  
  XLM.ret <- ((XLM.price[2:n,1] - XLM.price[1: (n-1), 1]) / XLM.price[1: (n-1), 1])
  
XRP.price <- XRP.charts[XRP.charts$date >= "2016-01-10", "close", drop = FALSE]
  
  n <- nrow(XRP.price)
  
  XRP.ret <- ((XRP.price[2:n,1] - XRP.price[1: (n-1), 1]) / XRP.price[1: (n-1), 1])
  

##----------------------------------------------------mean##

question2 <- max(c(mean(btc.ret),mean(dash.ret),mean(ETH.ret),mean(LTC.ret),mean(PPC.ret),mean(XLM.ret),mean(XRP.ret)))*100

question2
##----------------------------------------------------SD##

question3 <- min(c(sd(btc.ret),sd(dash.ret),sd(ETH.ret),sd(LTC.ret),sd(PPC.ret),sd(XLM.ret),sd(XRP.ret)))*100

question3
##----------------------------------------------------2A portfolio value##

##port 1## 
port1.val <- (5000/BTC.charts[BTC.charts$date == "2016-01-10", "close"]) * 
  BTC.charts[BTC.charts$date == "2021-09-01", "close"]

##port 2##

xrp.value <- (2500/XRP.charts[XRP.charts$date == "2016-01-10", "close"]) * 
  XRP.charts[XRP.charts$date == "2021-09-01", "close"]

ltc.value <- (2500/LTC.charts[LTC.charts$date == "2016-01-10", "close"]) * 
  LTC.charts[LTC.charts$date == "2021-09-01", "close"]

port2.val <- xrp.value + ltc.value

##port 3##

eth.value <- (1250/ETH.charts[ETH.charts$date == "2016-01-10", "close"]) * 
  ETH.charts[ETH.charts$date == "2021-09-01", "close"]

PPC.value <- (1250/PPC.charts[PPC.charts$date == "2016-01-10", "close"]) * 
  PPC.charts[PPC.charts$date == "2021-09-01", "close"]

DASH.value <- (1250/DASH.charts[DASH.charts$date == "2016-01-10", "close"]) * 
  DASH.charts[DASH.charts$date == "2021-09-01", "close"]

XLM.value <- (1250/XLM.charts[XLM.charts$date == "2016-01-10", "close"]) * 
  XLM.charts[XLM.charts$date == "2021-09-01", "close"]

port3.val <- eth.value+PPC.value+DASH.value+XLM.value

##----------------------------------------------------final 2a##

question4 <- max(c(port1.val,port3.val,port2.val))

question4

##----------------------------------------------------2b for port1##

port1.df <- cbind(btc.price)

n <- nrow(port1.df)

port1.plz <- ((5000/port1.df[1, 1]) * port1.df[2:n, 1]) 

question5 <- max(port1.plz)

question5


##----------------------------------------------------2b for port2## 

dates <- BTC.charts[BTC.charts$date >= "2016-01-10", "date", drop = FALSE]

port2.df <- cbind(XRP.price,LTC.price)

n <- nrow(port2.df)

port2.plz <- ((2500/port2.df[1, 1]) * port2.df[2:n, 1]) + ((2500/port2.df[1, 2]) * port2.df[2:n, 2])

question6 <- max(port2.plz)

question6


##----------------------------------------------------2b for port3## - good one you legend you

port3.df <- cbind(ETH.price,PPC.price, dash.price, XLM.price)

n <- nrow(port3.df)

port3.plz <- ((1250/port3.df[1, 1]) * port3.df[2:n, 1]) + ((1250/port3.df[1, 2]) * port3.df[2:n, 2]) + ((1250/port3.df[1, 3]) * port3.df[2:n, 3]) + ((1250/port3.df[1, 4]) * port3.df[2:n, 4])

question7 <- max(port3.plz)

question7

dates <- BTC.charts[BTC.charts$date >= "2016-01-11", "date", drop = FALSE]
price.graph <- cbind(dates,port1.plz,port2.plz,port3.plz)
plot(price.graph$date,price.graph$port1.plz, type = "l", xlab = "Date (years)", ylab = "Portfolio 1")
plot(price.graph$date,price.graph$port2.plz, type = "l", xlab = "Date (years)", ylab = "Portfolio 2")
plot(price.graph$date,price.graph$port3.plz, type = "l", xlab = "Date (years)", ylab = "Portfolio 3")

##----------------------------------------------------2c correlation port 1 and port 2## Not needed?
df1 <- data.frame(rowMeans(port1.df))

n <- nrow(port1.df)

port1.ret <- ((df1[2:n,1] - df1[1 :(n-1),1])/ df1[1:(n-1),1])

df2 <- data.frame(rowSums(port2.df))

n <- nrow(df2)

port2.ret <- ((df2[2:n,1] - df2[1: (n-1), 1]) / df2[1: (n-1), 1])

port1_2cor <- cor(port1.ret,port2.ret)

plot(btc.ret,port2.ret, xlab = "Portfolio 1 Return", ylab = "Portfolio 2 Return")

##----------------------------------------------------2c correlation part 1 and port 3##

df3 <- data.frame(rowMeans(port3.df))

n <- nrow(df3)

port3.ret <- ((df3[2:n,1] - df3[1: (n-1), 1]) / df3[1: (n-1), 1])

question8 <- cor(btc.ret,port3.ret)

question8

plot(btc.ret,port3.ret, xlab = "Portfolio 1 Return", ylab = "Portfolio 3 Return")

#2d---------------------------------------------------------------------------------------#

xrp.value2 <- (2500/XRP.charts[XRP.charts$date == "2016-01-10", c(5)])
LTC.value2 <- (2500/LTC.charts[LTC.charts$date == "2016-01-10", c(5)])
LTC.charts <- LTC.charts[LTC.charts$date >= "2016-01-10",]
XRP.charts <- XRP.charts[XRP.charts$date >= "2016-01-10",]
BTC.charts <- BTC.charts[BTC.charts$date >= "2016-01-10",]
BTC.charts$port2.vaaalue <- c((xrp.value2 * XRP.charts$close) + (LTC.value2 * LTC.charts$close))

n <- nrow(BTC.charts)

LUCK <- data.frame((BTC.charts[2:n,5] - BTC.charts[1: (n-1), 5]) / BTC.charts[1: (n-1), 5],(BTC.charts[2:n,6] - BTC.charts[1: (n-1), 6]) / BTC.charts[1: (n-1), 6])

LUCK.neg <- LUCK[LUCK$X.BTC.charts.2.n..5....BTC.charts.1..n...1...5...BTC.charts.1..n... < 0, c(2)]

question9 <- mean(LUCK.neg)*100

port1_3 <- data.frame(port1.ret,port3.ret)

port1_3pos <- port1_3[port1_3[,1] >= 0,]

question10 <- mean(port1_3pos$port3.ret)*100


##----------------------------------------------------2d port 1 & 2 neg# FOR QUESTION 9 
question9

##----------------------------------------------------2d port 1 & 3 pos# FOR QUESTION 10
question10

#######################################################
################### FINAL ANSWERS ####################
######################################################

question1

question2

question3

question4

question5

question6

question7

question8

question9

question10

#######################################################
################### QUESTION 3 #######################
#######################################################

##question 3 - 1a - not different - ETH has highest ROI

BTC.sept <- rowMeans(BTC.charts[BTC.charts$date == as.Date("2021-09-01"),c(3:4)])

BTC.jan <- rowMeans(BTC.charts[BTC.charts$date == as.Date("2016-01-10"),c(3:4)])

BTC.ROI <- ((BTC.sept - BTC.jan)/BTC.jan) 

rownames(BTC.ROI) <- NULL

colnames(BTC.ROI) <- NULL

##dash roi##

DASH.sept <- rowMeans(DASH.charts[DASH.charts$date == as.Date("2021-09-01"),c(3:4)])

DASH.jan <- rowMeans(DASH.charts[DASH.charts$date == as.Date("2016-01-10"),c(3:4)])

DASH.ROI <- ((DASH.sept - DASH.jan)/DASH.jan) 

rownames(DASH.ROI) <- NULL

colnames(DASH.ROI) <- NULL

##ETH ROI##

ETH.sept <- rowMeans(ETH.charts[ETH.charts$date == as.Date("2021-09-01"),c(3:4)])

ETH.jan <- rowMeans(ETH.charts[ETH.charts$date == as.Date("2016-01-10"),c(3:4)])

ETH.ROI <- ((ETH.sept - ETH.jan)/ETH.jan) 

rownames(ETH.ROI) <- NULL

colnames(ETH.ROI) <- NULL

##LTC ROI##

LTC.sept <- rowMeans(LTC.charts[LTC.charts$date == as.Date("2021-09-01"),c(3:4)])

LTC.jan <- rowMeans(LTC.charts[LTC.charts$date == as.Date("2016-01-10"),c(3:4)])

LTC.ROI <- ((LTC.sept - LTC.jan)/LTC.jan) 

rownames(LTC.ROI) <- NULL

colnames(LTC.ROI) <- NULL

##PPC ROI##

PPC.sept <- rowMeans(PPC.charts[PPC.charts$date == as.Date("2021-09-01"),c(3:4)])

PPC.jan <- rowMeans(PPC.charts[PPC.charts$date == as.Date("2016-01-10"),c(3:4)])

PPC.ROI <- ((PPC.sept - PPC.jan)/PPC.jan) 

rownames(PPC.ROI) <- NULL

colnames(PPC.ROI) <- NULL

##XLM ROI##

XLM.sept <- rowMeans(XLM.charts[XLM.charts$date == as.Date("2021-09-01"),c(3:4)])

XLM.jan <- rowMeans(XLM.charts[XLM.charts$date == as.Date("2016-01-10"),c(3:4)])

XLM.ROI <- ((XLM.sept - XLM.jan)/XLM.jan) 

rownames(XLM.ROI) <- NULL

colnames(XLM.ROI) <- NULL

##XRP ROI##

XRP.sept <- rowMeans(XRP.charts[XRP.charts$date == as.Date("2021-09-01"),c(3:4)])

XRP.jan <- rowMeans(XRP.charts[XRP.charts$date == as.Date("2016-01-10"),c(3:4)])

XRP.ROI <- ((XRP.sept - XRP.jan)/XRP.jan) 

rownames(XRP.ROI) <- NULL

colnames(XRP.ROI) <- NULL

c(XRP.ROI,BTC.ROI,DASH.ROI,ETH.ROI,LTC.ROI,PPC.ROI,XLM.ROI)*100

##question 3 - 1b/c - ETH still has the highest mean / BTC still has lowest sd

btc.price <- data.frame(rowMeans(BTC.charts[BTC.charts$date >= "2016-01-10", c(3:4), drop = FALSE]))

n <- nrow(btc.price)

btc.ret <- ((btc.price[2:n,1] - btc.price[1: (n-1), 1]) / btc.price[1: (n-1), 1])

dash.price <- data.frame(rowMeans(DASH.charts[DASH.charts$date >= "2016-01-10", c(3:4), drop = FALSE]))

n <- nrow(dash.price)

dash.ret <- ((dash.price[2:n,1] - dash.price[1: (n-1), 1]) / dash.price[1: (n-1), 1])

ETH.price <- data.frame(rowMeans(ETH.charts[ETH.charts$date >= "2016-01-10", c(3:4), drop = FALSE]))

n <- nrow(ETH.price)

ETH.ret <- ((ETH.price[2:n,1] - ETH.price[1: (n-1), 1]) / ETH.price[1: (n-1), 1])

LTC.price <- data.frame(rowMeans(LTC.charts[LTC.charts$date >= "2016-01-10", c(3:4), drop = FALSE]))

n <- nrow(LTC.price)

LTC.ret <- ((LTC.price[2:n,1] - LTC.price[1: (n-1), 1]) / LTC.price[1: (n-1), 1])

PPC.price <- data.frame(rowMeans(PPC.charts[PPC.charts$date >= "2016-01-10", c(3:4), drop = FALSE]))

n <- nrow(PPC.price)

PPC.ret <- ((PPC.price[2:n,1] - PPC.price[1: (n-1), 1]) / PPC.price[1: (n-1), 1])

XLM.price <- data.frame(rowMeans(XLM.charts[XLM.charts$date >= "2016-01-10", c(3:4), drop = FALSE]))

n <- nrow(XLM.price)

XLM.ret <- ((XLM.price[2:n,1] - XLM.price[1: (n-1), 1]) / XLM.price[1: (n-1), 1])

XRP.price <- data.frame(rowMeans(XRP.charts[XRP.charts$date >= "2016-01-10", c(3:4), drop = FALSE]))

n <- nrow(XRP.price)

XRP.ret <- ((XRP.price[2:n,1] - XRP.price[1: (n-1), 1]) / XRP.price[1: (n-1), 1])

mean.ret <- max(c(mean(btc.ret),mean(dash.ret),mean(ETH.ret),mean(LTC.ret),mean(PPC.ret),mean(XLM.ret),mean(XRP.ret)))*100

print(mean.ret)

sd.ret <- min(c(sd(btc.ret),sd(dash.ret),sd(ETH.ret),sd(LTC.ret),sd(PPC.ret),sd(XLM.ret),sd(XRP.ret)))*100

print(sd.ret)

##question 3 - 2a - SAME port 3 is still the most valuable 

##2A portfolio value##

##port 1## 
port1.val <- rowMeans(5000/BTC.charts[BTC.charts$date == "2016-01-10", c(3:4)]) * 
  rowMeans(BTC.charts[BTC.charts$date == "2021-09-01", c(3:4)])

##port 2##

xrp.value <- rowMeans((2500/XRP.charts[XRP.charts$date == "2016-01-10", c(3:4)])) * 
  rowMeans(XRP.charts[XRP.charts$date == "2021-09-01", c(3:4)])

ltc.value <- rowMeans((2500/LTC.charts[LTC.charts$date == "2016-01-10", c(3:4)])) * 
  rowMeans(LTC.charts[LTC.charts$date == "2021-09-01", c(3:4)])

port2.val <- xrp.value + ltc.value

##port 3##

eth.value <- rowMeans((1250/ETH.charts[ETH.charts$date == "2016-01-10", c(3:4)])) * 
  rowMeans(ETH.charts[ETH.charts$date == "2021-09-01", c(3:4)])

PPC.value <- rowMeans((1250/PPC.charts[PPC.charts$date == "2016-01-10", c(3:4)])) * 
  rowMeans(PPC.charts[PPC.charts$date == "2021-09-01", c(3:4)])

DASH.value <- rowMeans((1250/DASH.charts[DASH.charts$date == "2016-01-10", c(3:4)])) * 
  rowMeans(DASH.charts[DASH.charts$date == "2021-09-01", c(3:4)])

XLM.value <- rowMeans((1250/XLM.charts[XLM.charts$date == "2016-01-10", c(3:4)])) * 
  rowMeans(XLM.charts[XLM.charts$date == "2021-09-01", c(3:4)])

port3.val <- eth.value+PPC.value+DASH.value+XLM.value

max(c(port1.val,port3.val,port2.val))
##question 3 - 2b - same - port 3 still has highest value  
##port 1## 
port1.df <- cbind(btc.price)

n <- nrow(port1.df)

port1.plz <- ((5000/port1.df[1, 1]) * port1.df[2:n, 1]) 

max(port1.plz)

port2.df <- cbind(XRP.price,LTC.price)

n <- nrow(port2.df)

port2.plz <- ((2500/port2.df[1, 1]) * port2.df[2:n, 1]) + ((2500/port2.df[1, 2]) * port2.df[2:n, 2])

max(port2.plz)

port3.df <- cbind(ETH.price,PPC.price, dash.price, XLM.price)

n <- nrow(port3.df)

port3.plz <- ((1250/port3.df[1, 1]) * port3.df[2:n, 1]) + ((1250/port3.df[1, 2]) * port3.df[2:n, 2]) + ((1250/port3.df[1, 3]) * port3.df[2:n, 3]) + ((1250/port3.df[1, 4]) * port3.df[2:n, 4])

max(port3.plz)

##question 3 - 2c - port 2 still has a higher correlation
df1 <- data.frame(rowMeans(port1.df))

n <- nrow(port1.df)

port1.ret <- ((df1[2:n,1] - df1[1 :(n-1),1])/ df1[1:(n-1),1])

df2 <- data.frame(rowMeans(port2.df))

n <- nrow(df2)

port2.ret <- ((df2[2:n,1] - df2[1: (n-1), 1]) / df2[1: (n-1), 1])

cor(port1.ret,port2.ret)

df3 <- data.frame(rowMeans(port3.df))

n <- nrow(df3)

port3.ret <- ((df3[2:n,1] - df3[1: (n-1), 1]) / df3[1: (n-1), 1])

cor(btc.ret,port3.ret)

##question 3 - 2d - DIFFERENT, PORT 2 is less adversly effected on negative days than port 3 

port1_3 <- data.frame(port1.ret,port3.ret)

port1_3pos <- port1_3[port1_3[1] > 0,]

mean(port1_3pos$port3.ret)*100

question10

xrp.value2 <- rowMeans((2500/XRP.charts[XRP.charts$date == "2016-01-10", c(3:4)]))
LTC.value2 <- rowMeans((2500/LTC.charts[LTC.charts$date == "2016-01-10", c(3:4)]))
LTC.charts <- LTC.charts[LTC.charts$date >= "2016-01-10",]
XRP.charts <- XRP.charts[XRP.charts$date >= "2016-01-10",]
BTC.charts <- BTC.charts[BTC.charts$date >= "2016-01-10",]
BTC.charts$port2.vaaalue <- c((xrp.value2 * XRP.charts$close) + (LTC.value2 * LTC.charts$close))

n <- nrow(BTC.charts)

LUCK <- data.frame((BTC.charts[2:n,5] - BTC.charts[1: (n-1), 5]) / BTC.charts[1: (n-1), 5],(BTC.charts[2:n,6] - BTC.charts[1: (n-1), 6]) / BTC.charts[1: (n-1), 6])

LUCK.neg <- LUCK[LUCK$X.BTC.charts.2.n..5....BTC.charts.1..n...1...5...BTC.charts.1..n... < 0, c(2)]

question9 <- mean(LUCK.neg)*100

question9

#######################################################
######################## END #########################
#######################################################

#im still new to coding so I hope this isn't too messy
