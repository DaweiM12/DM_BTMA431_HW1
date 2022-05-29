##question 3 - 1a - not different

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

##question 3 - 1b/c - DIFFERENT

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

##question 3 - 2a - DIFFERENT

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
##question 3 - 2b - DIFFERENT  
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

##question 3 - 2c - changed
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

##question 3 - 2d 

port1_3 <- data.frame(port1.ret,port3.ret)

port1_3pos <- port1_3[port1_3[1] >= 0,]

mean(port1_3pos$port3.ret)*100

port1_3neg <- port1_3[port1_3[,1] <= 0,]

mean(port1_3neg$port3.ret)*100

port1_2 <- data.frame(btc.ret,port2.ret)

port1_2neg <- data.frame(port1_2[port1_2[,1] < 0,])

mean(port1_2neg[,2])*100

port1_2pos <- data.frame(port1_2[port1_2[,1] > 0,])

mean(port1_2pos[,2])*100
