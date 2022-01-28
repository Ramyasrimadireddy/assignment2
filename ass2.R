library(readr)
library(tseries)
s=read_csv("monthly-sunspots.txt")
View(s)
class(s)
colSums(is.na(s))
num=s$Sunspots

head(s)
summary(s)
p=matrix(s$Sunspots,nrow=2820,ncol=1)
p=as.vector(t(p))
p=ts(p,start=c(1,1),end=c(20,12),frequency=12)
plot(p)


data=ts(s[,2],start=c(1,1),frequency=12)
plot(data)
plot(decompose(data))
#mean
abline(reg = lm(data~time(data)))
summary(data)

#trend
plot(aggregate(data,FUN = mean), xlab= "month", ylab= "Sunspots", main = "Sunpots vs month (Original Dataset)" )
#sesonality 
boxplot(data~cycle(data),  xlab= "month", ylab= "Sunspots", main = "Sunpots vs month (Original Dataset)")

#differencing 
#1st degree
q<- diff(log(data))
plot(q,xlab= "month", ylab= "Sunspots", main = "Sunspots vs month(Original Dataset)")
summary(q)

#second degree
d.s<-diff(q)
plot(d.s,  xlab= "month", ylab= "Sunspots", main = "Sunspots vs month")
#summary 
summary(d.s)


adf.test(data,alternative = "stationary")


acf(data, main = "Original Seires")
pacf(data, main = "Original Seires")
acf(q , main = "1st differenced Seires")#error
pacf(q, main = "1st differenced Seires")#error
acf(d.s , main = "2nd differenced Seires")#error
pacf(d.s , main = "2nd differenced Seires")#error



arima(data,order=c(0,0,1))
arima(data,order=c(1,1,1))
arima(data,order=c(1,2,2))
arima(data,order=c(0,1,2))
arima(data,order=c(1,2,1))
arima(data,order=c(1,0,5))
arima(data,order=c(0,1,1))


library("forecast")
library(MLmetrics)
C <-auto.arima(data)
C 
b <-forecast(a,h=12,level=95)
b
accuracy(b)

#Split into train and test
strain <- ts(log(data[1:24]),start=c(1,1),end=c(20,12), frequency=12 ) 
fit1 <-arima(strain, order = c(1, 0, 2))
pred1 <- 2.718^predict(fit1,6)$pred
pred1

tst <-s[25:36,2]
st<-ts(tst,start = c(1,1),,end=c(20,12),frequency = 12)
st
class(st)

MAPE(st,pred1) #error


fit <-arima(data, order = c(1, 2, 2))
nextpred <-predict(fit,n.ahead = 12)
nextpred
plot(forecast(fit,h=12), xlab = "month", ylab = "Sunspots", main = "Forecast")

