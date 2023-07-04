pacman::p_load(forecast, tseries, fUnitRoots, lubridate, ggplot2, xts, quantmod, rugarch, PerformanceAnalytics, lmtest)# might take a while in colab
# load data
r<-read.csv("亿纬锂能.csv");r

a<-ts(r[,2]);a

install.packages("aTSA")
library(aTSA)
adf.test(a)
library(zoo)
library(forecast)
auto.arima(a)   
install.packages("MTS")
library(MTS)
archTest(a)    

install.packages("rugarch")
library("rugarch")

######################## 1. model with constant mean  ##########Akaike: -3.9952
######################## sGarch
s<-ugarchspec(mean.model=list(armaOrder=c(0,0)),
               variance.model=list(model="sGARCH"),
               distribution.model="norm")  # 分布假设#standard GARCH就是GARCH（1,0)
m1<-ugarchfit(data=a,spec=s)      #GARCHģ
print(m1)
plot(m1, which='all')

######################## 2. GARCH with sstd  ##########Akaike: -5.9641
######################## sGarch

s<-ugarchspec(mean.model=list(armaOrder=c(0,0)),
              variance.model=list(model="sGARCH"),
              distribution.model="sstd")  # 分布假设#standard GARCH就是GARCH（1,1)
m2<-ugarchfit(data=a,spec=s)      #GARCHģ
print(m2)
plot(m2, which='all')

######################## 3. GJR-GARCH with sstd  ########## Akaike:-5.9780
######################## sGarch
s<-ugarchspec(mean.model=list(armaOrder=c(0,0)),
              variance.model=list(model="gjrGARCH"),
              distribution.model="sstd")  # 分布假设#standard GARCH就是GARCH（1,1)
m3<-ugarchfit(data=a,spec=s)      #GARCHģ
print(m3)
plot(m3, which='all')


########################最终选择模型并预测   ##这里选择模型2
s<-ugarchspec(mean.model=list(armaOrder=c(0,0)),
              variance.model=list(model="sGARCH"),
              distribution.model="norm")  # 分布假设#standard GARCH就是GARCH（1,0)
m1<-ugarchfit(data=a,spec=s)      #GARCHģ
print(m1)
plot(m1, which='all')


sfinal = s
setfixed(sfinal) = as.list(coef(m2))
sfinal


ugarchforecast(m1,n.ahead=70) 
plot.zoo(fitted(m1))
plot.zoo(sigma(m1))


############## simulate return series for next month
# m.sim can be increased to do multiple simulations m.sim= 1, 2, 3 etc.
sim = ugarchpath(spec = sfinal,
                 m.sim = 2,
                 n.sim = 70,
                 rseed = 123)
sim
fitted(sim)
sigma(sim)
plot.zoo(fitted(sim))
plot.zoo(sigma(sim))
