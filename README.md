# Milk
Time series Analysis of data cow
library(TSA)


library(forecast)
data<-read.csv("projectm.csv")
project=ts(data, start = c(1962,1), frequency = 12)
plot(project,ylab='Monthly Milk Production',xlab='Month',type='l')
points(y=project,x=time(project),pch=as.vector(season(project)))

# Seasonal means + linear trend model
month=season(project)
tm=time(project)
model1=lm(project~month + tm)
summary(model1)

# Overlapping the model with the original data
plot(project)
fitted_model=ts(fitted(model1),start=c(1962,1),frequency=12)
lines(fitted_model,col=2,lty=2)

# Standardized Residual Plot (zero mean and homoscedasticity check)
plot(rstudent(model1),x=as.vector(time(project)),type='l',ylab='Standardized Residuals',xlab='Time')
abline(h=0,col=2)

# QQ plot (normality check)
qqnorm(rstudent(model1),main='QQ plot of standardized residuals')
qqline(rstudent(model1))

# Shapiro-Wilk test and Histogram (normality check)
shapiro.test(rstudent(model1))
hist(rstudent(model1),main='Histogram of standardized residuals')

# Runs test (independence check)
runs(rstudent(model1))

# ACF plot (independence check)
acf(rstudent(model1),main='ACF plot of standardized residuals')

# HAC test
require(sandwich)  
vcovHAC(model1)

# ADF and PP test (stationarity check)
adf.test(rstudent(model1))
pp.test(rstudent(model1))

# ACF,PACF and EACF plots (p,q estimation)
acf(rstudent(model1),main='ACF plot of standardized residuals')
pacf(rstudent(model1),main='PACF plot of standardized residuals')
eacf(rstudent(model1))
auto.arima(residuals(model1))

# Possible models 
ar1=Arima(residuals(model1),order=c(1,0,0),include.mean=F)
ar2=Arima(residuals(model1),order=c(2,0,0),include.mean=F)
arma11=Arima(residuals(model1),order=c(1,0,1),include.mean=F)

arima011=Arima(residuals(model1),order=c(0,1,1),include.mean=F)
arima111=Arima(residuals(model1),order=c(1,1,1),include.mean=F)
arima211=Arima(residuals(model1),order=c(2,1,1),include.mean=F)
arima212=Arima(residuals(model1),order=c(2,1,2),include.mean=F)

# Comparison of models using AIC/AICc/BIC
arima011
arima111
arima211
arima212

# Model diagnostics
tsdiag(arima211)

# Forecasting next 5 years based on AR(2) model
newtm=seq(from=1976,to=1981,length=60)
newdata=data.frame(month=as.factor(month[1:60]),tm=newtm)
predxreg=predict(model1,newdata)

predx=predict(arima211,n.ahead=60)
pr=predx$pred+predxreg
uci=pr+2*predx$se
lci=pr-2*predx$se

# Prediction intervals
pr=ts(pr,start=1976,freq=12)
uci=ts(uci,start=1976,freq=12)
lci=ts(lci,start=1976,freq=12)

ymin=min(c(as.vector(lci),project))-.1
ymax=max(c(as.vector(uci),project))+.1

par(mfrow=c(1,1))
plot(project,xlim=c(1962,1981),main="Monthly Milk Production",ylim=c(500,1100))
lines(pr,col=3)
lines(uci,col=2)
lines(lci,col=2)
