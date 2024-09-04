library(GGally)
library(ggplot2)
library(forecast)
library(imputeTS)
library(urca)

#cleaning the data, handling the missing data
#colsums function to sum up the total number of missing values in each column.
colSums(is.na(epest_county_estimates))
# total missing values in column EPEST_LOW_KG = 3441641, EPEST_HIGH_KG= 1276

#assigning missed values using na.interp to respective columns
epest_county_estimates$EPEST_LOW_KG=na.interp(epest_county_estimates$EPEST_LOW_KG)
epest_county_estimates$EPEST_HIGH_KG=na.interp(epest_county_estimates$EPEST_HIGH_KG)

colSums(is.na(epest_county_estimates))
#missed values in every column is 0

#data visualizing

pest=data.frame(
  COMPOUND=epest_county_estimates[,"COMPOUND"],
  STATE_FIPS_CODE=epest_county_estimates[,"STATE_FIPS_CODE"],
  COUNTY_FIPS_CODE=epest_county_estimates[,"COUNTY_FIPS_CODE"],
  EPEST_LOW_KG=epest_county_estimates[,"EPEST_LOW_KG"],
  EPEST_HIGH_KG=epest_county_estimates[,"EPEST_HIGH_KG"]
)
data <- ts(pest, start = 1992,end = 2019, frequency = 1)
data
#histogram of EPEST_HIGH_KG
data%>%
  as.data.frame()%>%
  ggplot(aes(x=EPEST_HIGH_KG))+
  geom_histogram(binwidth = 1000)

#histogram of EPEST_LOW_KG
data%>%
  as.data.frame()%>%
  ggplot(aes(x=EPEST_LOW_KG))+
  geom_histogram(binwidth = 1000)

#line graph of high pest usage vs year
autoplot(data[,"EPEST_HIGH_KG"])

#line graph of Low pest usage vs year
autoplot(data[,"EPEST_LOW_KG"])

#scatter plot Low pest usage vs high pest usage
data%>%
  as.data.frame()%>%
  ggplot(aes(x=EPEST_LOW_KG,y=EPEST_HIGH_KG))+
  geom_point()

#further visualization was used in tableau software to understand data better


#hypothetical test of the data EPEST_HIGH_KG and EPEST_LOW_KG that hypothesis both are equal since graph looks similar
t.test(data[,"EPEST_HIGH_KG"],data[,"EPEST_LOW_KG"])
#alternative hypothesis: true difference in means is not equal to 0
#the test results suggest that there is not enough evidence to reject the null hypothesis of no difference in means between the two groups, as the p-value is much larger than the conventional significance level 
#Additionally, the confidence interval includes 0, further supporting the conclusion of no significant difference.
#the null hypothesis cannot be rejected, and we do not have sufficient evidence to claim that there is a significant difference between the means of the two groups. Therefore, it is reasonable to consider that the means of the two groups may be equal.

#transformation of data

datalam=BoxCox.lambda(data[,"EPEST_HIGH_KG"])
autoplot(cbind(RAW=data[,"EPEST_HIGH_KG"], LOG=log(data[,"EPEST_HIGH_KG"]),BOXCOX=BoxCox(data[,"EPEST_HIGH_KG"],datalam)),facets = TRUE)

#for better understanding normality 
checkresiduals(data[,"EPEST_HIGH_KG"])
checkresiduals(log(data[,"EPEST_HIGH_KG"]))
checkresiduals(BoxCox(data[,"EPEST_HIGH_KG"],datalam))

#the best transformation based on residual analyses and model requirements is the LOG transformation for EPEST_HIGH_KG. 


datalam1=BoxCox.lambda(data[,"EPEST_LOW_KG"])
autoplot(cbind(RAW=data[,"EPEST_LOW_KG"], LOG=log(data[,"EPEST_LOW_KG"]),BOXCOX=BoxCox(data[,"EPEST_LOW_KG"],datalam1)),facets = TRUE)



#models

#simple exponential smoothing model

simple_model=ses(log(data[,"EPEST_HIGH_KG"]),h=4)
summary(simple_model)
#AICc 108.0416
simple_model1=ses(log(data[,"EPEST_HIGH_KG"]),alpha = 0.5)
summary(simple_model1)
# 111.7393

simple_model2=ses(log(data[,"EPEST_HIGH_KG"]),alpha = 0.75)
summary(simple_model2)
# 115.1210

#forecasting with original data
fcast <- forecast(simple_model, h=4)
checkresiduals(simple_model)
fcast
autoplot(log(data[,"EPEST_HIGH_KG"]))+
  autolayer(fcast)+
  autolayer(fitted(simple_model), series = "Fitted")
  
#back transforming data to check with original data
fcast1=fcast
fcast1
fcast1$mean <- exp(fcast$mean)
fcast1$lower <- exp(fcast$lower)
fcast1$upper <- exp(fcast$upper)


fit <- fitted(simple_model)
fit2 <- exp(fit)
autoplot(data[,"EPEST_HIGH_KG"]) +
  autolayer(fit2, series = "Fitted") +
  autolayer(fcast1, series = "Forecast") +
  ggtitle("Forecast and Fitted Model")

#holt linear model

holt_model=holt(log(data[,"EPEST_HIGH_KG"]),damped = TRUE,h=4)
summary(holt_model)
# 116.5141

holt_model1=holt(log(data[,"EPEST_HIGH_KG"]),h=4)
summary(holt_model1)
# 112.4651
fcast=forecast(holt_model1)
fcast
checkresiduals(fcast)
autoplot(log(data[,"EPEST_HIGH_KG"]))+
  autolayer(fcast,PI=TRUE)+
  autolayer(fitted(holt_model1), series = "Fitted")

#back transforming data to check with original data

fcast1=fcast
fcast1
fcast1$mean <- exp(fcast$mean)
fcast1$lower <- exp(fcast$lower)
fcast1$upper <- exp(fcast$upper)

fit <- fitted(holt_model)
fit2 <- exp(fit)
autoplot(data[,"EPEST_HIGH_KG"]) +
  autolayer(fit2, series = "Fitted") +
  autolayer(fcast1, series = "Forecast") +
  ggtitle("Forecast and Fitted Model")

#ETS MODEL

e_model=ets(log(data[,"EPEST_HIGH_KG"]))
summary(e_model)
#108.0415 Best

e_model1=ets(log(data[,"EPEST_HIGH_KG"]),model = "ANN")
summary(e_model1)
#108.0416

e_model2=ets(log(data[,"EPEST_HIGH_KG"]),model = "MAN")
summary(e_model2)
#112.5402

e_model3=ets(log(data[,"EPEST_HIGH_KG"]),model = "MMN")
summary(e_model3)
#114.5862


fcast=forecast(e_model,h=4)
fcast
autoplot(log(data[,"EPEST_HIGH_KG"]))+
  autolayer(fcast)+
  autolayer(fitted(holt_model), series = "Fitted")

fcast1=fcast
fcast1
fcast1$mean <- exp(fcast$mean)
fcast1$lower <- exp(fcast$lower)
fcast1$upper <- exp(fcast$upper)

fit <- fitted(e_model)
fit2 <- exp(fit)
autoplot(data[,"EPEST_HIGH_KG"]) +
  autolayer(fit2, series = "Fitted") +
  autolayer(fcast1, series = "Forecast") +
  ggtitle("Forecast and Fitted Model")

#ARIMA model

ggtsdisplay(log(data[,"EPEST_HIGH_KG"]))

unit_test=ur.df(log(data[,"EPEST_HIGH_KG"]), type = "drift", lags = 12, selectlags = "AIC") 
summary(unit_test)
#test-statistic is: -14.411 is larger hence data is stationary no need to do differencing

ar_model=arima(log(data[,"EPEST_HIGH_KG"]), order = c(1,0,0))
summary(ar_model)
#aic = 92.29

ar_model1=arima(log(data[,"EPEST_HIGH_KG"]), order = c(1,0,3))
summary(ar_model1)
# aic = 95.37

ar_model2=arima(log(data[,"EPEST_HIGH_KG"]), order = c(2,0,0))
summary(ar_model2)
# aic = 93.69

ar_model3=arima(log(data[,"EPEST_HIGH_KG"]), order = c(1,0,1))
summary(ar_model3)
# aic = 91.4 best performs better in terms of lower RMSE and MAE, and it produces residuals that are more like white noise.

ar_model4=arima(log(data[,"EPEST_HIGH_KG"]), order = c(1,0,2))
summary(ar_model4)
# aic = 93.38

ar_model5=auto.arima(log(data[,"EPEST_HIGH_KG"]))
summary(ar_model5)
#AIC=91.2 AICc=91.68

#forecasting arima
fcast=forecast(ar_model3,h=4)
fcast
autoplot(log(data[,"EPEST_HIGH_KG"]))+
  autolayer(fcast)+
  autolayer(fitted(ar_model3), series = "Fitted")


fcast1=fcast
fcast1
fcast1$mean <- exp(fcast$mean)
fcast1$lower <- exp(fcast$lower)
fcast1$upper <- exp(fcast$upper)

fit <- fitted(ar_model3)
fit2 <- exp(fit)
autoplot(data[,"EPEST_HIGH_KG"]) +
  autolayer(fit2, series = "Fitted") +
  autolayer(fcast1, series = "Forecast",PI=TRUE) +
  ggtitle("Forecast and Fitted Model")

#dynamic regression model


unit_test=ur.df(log(data[,"EPEST_HIGH_KG"]), type = "drift", lags = 12, selectlags = "AIC") 
summary(unit_test)
#test-statistic is: -14.411 smaller than critical values
unit_test1=ur.df(log(data[,"EPEST_LOW_KG"]), type = "drift", lags = 12, selectlags = "AIC") 
summary(unit_test1)
#test-statistic is: -20.1115  smaller than critical values since both variable passed unit root test no need of difference the data

ndiffs(log(data[,"EPEST_HIGH_KG"]))# 0
ndiffs(log(data[,"EPEST_LOW_KG"])) # 0

ggtsdisplay(log(data[,"EPEST_HIGH_KG"]))


dreg_model = auto.arima(log(data[,"EPEST_HIGH_KG"]),d=0,xreg =log(data[,"EPEST_LOW_KG"]))

summary(dreg_model)
#AICc=-79.87


dreg_model1 = Arima(log(data[,"EPEST_HIGH_KG"]),xreg =log(data[,"EPEST_LOW_KG"]), order = c(1,0,1))
summary(dreg_model1)
#AICc=-73.11

dreg_model2 = Arima(log(data[,"EPEST_HIGH_KG"]),xreg =log(data[,"EPEST_LOW_KG"]), order = c(1,0,2))
summary(dreg_model2)
#AICc=-73.19 best model

dreg_model3 = Arima(log(data[,"EPEST_HIGH_KG"]),xreg =log(data[,"EPEST_LOW_KG"]), order = c(1,0,3))
summary(dreg_model3)
#AICc=-69.15 

dreg_model4 = Arima(log(data[,"EPEST_HIGH_KG"]),xreg =log(data[,"EPEST_LOW_KG"]), order = c(1,0,4))
summary(dreg_model4) 
# AICc=-65.95

dreg_model5 = Arima(log(data[,"EPEST_HIGH_KG"]),xreg =log(data[,"EPEST_LOW_KG"]), order = c(1,0,5))
summary(dreg_model5)
#AICc=-68.82 

dreg_model6 = Arima(log(data[,"EPEST_HIGH_KG"]),xreg =log(data[,"EPEST_LOW_KG"]), order = c(1,0,2))
summary(dreg_model6)
#AICc=-63.2

dreg_model7 = Arima(log(data[,"EPEST_HIGH_KG"]),xreg =log(data[,"EPEST_LOW_KG"]), order = c(2,0,2))
summary(dreg_model7)
#AICc=-69.76

dreg_model8 = Arima(log(data[,"EPEST_HIGH_KG"]),xreg =log(data[,"EPEST_LOW_KG"]), order = c(3,0,2))
summary(dreg_model8)
#AICc=-65.87

dreg_model8 = Arima(log(data[,"EPEST_HIGH_KG"]),xreg =log(data[,"EPEST_LOW_KG"]), order = c(4,0,2))
summary(dreg_model8)
#AICc=-61.49

dreg_model8 = Arima(log(data[,"EPEST_HIGH_KG"]),xreg =log(data[,"EPEST_LOW_KG"]), order = c(5,0,2))
summary(dreg_model8)
#AICc=-61.23

checkresiduals(dreg_model1)


# Log-transform the future values
pred1 = auto.arima(log(data[,"EPEST_LOW_KG"]))
summary(pred1)
predcast = forecast(pred1,h=4) 
predcast

fcast <- forecast(dreg_model2,xreg=predcast$mean)
fcast
autoplot(fcast, PI=TRUE)+
  autolayer(fitted(dreg_model2), series = "Fitted")



backtransform=predcast
backtransform
backtransform$mean=exp(predcast$mean)
backtransform$upper=exp(predcast$upper)
backtransform$lower=exp(predcast$lower)
backtransform
fcast2 <- forecast(dreg_model2,xreg=backtransform$mean)
fcast2
fit <- fitted(dreg_model2)
fit2 <- exp(fit)


autoplot(data[,"EPEST_HIGH_KG"]) +
  autolayer(fit2, series = "Fitted") +
  autolayer(fcast2, series = "Forecast",PI=TRUE) +
  ggtitle("Forecast and Fitted Model")

#train and test validation

length(data)

train_data <- window(data, start=1992, end=2015)
test_data <- window(data, start=2016, end=2019)

#ses

trainses=ses(log(train_data[,"EPEST_HIGH_KG"]),h=4)


accuracy(trainses,log(test_data[,"EPEST_HIGH_KG"]))
# RMSE train 1.143762 test 1.249080

#holt
trainholt=holt(log(train_data[,"EPEST_HIGH_KG"]),h=4)
accuracy(trainholt,log(test_data[,"EPEST_HIGH_KG"]))
# RMSE train 1.152054 test 1.336982

#ETS

trainets=ets(log(train_data[,"EPEST_HIGH_KG"]))
fcasts <- forecast(trainets, h=4)
accuracy(fcasts,log(test_data[,"EPEST_HIGH_KG"]))
# RMSE train 1.143762 test 1.249476

#arima

trainarima=arima(log(train_data[,"EPEST_HIGH_KG"]), order = c(1,0,1))
fcasts1 <- forecast(trainarima, h=4)
accuracy(fcasts1,log(test_data[,"EPEST_HIGH_KG"]))
# RMSE train 1.005780 test 1.328333

#Dynamic regression
traindynamic=Arima(log(train_data[,"EPEST_HIGH_KG"]),xreg =log(train_data[,"EPEST_LOW_KG"]), order = c(1,0,2))
pred1 = auto.arima(log(train_data[,"EPEST_LOW_KG"]))
summary(pred1)
predcast = forecast(pred1,h=4) 
predcast

fcasts2 <- forecast(traindynamic,xreg=predcast$mean)
accuracy(fcasts2,log(test_data[,"EPEST_HIGH_KG"]))
# RMSE train 0.05581354 test 1.24739717

autoplot(log(data[,"EPEST_HIGH_KG"])) +
  autolayer(trainses, series = "SES Fit", PI=FALSE)+
  autolayer(trainholt, series = "holt fit", PI=FALSE)+
  autolayer(fcasts, series = "ets", PI=FALSE)+
  autolayer(fcasts1, series = "arima", PI=FALSE)+
  autolayer(fcasts2, series = "dynamic regression", PI=FALSE)

# back transform train

#ses
# Forecast using the SES model
bfcast <- forecast(trainses)


bfcast_transformed <- bfcast  
bfcast_transformed$mean <- exp(bfcast$mean)
bfcast_transformed$lower <- exp(bfcast$lower)
bfcast_transformed$upper <- exp(bfcast$upper)


bfcast_transformed
accuracy(bfcast_transformed,test_data[,"EPEST_HIGH_KG"])

bfcast
#holt
bfcast_holt <- forecast(trainholt)


bfcast_transformed_holt <- bfcast_holt 
bfcast_transformed_holt$mean <- exp(bfcast_holt$mean)
bfcast_transformed_holt$lower <- exp(bfcast_holt$lower)
bfcast_transformed_holt$upper <- exp(bfcast_holt$upper)

bfcast_transformed_holt
accuracy(bfcast_transformed_holt,test_data[,"EPEST_HIGH_KG"])
#ets
bfcast_ets <- forecast(trainets, h=4)


bfcast_transformed_ets <- bfcast_ets
bfcast_transformed_ets$mean <- exp(bfcast_ets$mean)
bfcast_transformed_ets$lower <- exp(bfcast_ets$lower)
bfcast_transformed_ets$upper <- exp(bfcast_ets$upper)

bfcast_transformed_ets
accuracy(bfcast_transformed_ets,test_data[,"EPEST_HIGH_KG"])

#arima
fcasts1 <- forecast(trainarima, h=4)
bfcast_transformed_arima <- fcasts1
bfcast_transformed_arima$mean <- exp(fcasts1$mean)
bfcast_transformed_arima$lower <- exp(fcasts1$lower)
bfcast_transformed_arima$upper <- exp(fcasts1$upper)

bfcast_transformed_arima
accuracy(bfcast_transformed_arima,test_data[,"EPEST_HIGH_KG"])

#dynamic regression
fcasts2 <- forecast(traindynamic,xreg=predcast$mean)
bfcast_transformed_dreg <- fcasts2
bfcast_transformed_dreg$mean <- exp(fcasts2$mean)
bfcast_transformed_dreg$lower <- exp(fcasts2$lower)
bfcast_transformed_dreg$upper <- exp(fcasts2$upper)

bfcast_transformed_dreg
accuracy(bfcast_transformed_dreg,test_data[,"EPEST_HIGH_KG"])

autoplot(data[,"EPEST_HIGH_KG"]) +
  autolayer(bfcast_transformed, series = "SES",PI=FALSE)+
  autolayer(bfcast_transformed_holt,series = "holt", PI=FALSE)+
  autolayer(bfcast_transformed_ets,series = "ets", PI=FALSE)+
  autolayer(bfcast_transformed_arima,series = "arima", PI=FALSE)+
  autolayer(bfcast_transformed_dreg,series = "dynamic regression", PI=FALSE)
 
