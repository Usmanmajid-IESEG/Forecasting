install.packages('fpp2')
install.packages('portes')
install.packages("readxl")
install.packages("forecast")
library(fpp2)
library(portes)
library(readxl)
library(forecast)


#Set Working Directory
setwd("C:/Users/mmajid1/Desktop/Forecasting")
data_turnover<-read_excel("DataSets.xlsx", sheet="Turnover")
turnover <- ts(data_turnover[,2], frequency = 12, start = c(2000,1))

# Split the data in training and test set
trnovr1 <- window(turnover, end=c(2015,12))
trnovr2 <- window(turnover, start=c(2016,1))

# Retrieve the length of the test set
h <- length(trnovr2)

###########################################Question 1########################################
#First we investigate the data set. We have seasonal (monthly) data, so we also explore the seasonal properties
#of the time series.

# Plot the data
plot(turnover)
lines(trnovr1, col="red")
lines(trnovr2, col="blue")

#We note trend in the time series, and high seasonality. We investigate the seasonality by means of
#a seasonplot and a monthplot.

par(mfrow=c(1,2))
seasonplot(turnover, year.labels=TRUE, year.labels.left=TRUE,
           main="Seasonal plot",
           ylab="Turnover",col=rainbow(20), pch=19)
monthplot(turnover, main="Month plot", ylab = "Turnover",
          xlab="Month", type="l")

#These plots conrm stable seasonality combined with a slight trend, but the slope is decreasing in more recent
#years.

##########################Question 2#########################################

n <- snaive(trnovr1, h=h) # seasonal naive
a_n <- accuracy(n,trnovr2)[,c(2,3,5,6)]
a_train_n <- a_n[1,]
a_train_n

a_test_n <- a_n[2,]
a_test_n

plot(turnover,main="Turnover", ylab="",xlab="Month")
lines(n$mean,col=4)
legend("topleft",lty=1,col=c(4),legend=c("Seaonsal naive"))


#By visual inspection, it looks like the seasonal naive method shows acceptable results. This can only be judged,
#however, in comparison with other methods. We check the quality of the residuals.

res <- residuals(n)
checkresiduals(n)

res <- na.omit(res)
LjungBox(res, lags=seq(1,24,4), order=0)

##The residual diagnostics show that the residuals of this naive method are not white noise. A forecast on the
#complete data set based on this method looks as follows:
n_final <- snaive(turnover, h=24)
plot(n_final)


#################################Question  3 ############################

d <- stl(trnovr1[,1], t.window=15, s.window=13)
trnovradj <- seasadj(d)

f_d <- forecast(d, method="rwdrift", h=h)
plot(f_d)

#In the graph below, we plot the various elements that make up the final forecast.

plot(rwf(trnovradj, drift=TRUE, h=h), col="red")
lines(turnover, col="black")
lines(f_d$mean, col="green")
legend("topleft", lty=1, col=c("black", "red", "blue", "green"),
       legend=c("Time series","Seasonally adjusted series",
                "Seasonally adjusted forecast", "Final forecast"))

#We check the accuracy of the forecasts based on a decomposition.

a_d <- accuracy(f_d,trnovr2)[,c(2,3,5,6)]
a_train_d <- a_d[1,]
a_train_d

a_test_d <- a_d[2,]
a_test_d

#We also check the residuals for the STL method.
checkresiduals(f_d)

res <- na.omit(f_d$residuals)
LjungBox(res, lags=seq(1,24,4), order=1)

#A forecast on the complete data set based on this method looks as follows:
d_final <- stl(turnover[,1], t.window=15, s.window=13)
trnovradj <- seasadj(d_final)
f_d_final <- forecast(d_final, method="rwdrift", h=24)
plot(f_d_final)

#################Question 4######################
fc <- hw(turnover,seasonal="mult")

plot(fc)


#exponential trend

fc1 <- hw(turnover,seasonal="mult",exponential=TRUE, h=24)

#damped exponential trend

fc2 <- hw(turnover,seasonal="mult",exponential=TRUE, damped=TRUE, h=24)

#additive damped trend

fc3 <- hw(turnover,seasonal="mult",damped=TRUE, h=24)

a_fc <- accuracy(fc)[,c(2,3,5,6)] 
a_fc1 <- accuracy(fc1)[,c(2,3,5,6)] 
a_fc2 <- accuracy(fc2)[,c(2,3,5,6)] 
a_fc3 <- accuracy(fc3)[,c(2,3,5,6)]
acc <- rbind(a_fc, a_fc1, a_fc2, a_fc3) 
rownames(acc) <- c("a_fc", "a_fc1", "a_fc2", "a_fc3") 
acc

fit <- rbind(fc$model$aic, fc1$model$aic, fc2$model$aic, fc3$model$aic)

colnames(fit) <- c("AIC")

rownames(fit) <- c("a_fc", "a_fc1", "a_fc2", "a_fc3")
fit


checkresiduals(fc1)

#################Question 5######################

#Models without damping (excluding possibly unstable models) 
e1 <- ets(trnovr1, model="AAA") 
e2 <- ets(trnovr1, model="MAA") 
e3 <- ets(trnovr1, model="MAM") 
e4 <- ets(trnovr1, model="MMM")
#Models with damping (excluding possibly unstable models) 
e5 <- ets(trnovr1, model="AAA", damped=TRUE) 
e6 <- ets(trnovr1, model="MAA", damped=TRUE) 
e7 <- ets(trnovr1, model="MAM", damped=TRUE) 
e8 <- ets(trnovr1, model="MMM", damped=TRUE)

m <- c("AAA", "MAA", "MAM", "MMM") 
result <- matrix(data=NA, nrow=4, ncol=9) 
for (i in 1:4){ 
  model <- ets(trnovr1, model=m[i], damped=FALSE) 
  f <- forecast(model, h=length(trnovr2)) 
  a <- accuracy(f, trnovr2) 
  result[i,1] <- model$aicc 
  result[i,2] <- a[1,2] 
  result[i,3] <- a[1,3] 
  result[i,4] <- a[1,5] 
  result[i,5] <- a[1,6] 
  result[i,6] <- a[2,2] 
  result[i,7] <- a[2,3] 
  result[i,8] <- a[2,5]
  result[i,9] <- a[2,6]
} 
rownames(result) <- m
result[,1] # Compare AICc values

a_train_e1 <- result[,2:5] 
colnames(a_train_e1) <- c("RMSE", "MAE", "MAPE", "MASE") 
a_train_e1

a_test_e1 <- result[,6:9] 
colnames(a_test_e1) <- c("RMSE", "MAE", "MAPE", "MASE") 
a_test_e1

m <- c("AAA", "MAA", "MAM", "MMM") 
result <- matrix(data=NA, nrow=4, ncol=9) 
for (i in 1:4){ 
  model <- ets(trnovr1, model=m[i], damped=TRUE) 
  f <- forecast(model, h=length(trnovr2)) 
  a <- accuracy(f, trnovr2) 
  result[i,1] <- model$aicc 
  result[i,2] <- a[1,2] 
  result[i,3] <- a[1,3] 
  result[i,4] <- a[1,5] 
  result[i,5] <- a[1,6] 
  result[i,6] <- a[2,2] 
  result[i,7] <- a[2,3] 
  result[i,8] <- a[2,5]
  result[i,9] <- a[2,6]
} 
rownames(result) <- m
result[,1] # Compare AICc values

a_train_e2 <- result[,2:5] 
colnames(a_train_e2) <- c("RMSE", "MAE", "MAPE", "MASE") 
a_train_e2


a_test_e2 <- result[,6:9] 
colnames(a_test_e2) <- c("RMSE", "MAE", "MAPE", "MASE") 
a_test_e2

summary(e8)

checkresiduals(e8)

auto_ets <- ets(trnovr1)
auto_ets$method

f <- forecast(auto_ets, h=length(trnovr2))
accuracy(f, trnovr2)[,c(2,6)]

checkresiduals(auto_ets)

e_final <- ets(turnover, model = "MMM", damped = TRUE)
e_final_f <- forecast(e_final, h=24)
plot(e_final_f)
#############Question 6################
#We further investigate the characteristics of the time series.
tsdisplay(trnovr1, main="Turnover", ylab="Turnover", xlab="Year")
#The ACF shows that nonstationarity is mainly caused by seasonality, and to a lesser extent by the trend.
#We start by differencing the data (the ndiffs function suggests one difference). Next, the nsdiffs function
#proposes to take seasonal differences as well.
ndiffs(trnovr1)

nsdiffs(diff(trnovr1))

#The characteristics of the double di???erenced time series are as follows.

tsdisplay(diff(diff(trnovr1,12)), main="Double Differenced Turnover", ylab="Turnover Index", xlab="Year")


#Model Estimation.
#We start with the auto.arima procedure to get a frst idea of a suitable model. We disable the stepwise and
#approximate search, and ask for first and seasonal differences.
m0 <- auto.arima(trnovr1, stepwise = FALSE, approximation = FALSE, d=1, D=1)
checkresiduals(m0)


tsdisplay(m0$residuals)


LjungBox(m0$residuals, lags=seq(length(m0$coef),24,4), order=length(m0$coef))



f0 <- forecast(m0, h=h) 
accuracy(f0,trnovr2)[,c(2,3,5,6)]



getinfo <- function(x,h,...) 
{ 
  train.end <- time(x)[length(x)-h]
  test.start <- time(x)[length(x)-h+1]
  train <- window(x,end=train.end)
  test <- window(x,start=test.start)
  fit <- Arima(train,...) 
  fc <- forecast(fit,h=h)
  a <- accuracy(fc,test)
  result <- matrix(NA, nrow=1, ncol=5)
  result[1,1] <- fit$aicc
  result[1,2] <- a[1,6]
  result[1,3] <- a[2,6]
  result[1,4] <- a[1,2]
  result[1,5] <- a[2,2]
  return(result)
}

mat <- matrix(NA,nrow=54, ncol=5)
modelnames <- vector(mode="character", length=54)
line <- 0 
for (i in 2:4){ 
  for (j in 0:2){ 
    for (k in 0:1){ 
      for (l in 0:2){
        line <- line+1 
        mat[line,] <- getinfo(turnover,h=37,order=c(i,1,j),seasonal=c(k,1,l)) 
        modelnames[line] <- paste0("ARIMA(",i,",1,",j,")(",k,",1,",l,")[12]") 
      }
    }
  }
}



colnames(mat) <- c("AICc", "MASE_train", "MASE_test", "RMSE_train", "RMSE_test")
rownames(mat) <- modelnames 

# best AICc 
mat[mat[,1]==min(mat[,1])]

#best MASE_train 
mat[mat[,2]==min(mat[,2])]

#best MASE_test
mat[mat[,3]==min(mat[,3])]

#best RMSE_train
mat[mat[,4]==min(mat[,4])]

#best RMSE_test
mat[mat[,5]==min(mat[,5])]





#Lowest AICc and RMSE Train
m1 <- Arima(trnovr1, order=c(4,1,2), seasonal=c(0,1,1)) 
LjungBox(m1$residuals, lags=seq(length(m1$coef),24,4), order=length(m1$coef))
tsdisplay(m1$residuals)

f1 <- forecast(m1, h=h)

#Best MASE and RMSE on the test set
m2 <- Arima(trnovr1, order=c(4,1,2), seasonal=c(1,1,2))
LjungBox(m2$residuals, lags=seq(length(m2$coef),24,4), order=length(m2$coef))
tsdisplay(m2$residuals)         
f2 <- forecast(m2, h=h)

#best MASE on training set
m3 <- Arima(trnovr1, order=c(2,1,2), seasonal=c(1,1,2))
LjungBox(m3$residuals, lags=seq(length(m3$coef),24,4), order=length(m3$coef))
tsdisplay(m3$residuals)         
f3 <- forecast(m3, h=h)



a_m0 <- accuracy(f0,trnovr2)[,c(2,3,5,6)] 
a_m1 <- accuracy(f1,trnovr2)[,c(2,3,5,6)] 
a_m2 <- accuracy(f2,trnovr2)[,c(2,3,5,6)] 
a_m3 <- accuracy(f3,trnovr2)[,c(2,3,5,6)]
a_train_a <- rbind(a_m0[1,], a_m1[1,], a_m2[1,], a_m3[1,])
rownames(a_train_a) <- c("a_m0", "a_m1", "a_m2", "a_m3") 
a_train_a



a_test_a <- rbind(a_m0[2,], a_m1[2,], a_m2[2,], a_m3[2,])
rownames(a_test_a) <- c("a_m0", "a_m1", "a_m2", "a_m3") 
a_test_a


#Question 8
a_final <- Arima(turnover, order=c(4,1,2), seasonal=c(1,1,2)) 
summary(a_final)


a_final_f <- forecast(a_final, h=24) 
plot(a_final_f)

a_final_f <- forecast(a_final, h=24) 
plot(a_final_f)


#########Final Forecast##############


Final_forecast<-f <- forecast(fc1, h=24)
plot(Final_forecast)
