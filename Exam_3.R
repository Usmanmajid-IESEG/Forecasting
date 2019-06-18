install.packages('fpp2')
install.packages('portes')
install.packages("readxl")
library(fpp2)
library(portes)
library(readxl)
#Set Working Directory
setwd("C:/Users/mmajid1/Desktop/Forecasting")
data_Beverages_Sale<-read_excel("SeriesReport-201904241559.xls")
Beverages_Sale<- ts(data_Beverages_Sale[,2], frequency = 12, start = c(2000,1))

# Split the data in training and test set
Beverages_Sale1 <- window(Beverages_Sale, end=c(2015,12))
Beverages_Sale2 <- window(Beverages_Sale, start=c(2016,1))

# Retrieve the length of the test set
h <- length(Beverages_Sale2)

#First we investigate the data set. We have seasonal (monthly) data, so we also explore the seasonal properties
#of the time series.

# Plot the data
plot(Beverages_Sale)
lines(Beverages_Sale1, col="red")
lines(Beverages_Sale2, col="blue")

#We note increasing trend in the time series, and have seasonality. We investigate the seasonality by means of
#a seasonplot and a monthplot.

par(mfrow=c(1,2))
seasonplot(Beverages_Sale, year.labels=TRUE, year.labels.left=TRUE,
           main="Seasonal plot",
           ylab="Beverages Sales",col=rainbow(20), pch=19)
monthplot(Beverages_Sale, main="Monthly Sales plot", ylab = "Beverages_Sale",
          xlab="Month", type="l")


#These plots confrm stable seasonality combined with an increasing trend




#####################################Seasonal Naive#######################################

n <- snaive(Beverages_Sale1, h=h) # seasonal naive
a_n <- accuracy(n,Beverages_Sale2)[,c(2,3,5,6)]
a_train_n <- a_n[1,]
a_train_n

a_test_n <- a_n[2,]
a_test_n

plot(Beverages_Sale,main="Bevegares Monthly Sales", ylab="",xlab="Month")
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
n_final <- snaive(Beverages_Sale, h=24)
plot(n_final)









#########################ARIMA MODELS#####################################
####################Data Characteristics#############

tsdisplay(Beverages_Sale1, main="Monthly Beverages Sales", ylab="Beverages Sales Index", xlab="Year")


ndiffs(Beverages_Sale1)
nsdiffs(diff(Beverages_Sale1))


tsdisplay(diff(diff(Beverages_Sale1,12)), main="Double differenced Monthly Beverages Sales", ylab="Beverages Sales Index", xlab="Year")
diff_bev_sale1<-tsdisplay(diff(diff(Beverages_Sale1,12)), main="Double differenced Monthly Beverages Sales", ylab="Beverages Sales Index", xlab="Year")

nsdiffs(diff_bev_sale1)
nsdiffs(diff(diff_bev_sale1))

#############################Model estimation#########################


m0 <- auto.arima(Beverages_Sale1, stepwise = FALSE, approximation = FALSE, d=1, D=1)
checkresiduals(m0)



tsdisplay(m0$residuals)
LjungBox(m0$residuals, lags=seq(length(m0$coef),24,4), order=length(m0$coef))


f0 <- forecast(m0, h=h) 
accuracy(f0,Beverages_Sale2)[,c(2,3,5,6)]





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
        mat[line,] <- getinfo(Beverages_Sale,h=37,order=c(i,1,j),seasonal=c(k,1,l)) 
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
    



#Lowest RMSE Train
m1 <- Arima(Beverages_Sale1, order=c(4,1,1), seasonal=c(0,1,2)) 
LjungBox(m1$residuals, lags=seq(length(m1$coef),24,4), order=length(m1$coef))
tsdisplay(m1$residuals)

f1 <- forecast(m1, h=h)


#LOWEST RMSE TEST  
m2 <- Arima(Beverages_Sale1, order=c(2,1,0), seasonal=c(1,1,1))
LjungBox(m2$residuals, lags=seq(length(m2$coef),24,4), order=length(m2$coef))
tsdisplay(m2$residuals)         
f2 <- forecast(m2, h=h)



#Lowest MASE TEST 
m3 <- Arima(Beverages_Sale1, order=c(4,1,2), seasonal=c(0,1,2)) 
LjungBox(m3$residuals, lags=seq(length(m3$coef),24,4), order=length(m3$coef))
tsdisplay(m3$residuals) 
f3 <- forecast(m3, h=h)

#Lowest AIC
m4 <- Arima(Beverages_Sale1, order=c(3,1,1), seasonal=c(0,1,2)) 
LjungBox(m4$residuals, lags=seq(length(m4$coef),24,4), order=length(m4$coef))
tsdisplay(m4$residuals) 
f4 <- forecast(m4, h=h)









a_m0 <- accuracy(f0,Beverages_Sale2)[,c(2,3,5,6)] 
a_m1 <- accuracy(f1,Beverages_Sale2)[,c(2,3,5,6)] 
a_m2 <- accuracy(f2,Beverages_Sale2)[,c(2,3,5,6)] 
a_m3 <- accuracy(f3,Beverages_Sale2)[,c(2,3,5,6)]
a_m4 <- accuracy(f4,Beverages_Sale2)[,c(2,3,5,6)]
a_train_a <- rbind(a_m0[1,], a_m1[1,], a_m2[1,], a_m3[1,],  a_m4[1,])
rownames(a_train_a) <- c("a_m0", "a_m1", "a_m2", "a_m3","a_m4" ) 
a_train_a


a_test_a <- rbind(a_m0[2,], a_m1[2,], a_m2[2,], a_m3[2,],  a_m4[2,]) 
rownames(a_test_a) <- c("a_m0", "a_m1", "a_m2", "a_m3","a_m4" )  
a_test_a





##############Final model and forecasts##########################
a_final <- Arima(Beverages_Sale, order=c(4,1,1), seasonal=c(0,1,2)) 
summary(a_final)


a_final_f <- forecast(a_final, h=24) 
plot(a_final_f)

#Conclusion
final_train <- rbind(a_train_n, a_train_a[3,])
rownames(final_train) <- c("snaive", "ARIMA(4,1,2)(1,1,2)[12]")
final_train


final_test <- rbind(a_test_n, a_test_a[3,])
rownames(final_train) <- c("snaive", "ARIMA(4,1,2)(1,1,2)[12]")
final_test



model <- Arima(Beverages_Sale, model = m1)
summary(model)

plot(forecast(model, h=24), include = 80)

