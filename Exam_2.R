install.packages('fpp2')
install.packages('portes')
install.packages("readxl")

library(fpp2)
library(portes)
library(readxl)
library(tseries)


#Set Working Directory
setwd("C:/Users/mmajid1/Desktop/Forecasting")
data_Houses<-read_excel("DataSets.xlsx", sheet="Houses")
data_Houses$average_house_prices<- data_Houses$Houseprices/data_Houses$Houses
avghouprc <- ts(data_Houses[,4], frequency = 1, start = 1973)

# Split the data in training and test set
avghouprc1 <- window(avghouprc, end=2010)
avghouprc2 <- window(avghouprc, start=2011)

# Retrieve the length of the test set
h <- length(avghouprc2)

#Data Visualization
plot(avghouprc)
lines(avghouprc1, col="red")
lines(avghouprc2, col="blue")

##### Forecast using appropriate naive method#######
f1 <- meanf(avghouprc1, h=h)			      #mean
f2 <- rwf(avghouprc1, h=h)				      #naive
f3 <- rwf(avghouprc1, drift=TRUE, h=h)	#drift
accuracy(f1,avghouprc2)

accuracy(f2,avghouprc2)


accuracy(f3,avghouprc2)



plot(avghouprc,main="Average House Price", ylab="",xlab="Year")
lines(f1$mean,col=4)
lines(f2$mean,col=2)
lines(f3$mean,col=3)
legend("left",lty=1,col=c(4,2,3),legend=c("Mean","Naive","Drift"))

plot(f3)
lines(avghouprc2, col="red")

checkresiduals(f3)

checkresiduals(f2)

checkresiduals(f1)

#now lets forecast on the original dataset.

f4 <- rwf(avghouprc, drift=TRUE, h=h)	#drift
plot(f4)


###############Question 3 Exponential smoothing#######################
h1 <- holt(avghouprc1,h=h)
h2 <- holt(avghouprc1,h=h, damped=TRUE)
h4 <- holt(avghouprc1,h=h, exponential=TRUE, damped=TRUE)



plot(h1, type="l", ylab="Average House Prices", 
     xlab="Year", fcol="white", shadecols="white")
lines(fitted(h1), col=2) 
lines(fitted(h2), col=3)
lines(fitted(h4), col=5)
lines(h1$mean, col=2, type="l") 
lines(h2$mean, col=3, type="l")
lines(h4$mean, col=5, type="l") #almost the same as h2
legend("left", lty=1, col=c(2,3,4,5),c("Holt's Linear", "Additive Damped", "Multiplicative Damped"), cex=0.75)


accuracy(h1, avghouprc2)[,c(2,3,5,6)]     
accuracy(h2,avghouprc2)[,c(2,3,5,6)]
accuracy(h4, avghouprc2)[,c(2,3,5,6)]

res <- residuals(h2)
checkresiduals(res)
LjungBox(res, lags=seq(1,15,1), order=length(h2$model$par))

h2_on_total_data = holt(avghouprc,damped = TRUE)
plot(h2_on_total_data)



###################Question 4 ETS########################

#Different ETS models evaluation
e1 <- ets(avghouprc1,"AAN")
e2 <- ets(avghouprc1,"MNN")
e3 <- ets(avghouprc1,"ANN")
e4 <- ets(avghouprc1,"MAN")
e5 <- ets(avghouprc1,"MMN")

e6 <- ets(avghouprc1, model = "AAN", damped = TRUE)    
e7 <- ets(avghouprc1, model = "MAN", damped = TRUE)    
e8 <- ets(avghouprc1, model = "MMN", damped = TRUE)         
auto_ets = ets(avghouprc1)                


#Forecasting with all ets models
f1 = forecast(e1,h=h)
accuracy(f1,avghouprc2)[,c(2,3,5,6)]


f2 = forecast(e2,h=h)
accuracy(f2,avghouprc2)[,c(2,3,5,6)]

f3 = forecast(e3,h=h)
accuracy(f3,avghouprc2)[,c(2,3,5,6)]

f4 = forecast(e4,h=h)
accuracy(f4,avghouprc2)[,c(2,3,5,6)]

f5 = forecast(e5,h=h)
accuracy(f5,avghouprc2)[,c(2,3,5,6)]

f6 = forecast(e6,h=h)
accuracy(f6,avghouprc2)[,c(2,3,5,6)]


f7 = forecast(e7,h=h)
accuracy(f7,avghouprc2)[,c(2,3,5,6)]


f8 = forecast(e8,h=h)
accuracy(f8,avghouprc2)[,c(2,3,5,6)]

fauto_ets = forecast(auto_ets,h=h)
accuracy(fauto_ets,avghouprc2)[,c(2,3,5,6)]


plot(f1)
plot(f6)

ets_res = residuals(e1)
tsdisplay(ets_res)
checkresiduals(e1)           

ets_res_2 = residuals(e6)
tsdisplay(ets_res_2)
checkresiduals(e6)            

#Doing the Ljung box test for finding the significance value
LjungBox(ets_res,lags = seq(5,8,1),order=7)      # for model 1
LjungBox(ets_res_2,lags = seq(5,8,1),order=7)    # for model 6


ets_complete_data = ets(avghouprc,model = "AAN", damped = TRUE)                  ## forecasting on main dataset with model 6 AAN with damping 





summary(ets_complete_data)

forc2 = forecast(ets_complete_data,h=3)
plot(forc2)


accuracy(f6,avghouprc2)[,c(2,3,5,6)]


#####################Arima################################3
tsdisplay(avghouprc1, main="Average hOUSE PRICE", ylab="avghOUPRC", xlab="Year")

ndiffs(avghouprc1)         
ndiffs(diff(avghouprc1))  
ndiffs(diff(diff(avghouprc1)))    

tsdisplay(diff(diff(avghouprc1)))

#Hence we can go with double differentiation as it has the value 1
tsdisplay(diff(h1))

#Check with double differentiation
tsdisplay(diff(diff(h1)))

#Running auto arima
auto_arima <-auto.arima(avghouprc1, seasonal=FALSE, allowdrift = TRUE)

summary(auto_arima)                   

f_auto =forecast(auto_arima,h=h)  
accuracy(f_auto,avghouprc2)[,c(2,3,5,6)]

checkresiduals(auto_arima)

arima123 <- Arima(avghouprc1, order=c(0,2,1))
checkresiduals(arima123)


forcastarima123 =forecast(arima123 ,h=h) 



accuracy(forcastarima123,avghouprc2)[,c(2,3,5,6)]



final = Arima(avghouprc, order = c(0,2,1))

f_final = forecast(final,h=3)
plot(f_final)


###### Question 7############
final_forecast_h2 <- forecast(h2, h=24)
plot(final_forecast_h2)
