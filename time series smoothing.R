library(readr)
data <- read_csv("R/ITC-BSE.csv")
View(data)


# Create the Date column
data$Date <- as.Date(data$`date`, format = "%Y-%m-%d")

#install.packages("zoo")
# Calculate the moving average using the zoo package
library(zoo)
data$MA10 <- rollmean(data$high, k = 10, fill = NA)

# Plot the data and the moving average using the ggplot2 package
library(ggplot2)
ggplot(data, aes(x = Date)) +
  geom_line(aes(y = high), color = "blue") +
  geom_line(aes(y = MA10), color = "red") +
  labs(title = "ITC-BSE Closing Prices with 10-day Moving Average",
       x = "Date", y = "high")

#using ETS model
library(forecast)
high_price= data$high
fit= ets(high_price)
forecast <- forecast(fit, h = 10)
autoplot(forecast)



#Convert the data to a time series object
ts_data <- ts(data$high, start=c(2005, 1, 3), frequency=365)

#Moving Average
ma_data <- ma(ts_data, order=3, centre=TRUE)
plot(ma_data, main="Moving Average Smoothing", ylab="Close Price", xlab="Date")

#Exponential Smoothing
es_data <- HoltWinters(ts_data)
plot(forecast(es_data), main="Exponential Smoothing", ylab="high Price", xlab="Date")

#Moving Average
ma_data <- ma(ts_data, order=3, centre=TRUE)
ma_forecast <- forecast(ma_data, h=10)
plot(ma_forecast, main="Moving Average Smoothing and Forecast", ylab="high Price", xlab="Date")

#Exponential Smoothing
es_data <- HoltWinters(ts_data)
es_forecast <- forecast(es_data, h=10)
plot(es_forecast, main="Exponential Smoothing and Forecast", ylab="high Price", xlab="Date")

#Local Linear Regression
llr_data <- stl(ts_data, s.window="periodic")
llr_forecast <- forecast(llr_data, h=10)
plot(llr_forecast, main="Local Linear Regression and Forecast", ylab="Close Price", xlab="Date")




# Extract closing prices and convert to time series
high_prices <- ts(data$high, frequency = 1000, start = c(2005, 1))
# Apply Holt-Winters exponential smoothing
hw <- HoltWinters(high_prices)

# Print model summary
print(summary(hw))

# Plot the original closing prices and the forecast
plot(high_prices, ylim = range(c(high_prices, hw$fitted)), main = "high Prices and Forecast", xlab = "Date", ylab = "Price")
lines(hw$fitted, col = "blue")
lines(hw$forecast, col = "red")
legend("topleft", legend = c("Actual", "Fitted", "Forecast"), col = c("black", "blue", "red"), lty = 1, cex = 0.8)

# Apply Holt-Winters exponential smoothing
hw <- HoltWinters(high_prices)

# Subset hw$fitted to match the same time period as closing_prices
hw_fitted <- hw$fitted[1:length(high_prices)]

# Plot the original closing prices and the fitted values
plot(high_prices, ylim = range(c(high_prices, hw_fitted)), main = "Closing Prices and Fitted Values", xlab = "Date", ylab = "Price")
lines(hw_fitted, col = "blue")


library(ggplot2)
library(seasonal)
library(forecast)

#Transform data into time series
ts.Exports = ts(Exports$high, start=2010,frequency=12)

#Data plot
autoplot(ts.Exports) + ggtitle("U.S. Exports of Goods by F.A.S. Basis to Mainland China 2010-Present") 

#Split data into a training and testing set - approximately 20/80
train = ts(ts.Exports[1:107], start=2010, frequency=12)
test = ts(ts.Exports[108:133],start=2019, frequency=12)

#ETS ANN Model - Simple Exponential Smoothing Additive
ETS_Model1 = ets(train, model="ANN")

#Analyze model performance
summary(ETS_Model1)

autoplot(ETS_Model1)

Forecst_1 = forecast(ETS_Model1, 25)
Accuracy_1 = accuracy(Forecst_1, test[1:25])

#ETS AAA Model - Additive error, Additive trend and Additive seasonality
ETS_Model2 = ets(train, model="AAA")

#Analyze model performance
summary(ETS_Model2)
autoplot(ETS_Model2)

Forecst_2 = forecast(ETS_Model2, 25)
Accuracy_2 = accuracy(Forecst_2, test[1:25])

#ETS MNN - Simple Exponential Smoothing Multiplicative
ETS_Model3 = ets(train, model="MNN")

#Analyze model performance
summary(ETS_Model3)

autoplot(ETS_Model3)
Forecst_3 = forecast(ETS_Model3, 25)
Accuracy_3 = accuracy(Forecst_3, test[1:25])

#Plot Models
#par(mfrow=c(3,1), mar=c(4, 4, 2, 1))
#png("plot.png", width = 800, height = 800) 
plot(Forecst_1, main="ANN")
plot(Forecst_2, main="AAA")
plot(Forecst_3, main="MNN")





