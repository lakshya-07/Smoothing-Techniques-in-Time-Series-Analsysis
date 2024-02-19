library(readr)
library(forecast)
library(tseries)
library(ggplot2)
library(Metrics)

#cols <- c("year", "month", "frac_date", "mean_spots", "mean_std", "observations", "definitive")
df <- sunspot_data

samp <- df
head(samp, 10)

#missing values
sum(is.na(data))

#function to convert fractional year to normal date
library(lubridate)

fraction_year_to_date <- function(fractional_year) {
  
  year <- as.integer(fractional_year)
  
  remaining_fraction <- fractional_year - year
  days_in_year <- 365 + as.integer(year %% 4 == 0 & (year %% 100 != 0 | year %% 400 == 0))
  
  total_days <- as.integer(remaining_fraction * days_in_year)
  
  base_date <- as.Date(paste0(year, "-01-01"))
  
  target_date <- base_date + days(total_days)
  return(format(target_date, "%d-%m-%Y"))
}

fractional_year <- 1749.042
fraction_year_to_date(fractional_year)

# Get the dimensions of the data frame
dim(samp)


library(ggplot2)

ggplot(samp, aes(x=frac_date, y=mean_spots)) +
  geom_line(color = "blue") +
  ggtitle('Monthly Mean Total Sunspot Number (1749-2023)') +
  xlab('Year') +
  ylab('Mean Sunspot Number') +
  theme_bw(base_size = 12) +
  theme(plot.margin = unit(c(10, 6, 10, 6), "points")) +
  theme(legend.position="bottom") +
  guides(colour=guide_legend(title='Original Data'))

library(zoo)

#Moving Average
window_size <- 12
samp$MA <- rollapply(samp$mean_spots, width = window_size, FUN = mean, fill = NA, align = "right", partial = TRUE)
head(samp, 10)

ggplot(samp, aes(x = frac_date)) +
  geom_line(aes(y = mean_spots), color = "lightblue") +
  geom_line(aes(y = MA), color = "blue") +
  labs(title = 'Monthly Mean Total Sunspot Number with Moving Average',
       x = 'Year',
       y = 'Mean Sunspot Number') +
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(title = "Series"))



#Exponential Smoothing (Simple Exponential Smoothing)
ses_model <- ses(samp$mean_spots, alpha = 0.2)
samp$SES <- fitted(ses_model)

# Plot Exponential Smoothing
ggplot(samp, aes(x = frac_date)) +
  geom_line(aes(y = mean_spots), color = "lightblue") +
  geom_line(aes(y = SES), color = "red") +
  labs(title = 'Monthly Mean Total Sunspot Number with Exponential Smoothing',
       x = 'Year',
       y = 'Mean Sunspot Number') +
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(title = "Series"))

# Double Exponential Smoothing without Damp Trend (Holt's Linear Trend Method)
holt_model <- holt(samp$mean_spots, alpha = 0.2, beta = 0.2)
samp$Holt <- fitted(holt_model)

# Plot Double Exponential Smoothing without Damp Trend
ggplot(samp, aes(x = frac_date)) +
  geom_line(aes(y = mean_spots), color = "lightblue") +
  geom_line(aes(y = Holt), color = "pink4") +
  labs(title = 'Monthly Mean Total Sunspot Number with Double Exponential Smoothing (No Damp Trend)',
       x = 'Year',
       y = 'Mean Sunspot Number') +
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(title = "Series"))

#Double Exponential Smoothing with Damp Trend
holt_damped_model <- holt(samp$mean_spots, damped=TRUE, alpha = 0.2, beta = 0.2)
samp$Holt_Damped <- fitted(holt_damped_model)

#Double Exponential Smoothing with Damp Trend
ggplot(samp, aes(x = frac_date)) +
  geom_line(aes(y = mean_spots), color = "lightblue") +
  geom_line(aes(y = Holt_Damped), color = "green4") +
  labs(title = 'Monthly Mean Total Sunspot Number with Double Exponential Smoothing (Damp Trend)',
       x = 'Year',
       y = 'Mean Sunspot Number') +
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(title = "Series"))

#Triple Exponential Smoothing (Holt-Winters Method)
hw_model <- hw(samp$mean_spots, alpha = 0.2, beta = 0.2, phi = 0.9)
samp$Holt_Winters <- fitted(hw_model)

#Plot Triple Exponential Smoothing
ggplot(samp, aes(x = frac_date)) +
  geom_line(aes(y = mean_spots), color = "lightblue") +
  geom_line(aes(y = Holt_Winters), color = "blue3") +
  labs(title = 'Monthly Mean Total Sunspot Number with Triple Exponential Smoothing',
       x = 'Year',
       y = 'Mean Sunspot Number') +
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(title = "Series"))




#######################Examples########################################

# Load required library
library(forecast)
#Example 1: Additive Error, Additive Trend, and Additive Seasonality (ETS(A, A, A))
# Example time series data
ts_data <- ts(c(23, 27, 30, 33, 36, 40, 45, 50, 55, 60), frequency = 12)
ets_model <- ets(ts_data, model = "AAA")
summary(ets_model)


# Example 2: Multiplicative Error, Additive Trend, and Multiplicative Seasonality (ETS(M, A, M))
ts_data <- ts(c(100, 120, 140, 160, 180, 200, 220, 240, 260, 280), frequency = 12)
ets_model <- ets(ts_data, model = "MAA")
summary(ets_model)


# Example 3: No Error, Additive Trend, and Multiplicative Seasonality (ETS(N, A, M))
# Example time series data
ts_data <- ts(c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100), frequency = 12)
ets_model <- ets(ts_data, model = "NAE")
summary(ets_model)


#######################Examples########################################


#calculating MSE and MAS

# Load required libraries
library(Metrics)

# 1. Exponential Smoothing (Simple Exponential Smoothing)
ses_model <- ses(samp$mean_spots,  alpha = 0.2)
samp$SES <- fitted(ses_model)

# Calculate MSE and MAE for SES
mse_ses <- mse(samp$SES, samp$mean_spots)
mae_ses <- mae(samp$SES, samp$mean_spots)
cat("SES Model Metrics:\n")
cat("MSE:", mse_ses, "\n")
cat("MAE:", mae_ses, "\n\n")

# 2. Double Exponential Smoothing without Damp Trend (Holt's Linear Trend Method)
holt_model <- holt(samp$mean_spots,alpha = 0.2, beta = 0.2)
samp$Holt <- fitted(holt_model)

# Calculate MSE and MAE for Holt
mse_holt <- mse(samp$Holt, samp$mean_spots)
mae_holt <- mae(samp$Holt, samp$mean_spots)
cat("Holt Model Metrics:\n")
cat("MSE:", mse_holt, "\n")
cat("MAE:", mae_holt, "\n\n")

# 3. Double Exponential Smoothing with Damp Trend
holt_damped_model <- holt(samp$mean_spots, damped=TRUE, alpha = 0.2, beta = 0.2)
samp$Holt_Damped <- fitted(holt_damped_model)

# Calculate MSE and MAE for Holt with Damped Trend
mse_holt_damped <- mse(samp$Holt_Damped, samp$mean_spots)
mae_holt_damped <- mae(samp$Holt_Damped, samp$mean_spots)
cat("Holt with Damped Trend Model Metrics:\n")
cat("MSE:", mse_holt_damped, "\n")
cat("MAE:", mae_holt_damped, "\n\n")

# 4. Triple Exponential Smoothing (Holt-Winters Method)
hw_model <- hw(samp$mean_spots, alpha = 0.2, beta = 0.2, phi = 0.9)
samp$Holt_Winters <- fitted(hw_model)

# Calculate MSE and MAE for Holt-Winters
mse_hw <- mse(samp$Holt_Winters, samp$mean_spots)
mae_hw <- mae(samp$Holt_Winters, samp$mean_spots)
cat("Holt-Winters Model Metrics:\n")
cat("MSE:", mse_hw, "\n")
cat("MAE:", mae_hw, "\n")


#Choosing the best model
model_best <- ets(samp$mean_spots)
result <- fitted(model_best)
samp$Triple_Exp_Smooth <- result

summary(model_best)
head(samp, 10)

library(ggplot2)
ggplot(samp, aes(x=frac_date)) +
  geom_line(aes(y=mean_spots), linetype="dashed", size=0.5, color="lightblue", label='Original Data') +
  geom_line(aes(y=Triple_Exp_Smooth), color="orange3", label='Triple Exponential Smoothing') +
  labs(title='Monthly Mean Total Sunspot Number ANN Smoothing',
       x='Year',
       y='Mean Sunspot Number') +
  theme_bw() +
  theme(legend.position="bottom") +
  guides(colour=guide_legend(title="Series"))
