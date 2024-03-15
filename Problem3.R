# Load necessary libraries
library(forecast)


##--3.1----------------------------------------------------------------
# Set the parameters
phi1 <- 0.6
n <- 100  # The number of observations

# Simulate an AR(1) model with phi1 = 0.6
set.seed(123)  # Set seed for reproducibility
ar1_model <- arima.sim(n = n, list(ar = c(phi1)))

# Plot the time series
plot(ar1_model, main = "AR(1) Time Series with phi1 = 0.6", ylab = "Value")

# Plot the ACF and PACF
acf(ar1_model, main = "ACF of AR(1) Model")
pacf(ar1_model, main = "PACF of AR(1) Model")


##--3.2----------------------------------------------------------------
# Set the parameters
Phi1 <- -0.9
n <- 120  # The number of observations, 10 years of monthly data

# Simulate a SAR(1) model with Phi1 = -0.9
set.seed(123)  # Set seed for reproducibility
sar1_model <- arima.sim(n = n, list(order = c(0, 0, 0), seasonal = list(order = c(1, 0, 0), period = 12), sar = c(Phi1)))

# Plot the time series
plot(sar1_model, main = "SAR(1) Time Series with Phi1 = -0.9", ylab = "Value")

# Plot the ACF and PACF
acf(sar1_model, main = "ACF of SAR(1) Model")
pacf(sar1_model, main = "PACF of SAR(1) Model")

##--3.3----------------------------------------------------------------
# Set the parameters
phi1 <- 0.9
Theta1 <- -0.7
n <- 120  # The number of observations, for example, 10 years of monthly data

# Simulate an ARIMA (1, 0, 0) x (0, 0, 1)12 model
set.seed(123)  # Set seed for reproducibility
arima_model <- arima.sim(n = n, 
                         list(order = c(1, 0, 0), 
                              seasonal = list(order = c(0, 0, 1), period = 12),
                              ar = c(phi1), 
                              ma = c(Theta1)))

# Plot the time series
plot(arima_model, main = "ARIMA (1, 0, 0) x (0, 0, 1)12 Time Series", ylab = "Value")

# Plot the ACF and PACF
acf(arima_model, main = "ACF of ARIMA (1, 0, 0) x (0, 0, 1)12")
pacf(arima_model, main = "PACF of ARIMA (1, 0, 0) x (0, 0, 1)12")
