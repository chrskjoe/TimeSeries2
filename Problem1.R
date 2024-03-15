library(stats)
##--define functions--------------------------------
# A simulation function for ARMA simulation, use model as arima.sim, i.e. flip sign of phi (into ar) coefficients
sim <- function(model, n, nburnin=100){
  n <- n + nburnin
  # Take the ar and ma part
  ar <- model$ar
  ma <- model$ma
  # The order (i.e. the number of lags)
  p <- length(ar)
  q <- length(ma)
  # The vector for the simulation result
  y <- numeric(n)
  # Generate the random normal values
  eps <- rnorm(n)
  # Run the simulation
  for(i in (max(p,q)+1):n){
    y[i] <- eps[i] + sum(y[i-(1:p)] * ar) + sum(eps[i-(1:q)] * ma)
  }
  # Return without the burn-in period
  return(y[(nburnin+1):n])
}

simulate <- function(phi1, phi2, n, N) {
  # Initialize a list to store simulated data
  simulated_data <- list()
  # Loop through different seeds, generate data, and store in the list
  for (i in 1:N) {
    set.seed(i)
    simulated_data[[i]] <- arima.sim(model = list(ar = c(phi1, phi2)), n = n)
  }
  return(simulated_data)
}

simulate_nonstationary <- function(phi1, phi2, n, N) {
  # Initialize a list to store simulated data
  simulated_data <- list()
  # Loop through different seeds, generate data, and store in the list
  for (i in 1:N) {
    set.seed(i)
    simulated_data[[i]] <- sim(model = list(ar = c(phi1, phi2)), n)
  }
  return(simulated_data)
}

plot_sim <- function(simulated_data, N) {
  # Plot all simulated data in one plot
  plot(simulated_data[[1]], type = "l", col = "blue", ylim = range(unlist(simulated_data)), ylab = "Simulated data 1-5")
  for (i in 1:N) {
    lines(simulated_data[[i]], col = rainbow(N)[i])
  }
}

acf_theoretical <- function(phi1, phi2) {
  # Define AR coefficients in the order of AR(1) to AR(p)
  ar_coefficients <- c(phi1, phi2)
  # Compute the theoretical autocorrelation function for an AR(2) process
  acf_values <- ARMAacf(ar = ar_coefficients, ma = numeric(0), lag.max = 30)
  acf_df <- data.frame(Lag = 0:30, ACF = acf_values)
  return(acf_df)
}

acf_empirical <- function(simulated_data, N) {
  emp_acf_list <- lapply(1:N, function(i) acf(simulated_data[[i]], plot = FALSE, lag.max = 30)$acf)
  # Combine empirical ACFs into a single data frame
  emp_acf_df <- do.call(rbind, lapply(1:N, function(i) {
    data.frame(Lag = 0:30, ACF = emp_acf_list[[i]], Realization = i)
  }))
  return(emp_acf_df)
}

plot_acf <- function(acf_theoretical_df, emp_acf_df, title) {
  # Plot the empirical ACFs and theoretical ACF using ggplot2
  p <- ggplot() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_bar(data = acf_theoretical_df, aes(x = Lag, y = ACF), stat = "identity", fill = "blue", alpha = 0.5) +
    geom_point(data = emp_acf_df, aes(x = Lag, y = ACF, color = Realization), size = 1) +
    labs(title = title,
         x = "Lag",
         y = "Autocorrelation") 
  return(p)
}




##--run the code--------------------------------
# AR(2), with phi1 and phi2
phi1 <- -0.7
phi2 <- -0.2

##--1.4--------------------------------
# Define AR coefficients in the order of AR(1) to AR(p)
ar_coefficients <- c(-phi1, -phi2)
# Compute the theoretical autocorrelation function for an AR(2) process
acf_values <- ARMAacf(ar = ar_coefficients, ma = numeric(0), lag.max = 30)
acf_df <- data.frame(Lag = 0:30, ACF = acf_values)
# Plot the autocorrelation function
p <- ggplot(acf_df, aes(x = Lag, y = ACF)) +
  geom_bar(stat = "identity", color = "blue", fill = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Theoretical ACF of an AR(2) process",
       x = "Lag",
       y = "Autocorrelation")
print(p)


##--1.5--------------------------------
n <- 200
N <- 5  # Set the number of iterations
# 1.5 Simulate AR(2)
# Initialize a list to store simulated data
simluated_data <- simulate(-phi1, -phi2, n, N)
p <- plot_sim(simluated_data, N)

##--1.6 Compare theoritical and empirical ACF--------------------------------
acf_theoretical_df <- acf_theoretical(-phi1, -phi2)
acf_empirical_df <- acf_empirical(simluated_data, N)
p <- plot_acf(acf_theoretical_df, acf_empirical_df, 
  "Empirical and Theoretical ACF of an AR(2) process")
print(p)

##--1.7--------------------------------
phi1 <- -0.2
simluated_data <- simulate(-phi1, -phi2, n, N)
p <- plot_sim(simluated_data, N)
print(p)
acf_theoretical_df <- acf_theoretical(-phi1, -phi2)
acf_empirical_df <- acf_empirical(simluated_data, N)
p <- plot_acf(acf_theoretical_df, acf_empirical_df, 
  "Empirical and Theoretical ACF of an AR(2) process with phi1 = -0.2")
print(p)

##--1.8--------------------------------
phi1 <- 0.7
simluated_data <- simulate(-phi1, -phi2, n, N)
p <- plot_sim(simluated_data, N)
print(p)
acf_theoretical_df <- acf_theoretical(-phi1, -phi2)
acf_empirical_df <- acf_empirical(simluated_data, N)
p <- plot_acf(acf_theoretical_df, acf_empirical_df, 
  "Empirical and Theoretical ACF of an AR(2) process with phi1 = 0.7")
print(p)


##--1.9--------------------------------
# https://stats.stackexchange.com/questions/492613/why-theoretical-acf-in-the-not-stationary-case-is-not-exist
phi1 <- -0.8
simluated_data <- simulate_nonstationary(-phi1, -phi2, n, N)
p <- plot_sim(simluated_data, N)
print(p)
acf_theoretical_df <- data.frame(Lag = integer(), ACF = numeric(), Type = character())
acf_empirical_df <- acf_empirical(simluated_data, N)
p <- plot_acf(acf_theoretical_df, acf_empirical_df, 
  "Empirical and Theoretical ACF of an AR(2) process with phi1 = -0.8")
print(p)

##--1.10--------------------------------
phi1 <- -0.85
simluated_data <- simulate_nonstationary(-phi1, -phi2, n, N)
p <- plot_sim(simluated_data, N)
print(p)
acf_theoretical_df <- data.frame(Lag = integer(), ACF = numeric(), Type = character())
acf_empirical_df <- acf_empirical(simluated_data, N)
p <- plot_acf(acf_theoretical_df, acf_empirical_df, 
  "Empirical and Theoretical ACF of an AR(2) process with phi1 = -0.85")
print(p)

