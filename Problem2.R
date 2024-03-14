library(zoo)
library(readr)
library(ggplot2)
datasolar <- read_csv("datasolar.csv")

phi1 = -0.38
Phi1 = -0.94
mu = 5.72

# Format data
Yt <- datasolar$power
date <- as.yearmon(paste(datasolar$year, datasolar$month), "%Y %m")
Xt <- log(Yt) - mu
data <- data.frame(index = seq.int(length(Yt)), date = date, Yt = Yt, Xt = Xt) 

# Plot Yt
ggplot(data, aes(x = date, y = Yt)) + geom_line() + geom_point() +
  labs(title = "Solar power Yt", x = "Date", y = "Solar Power Yt")

# Plot Xt
ggplot(data, aes(x = date, y = Xt)) + geom_line() + geom_point() +
  labs(title = "Xt", x = "Date", y = "Xt")

# predict function 
predict <- function(t) {
  pred <- -(phi1 * data$Xt[t] + Phi1 * data$Xt[t - 12] + phi1 * Phi1 * data$Xt[t - 13]) 
  return(pred)
}

# 2.1 

start = 13
residuals = rep(0, length(data$Xt) - start)
for (t in start:length(data$Xt)) {
  print(paste("t = ", t))
  residuals[t - start] <- predict(t) - data$Xt[t - 1]
}
residuals <- data.frame(
  idx = seq.int(from = start, to = start + length(residuals) - 1), 
  vals = residuals
)

# scatter plot
ggplot(residuals, aes(x = idx, y = vals)) + 
  geom_point()

# density histogram with a normal distribution curve
ggplot(residuals, aes(x = vals)) + 
  geom_histogram(aes(y = ..density..), binwidth = 0.1, colour = "black", fill = "white") +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 0.22)) +
  labs(title = "Histogram of residuals", x = "Residuals", y = "Density")

# Q-Q plot
qqnorm(residuals$vals)
qqline(residuals$vals)

# Sign-Test

# Number of sign changes: Binom(N − 1, 1/2).
sign_changes <- sum(diff(sign(residuals$vals)) != 0)
# The total number of comparisons
N <- length(residuals$vals) - 1
# Expected number of sign changes for white noise
expected_sign_changes <- N / 2
# Standard deviation for the number of sign changes under the null hypothesis
std_dev <- sqrt(N / 4)
# Calculate z-score
z_score <- (sign_changes - expected_sign_changes) / std_dev
# Assuming a normal approximation, calculate the p-value
p_value <- 2 * pnorm(-abs(z_score))
# Print the results
cat("Number of sign changes:", sign_changes, "\n")
cat("Expected number of sign changes:", expected_sign_changes, "\n")
cat("Standard deviation:", std_dev, "\n")
cat("Z-score:", z_score, "\n")
cat("P-value:", p_value, "\n")

p <- 0.5  # Probability of success
# Generate binomial outcomes
binom_outcomes <- rbinom(10000, N, p)

# Plot the binomial outcomes (histogram)
binom_plot <- ggplot(data = data.frame(binom_outcomes), aes(x = binom_outcomes)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, color = "black", fill = "blue") +
  labs(title = "Binomial Outcomes and Normal Approximation",
       x = "Number of Sign Changes",
       y = "Density")

# Calculate parameters for the normal approximation
mean <- N * p
std_dev <- sqrt(N * p * (1 - p))

# Add a normal density curve
binom_plot <- binom_plot + 
  stat_function(fun = dnorm, args = list(mean = mean, sd = std_dev), color = "red", size = 1)

# Display the plot
print(binom_plot)

# 2.2
## predict function k steps
Y_pred <- rep(0, 12) 
predictk <- function(t, k) {
  Xt_minus_1 <- data$Xt[t]
  if (t + k - 1 > 36) {
    Xt_minus_1 <- Y_pred[k]
  }
  pred <- -(phi1 * Xt_minus_1 + Phi1 * data$Xt[t + k - 12] + phi1 * Phi1 * data$Xt[t + k - 13]) 
  return(pred)
}

for (k in 1:12) {
  print(paste("k = ", k))
  Y_pred[k] <- exp(predictk(36, k) + mu)
}
print(Y_pred)
Y_pred_idx <- seq.int(from = 37, to = 37 + 12 - 1)
Yt_pred <- data.frame(index = Y_pred_idx, Yt = Y_pred)
plot <- ggplot(data, aes(x = index, y = Yt)) + geom_line() +
  geom_point(color = "blue") + # Plot the data frame points in blue
  geom_point(data = Yt_pred, aes(x = index, y = Yt), color = "red") + 
  geom_line(data = Yt_pred, aes(x = index, y = Yt), color = "black") +
  labs(title = "Yt and 12-month ahead predictions",
       x = "Month index",
       y = "Power - Yt")
print(plot)

# 2.3 

sigma_eps_2 <- 0.22^2
var_pred_err <- rep(0, 12)

for (k in 1:12) {
  # compute variance 
  phi_sum <- 1
  for (i in 1:k){
    phi_sum <- phi_sum + phi1^(2*(k - i))
  }
  var_pred_err[k] <- exp(phi_sum*sigma_eps_2 + mu)
}

print(var_pred_err)
Yt_pred$lower <- Yt_pred$Yt - qnorm(0.975)*sqrt(var_pred_err)
Yt_pred$upper <- Yt_pred$Yt + qnorm(0.975)*sqrt(var_pred_err)

plot <- ggplot(Yt_pred, aes(x = index, y = Yt)) +
  geom_point(data = Yt_pred, aes(x = index, y = Yt), color = "red") + 
  geom_line(data = Yt_pred, aes(x = index, y = Yt), color = "black") +
  geom_ribbon(data = Yt_pred, aes(x = index, ymin = lower, ymax = upper), alpha=0.2, fill = "red") +
  labs(title = "Yt and 12-month ahead predictions",
       x = "Month index",
       y = "Power - Yt")
  
print(plot)


