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


