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

