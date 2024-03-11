# Function for plot
plotit <- function(x){
  layout(rbind(1,2:3))
  par(mar=c(3,3,1,1), mgp=c(2, 0.7,0))
  plot(x, ylab="X")
  #acf(x, lag.max=30, lwd=2)
  #pacf(x, lag.max=30, lwd=2)
}


# Function to generate ARIMA simulated data
generate_data <- function(phi1, phi2, n) {
  return(arima.sim(model = list(ar = c(phi1, phi2)), n = n))
}

# AR(2), with phi1 and phi2
phi1 <- -0.7
phi2 <- -0.2

n <- 200
x <- numeric(n)

#1.4
set.seed(1)
plotit( arima.sim(model=list(ar=c(phi1,phi2)), n=n) )


# 1.5

# Initialize a list to store simulated data
simulated_data <- list()

N <- 5  # Set the number of iterations
# Loop through different seeds, generate data, and store in the list
for (i in 1:N) {
  set.seed(i)
  simulated_data[[i]] <- generate_data(phi1, phi2, n)
}


# Plot all simulated data in one plot
plot(simulated_data[[1]], type = "l", col = "blue", ylim = range(unlist(simulated_data)), ylab = "Simulated data 1-5")
for (i in 1:N) {
  lines(simulated_data[[i]], col = rainbow(N)[i])
}

# 1.7
# Function for plot
plotit <- function(x){
  layout(rbind(1,2:3))
  par(mar=c(3,3,1,1), mgp=c(2, 0.7,0))
  #plot(x, ylab="X")
  acf(x, lag.max=30, lwd=2)
  #pacf(x, lag.max=30, lwd=2)
}


set.seed(11)
phi1 <- -0.2
plotit( arima.sim(model=list(ar=c(phi1,phi2)), n=n) )

set.seed(12)
phi1 <- 0.7
plotit( arima.sim(model=list(ar=c(phi1,phi2)), n=n) )

set.seed(13)
phi1 <- -0.8
plotit( arima.sim(model=list(ar=c(phi1,phi2)), n=n) )

set.seed(14)
phi1 <- -0.85
plotit( arima.sim(model=list(ar=c(phi1,phi2)), n=n) )
