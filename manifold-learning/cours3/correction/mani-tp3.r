## File: mani-tp3.r
## Description : Lab 3 of Manifold Learning 
## Date: November 2016 by jc

## 1. 3-sensor data set ####
## Artificial data as from P. Desmartines, PhD Tesis 1994
generateData <- function(n) {
  require(pdist)
  
  # these sensors where selected randomly
  sensors <- matrix(ncol = 3, data = 
    c(0.026, 0.236, -0.653, 0.310, 0.507, -0.270, -0.466,  -0.140, 0.353, -0.473,
      0.241, 0.193, 0.969, 0.094, 0.756, -0.978, -0.574, -0.502, -0.281, 0.993,
      0.026, -0.913, -0.700, 0.876, 0.216, -0.739, 0.556, -0.155, 0.431, 0.411))
  
  # draw random points on the 3d unit cube
  unitcube <- matrix(runif(3 * n, -1, 1), ncol = 3)
  
  # We ode each point as the distance to sensors : intrinsic dimension = 3
  # while extrinsic dimension = 10
  X <- as.matrix(pdist(unitcube, sensors))
  noise <- matrix(rnorm(ncol(X) * nrow(X), sd = .01), ncol = ncol(X))
  return(X + noise)
}

res100   <- generateData(100)
res1000  <- generateData(1000)
res10000 <- generateData(10000)

## 2. PCA estimator ####

pr100   <- prcomp(res100)
pr1000  <- prcomp(res1000)
pr10000 <- prcomp(res10000)

plot( pr100$sdev   / pr100$sdev[1]  , lwd = 2, type = 'l',
      ylab = "Normalized Eigenvalues", xlab = "Dimension")
lines(pr1000$sdev  / pr1000$sdev[1] , lwd = 2, col = 2)
lines(pr10000$sdev / pr10000$sdev[1], lwd = 2, col = 4)

## 3. Correlation Dimension Estimator ####
corrDim <- function(data, epsilon = 10^seq(-2, 1, length.out = 100)){
  distmat <- dist(data)
  # Determine the proportion of distances <= epsilon
  C <- numeric(length(epsilon))
  for(k in seq_along(C)) {
    C[k] <- sum(distmat <= epsilon[k]) #/ (nrow(X) - 1) / nrow(X)
  }
  
  return(list(epsilon = epsilon, C = C))
}

derivate <- function(x, y) {
  ll     <- length(y)
  deltax <- x[2] - x[1] # assumes equally spaced grid
  deltaf <- y[3:ll] - y[1:(ll - 2)]
  return(c(NA, deltaf / 2 / deltax, NA))
}

X <- generateData(1000)
Xdim <- corrDim(X, epsilon = 10^seq(-4, 1, length.out = 100))

# Plot C2 vs epsilon (in log-log)
plot(log10(Xdim$epsilon), log10(Xdim$C), type = 'l', 
     xlab = expression(log(epsilon)), ylab = expression(log(C(epsilon))))

# Plot d log(C2) vs d log(epsilon) 
plot(log10(Xdim$epsilon), derivate(log10(Xdim$epsilon), log10(Xdim$C)), 
     type = 'l')


Xdim100 <- corrDim(res100, epsilon = 10^seq(-2, 1, length.out = 100))
Xdim1000 <- corrDim(res1000, epsilon = 10^seq(-2, 1, length.out = 100))
#Xdim10000 <- corrDim(res10000, epsilon = 10^seq(-4, 1, length.out = 100))

# Plot C2 vs epsilon (in log-log)
plot(log10(Xdim100$epsilon), log10(Xdim100$C), type = 'l', ylim = c(0, 6), 
     xlab = expression(log(epsilon)), ylab = expression(log(C(epsilon))))
lines(log10(Xdim1000$epsilon), log10(Xdim1000$C), col = 2,
     xlab = expression(log(epsilon)), ylab = expression(log(C(epsilon))))
 
# Plot d log(C2) vs d log(epsilon) 
plot(log10(Xdim100$epsilon), 
     derivate(log10(Xdim100$epsilon), log10(Xdim100$C)), 
     type = 'l')
lines(log10(Xdim1000$epsilon), 
      derivate(log10(Xdim1000$epsilon), log10(Xdim1000$C)), 
      col = 2)


x <- seq(1e-5, 1, length.out = 5000)
n <- rnorm(length(x), sd = 0.005)
y <- sqrt(x) * cbind(c(cos(10 * pi * sqrt(x))), c(sin(10 * pi * sqrt(x)))) + n
plot(y)

ydim <- corrDim(data = y, epsilon = 10^seq(-4, 0, length.out = 20))

# Plot C2 vs epsilon (in log-log)
plot(log10(ydim$epsilon), log10(ydim$C), type = 'l', 
     xlab = expression(log(epsilon)), ylab = expression(log(C(epsilon))))

# Plot d log(C2) vs d log(epsilon) 
plot(log10(ydim$epsilon), derivate(log10(ydim$epsilon), log10(ydim$C)), 
     type = 'l')

