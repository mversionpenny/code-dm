#------------------- Margot Selosse -------------------
#----------------- Manifold Learning ------------------
#------------------ 15 oct. 2016 ----------------------

#install.packages("plot3D")
#install.packages("rgl")
library(rgl)
library(plot3D)
observationsNum <- 10000
noise <- 0.05


#### Swiss ROll ####
#TODO : title to the plot + comments

swissroll <- function(n,noise){
  p <- runif(n)
  q <- runif(n)
  t <- 3*pi/2 * (1 + 2*p)
  X <- cbind(t*cos(t), t*sin(t), 30*q) + noise * rnorm(n)

  x <- t(X[,1])
  y <- t(X[,2])
  z <- t(X[,3])
  par(mfrow = c(1, 2))
  scatter3D(x,y,z, phi= 80, theta=90) 
  scatter3D(x,y,z, phi= 40, theta=90) 
  plot3d(x,y,z, type="p",aspect =TRUE,col=rainbow(5))
  
  return(X)
}


#### Broken Swiss Roll ####
#/!\didn't understand interval of rejected ti!

brokenSwissroll <- function(n, noise){
  p <- runif(n)
  q <- runif(n)
  t <- 3*pi/2 * (1 + 2*p)
  keptIndex1 <- which(t<8,arr.ind = TRUE)
  keptIndex2 <- which(t>10,arr.ind = TRUE)
  generalKeptIndex <- sort(c(keptIndex1,keptIndex2))
  t <- t[generalKeptIndex]
  X <- cbind(t*cos(t), t*sin(t), 30*q[generalKeptIndex]) + noise * rnorm(n)[generalKeptIndex]
  
  x <- t(X[,1])
  y <- t(X[,2])
  z <- t(X[,3])
  par(mfrow = c(1, 2))
  scatter3D(x,y,z, phi= 80, theta=90) 
  scatter3D(x,y,z, phi= 40, theta=90) 
  plot3d(x,y,z, type="p",aspect =TRUE,col=rainbow(5))
  
  return(X)
}

#### Helix ####
# TODO: find how to do a good-looking helix 

helix <- function(n, noise){
  p <- runif(n)
  X <- cbind((2+cos(8*p))*cos(p), (2+cos(8*p))*sin(p), sin(8*p)) + noise * rnorm(n)
  
  x <- t(X[,1])
  y <- t(X[,2])
  z <- t(X[,3])
  par(mfrow = c(1, 2))
  scatter3D(x,y,z, phi= 80, theta=90) 
  scatter3D(x,y,z, phi= 40, theta=90) 
  plot3d(x,y,z, type="p",aspect =TRUE,col=rainbow(5))
  
  return(X)
}

newHelix <- function(n, noise){
  t <- (1:n)/n
  t <- t * 2 * pi
  X <- cbind((2+cos(8*t))*cos(t), (2+cos(8*t))*sin(t), sin(8*t)) + noise * rnorm(n)
  
  x <- t(X[,1])
  y <- t(X[,2])
  z <- t(X[,3])
  par(mfrow = c(1, 2))
  scatter3D(x,y,z, phi= 80, theta=90) 
  scatter3D(x,y,z, phi= 40, theta=90) 
  plot3d(x,y,z, type="p",aspect =TRUE,col=rainbow(5))
  
  return(X)
}

#### Twinpeaks ####

wrongTwinpeaks <- function(n, noise){
  p <- runif(n)
  q <- runif(n)
  X <- cbind(1-2*p, sin(pi - 2*pi*p), tanh(3 - 6*q)) + noise * rnorm(n)
  
  x <- t(X[,1])
  y <- t(X[,2])
  z <- t(X[,3])
  par(mfrow = c(1, 2))
  scatter3D(x,y,z, phi= 80, theta=90) 
  scatter3D(x,y,z, phi= 40, theta=90) 
  plot3d(x,y,z, type="p",aspect =TRUE,col=rainbow(5))

  return(X)
}

twinpeaks <- function(n, noise){
  p <- runif(n)
  q <- runif(n)
  X <- cbind(1-2*p, 1-2*q, sin(pi - 2*pi*p) * tanh(3 - 6*q)) + noise * rnorm(n)
  x <- t(X[,1])
  y <- t(X[,2])
  z <- t(X[,3]*10)
  par(mfrow = c(1, 2))
  scatter3D(x,y,z, phi= 80, theta=90) 
  scatter3D(x,y,z, phi= 40, theta=90) 
  plot3d(x,y,z, type="p",aspect =TRUE,col=rainbow(5))
  
  return(X)
}

#### HD ####

HD <-function(n){
  ndims = 5
  nb_per_dim = round(n ^(1/ndims))
  l <- list(seq(0,1,length=nb_per_dim))
  temp <- rep(t(l),ndims)
  t <- expand.grid(temp)
  X <- cbind(cos(t[,1]), tanh(3 * t[,2]), t[,1] + t[,3], t[,4] * sin(t[,2]), sin(t[,1] + t[,5]), t[,5] * cos(t[,2]), t[,5] + t[,4], t[,2], t[,3] * t[,4], t[,1])
  
  return(X)
}


