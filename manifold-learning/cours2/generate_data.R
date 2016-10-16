#------------------- Margot Selosse -------------------
#----------------- Manifold Learning ------------------
#------------------ 15 oct. 2016 ----------------------


library(rgl)
library(plot3D)
observationsNum <- 10000


#### Swiss ROll
#TODO : title to the plot + comments

swissroll <- function(n){
  p <- runif(n)
  q <- runif(n)
  t <- 3*pi/2 * (1 + 2*p)
  X <- cbind(t*cos(t), t*sin(t), 30*q)

  x <- t(X[,1])
  y <- t(X[,2])
  z <- t(X[,3])
  par(mfrow = c(1, 2))
  scatter3D(x,y,z, phi= 80, theta=90) 
  scatter3D(x,y,z, phi= 40, theta=90) 
  plot3D(x,y,z, type="p",aspect =TRUE,col=rainbow(5))
  
  return(X)
}


#### Broken Swiss Roll
#/!\didn't understand interval of rejected ti!

brokenSwissroll <- function(n){
  p <- runif(n)
  q <- runif(n)
  t <- 3*pi/2 * (1 + 2*p)
  keptIndex1 <- which(t<8,arr.ind = TRUE)
  keptIndex2 <- which(t>10,arr.ind = TRUE)
  generalKeptIndex <- sort(c(keptIndex1,keptIndex2))
  t <- t[generalKeptIndex]
  X <- cbind(t*cos(t), t*sin(t), 30*q[generalKeptIndex])
  
  x <- t(X[,1])
  y <- t(X[,2])
  z <- t(X[,3])
  par(mfrow = c(1, 2))
  scatter3D(x,y,z, phi= 80, theta=90) 
  scatter3D(x,y,z, phi= 40, theta=90) 
  plot3d(x,y,z, type="p",aspect =TRUE,col=rainbow(5))
  
  return(X)
}

#### Helix
# TODO: find ho to do a good-looking helix 
helix <- function(n){
  p <- runif(n)
  X <- cbind((2+cos(8*p))*cos(p), (2+cos(8*p))*sin(p), sin(8*p))
  
  x <- t(X[,1])
  y <- t(X[,2])
  z <- t(X[,3])
  par(mfrow = c(1, 2))
  scatter3D(x,y,z, phi= 80, theta=90) 
  scatter3D(x,y,z, phi= 40, theta=90) 
  plot3d(x,y,z, type="p",aspect =TRUE,col=rainbow(5))
  
  return(X)
}

#### Twinpeaks
twinpeaks <- function(n){
  p <- runif(n)
  q <- runif(n)
  X <- cbind(1-2*p, sin(pi - 2*pi*p), tanh(3 - 6*q))
  
  x <- t(X[,1])
  y <- t(X[,2])
  z <- t(X[,3])
  par(mfrow = c(1, 2))
  scatter3D(x,y,z, phi= 80, theta=90) 
  scatter3D(x,y,z, phi= 40, theta=90) 
  plot3d(x,y,z, type="p",aspect =TRUE,col=rainbow(5))

}


