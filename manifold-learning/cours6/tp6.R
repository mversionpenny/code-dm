# ------------- TP manifold --------------#
# ------------ Margot selosse ----------- #
# ------------- 07.12.2016 -------------- #

# 1. Application sur des variétés développables ####

# install.packages("vegan")
library(vegan)

swissroll <- function(n,noise){
  p <- runif(n)
  q <- runif(n)
  t <- 3*pi/2 * (1 + 2*p)
  X <- cbind(t*cos(t), t*sin(t), 30*q) + noise * rnorm(n)
  
  x <- t(X[,1])
  y <- t(X[,2])
  z <- t(X[,3])
  par(mfrow = c(1, 2))
  #scatter3D(x,y,z, phi= 80, theta=90) 
  #scatter3D(x,y,z, phi= 40, theta=90) 
  #plot3d(x,y,z, type="p",aspect =TRUE,col=rainbow(5))
  
  return(X)
}

X <- swissroll(5000,0.05)
distances <- dist(X)
cmdscale <- cmdscale(distances)
isomap <- isomap(distances, ndim=2, k=7)

# displaying
par(mfrow=c(1,2))
plot(cmdscale,col=jet.col(5000))
plot(isomap, col=jet.col(5000))

# using 4 cores : not tested
library(doParallel)
library(foreach)
cl <- makeCluster(4)
clusterExport(cl)
# IMPORTANT!
registerDoParallel(cl)
foreachLoopPar <- function(){
  foreach(i=c(13,20,30,50)) %dopar% 
    isomap <- isomap(distances, ndim=2, k=i)
}

#2. ACP vs ACP à noyau ####s

library(rgl)
library(plot3D)

# circles
x1 <- matrix(rnorm(300),nc=3)
y1 <- x1/sqrt(rowSums(x1^2))

x2 <- matrix(rnorm(300),nc=3)
y2 <- 0.5*x2/sqrt(rowSums(x2^2))

#plot3d(y1, type="p",aspect =TRUE)
#plot(y1)
plot(y1)
points(y2, col="green")

# rings

anneaux <- function(n0 = 150){
  rayon_int <- runif(n0, 50,   80)
  rayon_ext <- runif(n0, 200, 230)
  
  rayons    <- c(rayon_int, rayon_ext)
  angles    <- runif(2*n0, 0, 2*pi)
  
  return(cbind(rayons * cos(angles),
               rayons * sin(angles)))
}

n <- 200
x <- anneaux(n)
plot(x, pch = 19, col = rep(2:1, each = n))
plot3d(x, type="p",aspect =TRUE)

# install.packages("e1071")
# install.packages("kernlab")
library(kernlab)

pca.lin <- prcomp(x)
plot(pca.lin$x[,1], pca.lin$x[,2], col = rep(2:1, each = n))

pca.k <- kpca(x, data=as.data.frame(x), kernel="rbfdot", kpar = list(sigma=0.0001), features = 2)

plot(rotated(pca.k), col = rep(2:1, each = n))
