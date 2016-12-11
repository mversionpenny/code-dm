## File: mani-tp4.r
## Description : Lab 4 of Manifold Learning 
## Date: November 2016 by jc

## 1. MDS & PCA ####

data_coor <- matrix(runif(4 * 10), ncol = 10)
data_dist <- dist(t(data_coor))

mymds <- function(data, s = 2) {
  if(is(data, "dist")) {
    D <- as.matrix(data)^2
    N <- nrow(D)
    rMeans <- matrix(rowMeans(D), ncol = N, nrow = N)
    cMeans <- matrix(colMeans(D), ncol = N, nrow = N, byrow = TRUE)
    S <- - 0.5 * (D - rMeans - cMeans + mean(D))
  } else {
    Y <- scale(data, center = TRUE, scale = FALSE)
    S <- t(Y) %*% Y
  }
  svdS <- svd(S)
  Xhat <- diag(sqrt(svdS$d[1:s])) %*% t(svdS$v[, 1:s])
  return(Xhat)
} 

# TEST : compare result with cmdscale
cbind(t(mymds(data_dist, 2)),
      cmdscale(data_dist, 2))

## 2. Distances entre villes françaises
# setwd("~/enseignement/Manifold Learning")

# 1. 
distfr <- as.matrix(read.csv("distfr.csv", row.names = 1))
# 2.o
mdsfr <- cmdscale(distfr, 2)
plot(mdsfr, type = "n")
text(mdsfr, labels = rownames(mdsfr))
# 3. look for an elbow (at s =~ 3)
eig <- cmdscale(distfr, 2, eig = TRUE)$eig
plot(eig / eig[1], type = "b") 
# 4. 
distfr2 <- dist(mdsfr)
# 5. (root mean square error) : over 40 kms
sqrt(mean((as.dist(distfr) - distfr2)^2))
# 6.
library(MASS)

isomdsfr <- isoMDS(distfr)
plot(isomdsfr$points, type = "n")
text(isomdsfr$points, labels = rownames(isomdsfr$points))
distfr3 <- dist(isomdsfr$points) 
sqrt(mean((as.dist(distfr) - distfr3)^2))

sammonfr <- sammon(distfr)
plot(sammonfr$points, type = "n")
text(sammonfr$points, labels = rownames(sammonfr$points))
distfr4 <- dist(sammonfr$points) 
sqrt(mean((as.dist(distfr) - distfr4)^2))

