# ---------TP Manifold learning --------- #
# ------------ Margot selosse ----------- #
# ------------- 22.11.2016 -------------- #

#### 1 MDS et ACP ####
# 1.
mymds <- function(data, k){
  N <- nrow(data)
  if(class(data) == "dist"){ # TODO : define
    D <- as.matrix(data)
    N <- nrow(D)
    N1 <- rep(1,N)
    D <- D^2
    S <- -1/2 * ( D - 1/N* D%*%N1%*%t(N1) - 1/N*N1%*%t(N1)%*%D + 1/(N^2)*N1%*%t(N1)%*%D%*%N1%*%t(N1))
  }
  else{
    data <- scale(data, center = TRUE, scale = FALSE)
    S <- data %*% t(data)
  }
  svd <- svd(S)
  d <- svd$d
  v <- svd$u
  
  X <- (diag(1, nrow=k, ncol=N) * (d^(1/2))) %*% t(v)
  
  return(X)
  
}

# correction prof :
mymds2 <- function(data, s = 2) {
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
  eigenS <- eigen(S)
  Xhat <- diag(sqrt(eigenS$values[1:s])) %*% t(eigenS$vectors[, 1:s])
  return(Xhat)
}

#2.
dataN <- matrix( rnorm(10*5,mean=0,sd=1), 10, 5) 
dataDist <-  UScitiesD

set.seed(1)
test <- mymds(dataDist,2)
plot(t(test))
with(text(t(test)[,1],t(test)[,2], labels = colnames(as.matrix(dataDist))))

#3.

X <- matrix(c(1,2,2,1,3,3,3,2,2,3), nrow=5, ncol=2, byrow = TRUE)
mymds(X,1)

test.pca <- prcomp(X)
test.pca$x
# PC1 is the same

#### 2 Distances entre villes françaises ####
# no french city doc
#1.
ukdist <- as.dist(read.table("uk_dist.txt"))
#2.

par(mfrow=c(1,2))
setwd("D:/master-DM/cours/manifold-learning/cours4")
ukdist <- as.dist(read.table("uk_dist.txt"))
uknames <- read.table("uk_names.txt")
test <- mymds(ukdist,2)
plot(t(test))
with(text(t(test)[,1],t(test)[,2], labels = uknames$V1))

#data set had real x and y
ukresponse <- read.table("uk_xy.txt")
plot(ukresponse)
with(text(ukresponse[,1],ukresponse[,2], labels = uknames$V1))# the result is not the same as I have

# with real mds
fit <- cmdscale(ukdist, eig = TRUE, k = 2)
x <- fit$points[, 1]
y <- fit$points[, 2]
plot(x,y, type="n")
with(text(x,y, labels = uknames$V1))

#6.
library(MASS)
par(mfrow=c(1,2))
kruskal <- isoMDS(ukdist) 
plot(kruskal$points)
with(text(kruskal$points[,1],kruskal$points[,2], labels = uknames$V1))# the result is not the same as I have

sammon <- sammon(ukdist)
plot(sammon$points[,2],sammon$points[,1])
with(text(sammon$points[,2],sammon$points[,1], labels = uknames$V1))# the result is not the same as I have
