## File: pc4ds_lab5.r
## Description: Sample code on how to profile in R. 
##              Material from The Art of R programming (N. Matloff, 2011)
## Date: Mars 16 by jc

rm(list = ls())

## I. Loop vs Vectorization ####
x <- runif(1e6)
y <- runif(1e6)
z <- vector(length = 1e6)
system.time(z <- x + y)
system.time(for(i in 1:length(x)) z[i] <- x[i] + y[i])


## II. Polynomial regression ####
# forms matrix of powers of the vector x, through degree dg
powers1 <- function(x, dg) {
  pw <- matrix(x,nrow = length(x))
  prod <- x # current product
  for (i in 2:dg) {
    prod <- prod * x
    pw <- cbind(pw,prod)
  }
  return(pw)
}


# forms matrix of powers of the vector x, through degree dg
powers2 <- function(x, dg) {
  pw <- matrix(nrow = length(x), ncol = dg)
  prod <- x # current product
  pw[,1] <- prod
  for (i in 2:dg) {
    prod <- prod * x
    pw[,i] <- prod
  }
  return(pw)
}

powers3 <- function(x, dg) return(outer(x, 1:dg, "^"))

powers4 <- function(x, dg) {
  repx <- matrix(rep(x, dg), nrow = length(x))
  return(t(apply(repx, 1, cumprod)))
}


x <- runif(1e6)
system.time(powers1(x, 8))
system.time(powers2(x, 8))
system.time(powers3(x, 8))
system.time(powers4(x, 8))


## DO NOT RUN THIS 
# library(microbenchmark)   # benchmar tool (like system.time(replications(...)))
# library(ggplot2)          # nice graphics
# 
compare1 <- microbenchmark(powers1(x, 8),
                           powers2(x, 8),
                           powers3(x, 8),
                           powers4(x, 8),
                           times = 10)
autoplot(compare1)

## Profil powers1
Rprof()
invisible(powers1(x,8))
Rprof(NULL)
summaryRprof()


Rprof()
invisible(powers2(x,8))
Rprof(NULL)
summaryRprof()

Rprof()
invisible(powers3(x,8))
Rprof(NULL)
summaryRprof()

Rprof()
invisible(powers4(x,8))
Rprof(NULL)
summaryRprof()


#### Profilage II ####
data_coor <- matrix(runif(4 * 10), ncol = 10)
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

Rprof()
invisible(mymds(data_coor))
Rprof(NULL)
summaryRprof()
