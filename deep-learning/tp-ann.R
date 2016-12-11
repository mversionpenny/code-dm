X <- c(0.2,0.4,0.05,0.17,0.42,0.3,0.46,0.38,0.44,0.33,0.44,0.27,0.12,0.44,0.11,0.82,0.37,0.75,0.47,0.39,0.29)
X <- matrix(X, ncol = 3, nrow = 7, byrow= TRUE)

theta <- c(-0.1,0,0.3)

W <- matrix(c(-0.4,0.1,0.5,0.1,-0.1,-0.1,-0.6,0.2,0.4,-0.1,0,0.3), nrow = 3, ncol=3, byrow=TRUE)


matresult <- matrix(rep(0,21), nrow=nrow(X), ncol =ncol(X), byrow = TRUE)
for(i in 1:nrow(matresult)){
  matresult[i,] <- X[i,1]*t(W[1,]) + X[i,2]*t(W[2,]) + X[i,3]*t(W[3,])
  matresult[i,] <- matresult[i,]>theta
}
true <-  matrix(c(1,0,0,0,1,0,0,0,1,0,1,0,1,0,0,0,0,1,0,1,0), nrow=nrow(X), byrow=TRUE)

prediction <- c(1,2,2,2,1,2,3)
reality <- c(1,2,3,2,1,3,2)

# confusion
table(prediction,reality)

# computing error
error <- (true - matresult)^2
error <- sum(error)

# computing deltaW
lambda <- 0.1
deltaW <- lambda * t(X) %*% (true-matresult)
deltaW
W <- W +deltaW
