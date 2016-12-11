setwd("C:/Users/Hoai Thu Nguyen/Dropbox/DM/ML")
stress <- function(dx,dy){
  c <- sum(dy)
  s <- 1/c * (dy-dx)^2/dy
  return(s)
} 

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
  Xhat <- t(Xhat)
  return(Xhat)
} 

mySammon <- function(data, p){
  # Init
  dy <- data
  c <- sum(dy)
  newXp <- mymds(data,p)
  stress <- function(dx) return(1/c * sum((dy-dx)^2/dy))
 
  #Update
  dx <- dist(newXp)
  new_stress <- stress(dx)
  sub = 1
  while (sub>0){
    Xp <- newXp
    old_stress <- new_stress
    matX <- as.matrix(dx)
    matY <- as.matrix(dy)
    for (i in 1:nrow(Xp)){
      div1 <- 0
      div2 <- 0
      for (j in 1:nrow(Xp)){
        if (j != i){
          div1 <- div1 + (matY[i,j] - matX[i,j]) / matY[i,j]/matX[i,j]*(Xp[i,]- Xp[j,])
          div2 <- div2 + (matY[i,j] - matX[i,j]) / matY[i,j]/matX[i,j] - (Xp[i,]- Xp[j,])^2/(matX[i,j]^3)
        }
      }
      minus <- 0.3 * (-div1)/abs(-div2) 
      newXp[i,] <- Xp[i,] - minus
    }
    dx <- dist(newXp)
    new_stress <- stress(dx)
    sub <- old_stress - new_stress
  }
  return(Xp)
}
data <- UScitiesD
test <- mySammon(data,2)
plot(test, type = "n")
with(text(test[,1], test[,2], labels = colnames(as.matrix(data))))
