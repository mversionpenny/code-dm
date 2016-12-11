# ---------TP Manifold learning --------- #
# ------------ Margot selosse ----------- #
# ------------- 30.11.2016 -------------- #

# We have to implement Sammon mapping

data("UScitiesD")
Y <- as.matrix(UScitiesD)

sammonMapping <- function(Y, p, alpha, threshold=0.4){
  Xp <- prcomp(Y)$x[,1:p]
  
  stressTemp <- 10000000
  for(i in 1:15000){
    distanceX <- as.matrix(dist(Xp))
    Xp <- Xp - alpha * derivate(Y,Xp,distanceX)/abs(doublederivate(Y,Xp,distanceX))
  }
  return(Xp)

  
}

computeStress <- function(Y,X){
  distancesY <- as.dist(t(Y),Y)
  distancesX <- as.dist(t(X),X)
  c <- sum(distancesY)
  1/c * sum((distancesY-distancesX)^2 / distancesY)
  
}

derivate <- function(Y, Xp, distancesX){
  result <- matrix(rep(0,nrow(Xp)*ncol(Xp)), nrow=nrow(Xp))

  c <- sum(Y)
  for(i in 1:nrow(Xp)){
    sum <- 0
    for(j in 1:nrow(Xp)){
      if(j!=i){
        sum <- sum + (Y[i,j]-distancesX[i,j])/(distancesX[i,j]*distancesX[i,j]) * (Xp[i,]-Xp[j,])
      }
    }
    result[i,] <- -2/c * sum
  }
  return(result) 
}

doublederivate <- function(Y,Xp, distancesX){
  result <- matrix(rep(0,nrow(Xp)*ncol(Xp)), nrow=nrow(Xp))

  c <- sum(Y)
  for(i in 1:nrow(Xp)){
    sum <- 0
    for(j in 1:nrow(Xp)){
      if(j!=i){
        sum <- sum + (Y[i,j]-distancesX[i,j])/(distancesX[i,j]*distancesX[i,j]) - 
                                   ((Xp[i,]-Xp[j,])^2)/(distancesX[i,j])^3
      }
      
    }
    result[i,] <- -2/c * sum
  }
  return(result) 
}

test <- sammonMapping(Y,2,0.3)
plot(test)
with(text(test[,1],test[,2], labels = colnames(Y)))
