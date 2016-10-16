## File: cla-tp1.r
## Description: Solution du TP1 cours modèles classificatoires
## Date : jan 2016

# [Ex. 1] Simulation par la méthode de rejet ######################

# 1. Fonction de densité d'une va triagulaire
# Input
#    x :  abscisse
f_tri <- function(x) {
  SUP01 <- (0 <= x) & (x < 1)
  SUP12 <- (1 <= x) & (x < 2) 
  if(SUP01) {
    return(x)
  } else { if(SUP12) { 
    return(2 - x)
  } else {
    return(0)
  }
  }
}

# Version optimizée
# f_tri2 <- function(x) {
#   SUP01 <- (0 <  x) & (x < 1)
#   SUP12 <- (1 <= x) & (x < 2) 
#   ifelse(SUP01, x, ifelse(SUP12, 2 - x, 0))
# }

# Test f_tri
# x <- seq(-1, 3, length.out = 101)
# f_tri_x <- numeric(length(x)) 
# for(i in seq_along(x)) f_tri_x[i] <- f_tri(x[i])

#plot(x, f_tri_x, type = 'l')
#rug(Observations)


# 2. 
# Input
#    fx   :  fonction de densité
#    a, b :  bornes du support de fx
#    M    :  borne supérieure de fx
rejection <- function(fx, a, b, M) {
  while(TRUE){ 
    x <- runif(1, a, b)
    y <- runif(1, 0, M)  
    if(y < fx(x)) return(x)
  }   
}


# 3. Test rejection
nreps <- 1000
Observations <- numeric(nreps)
for(i in seq_along(Observations)) 
  Observations[i] <- rejection(f_tri, 0, 2, 1)


# [Ex. 2] Estimation de la densité par histogramme ##################

# 1. 
nreps <- 1000
Observations <- numeric(nreps)
for(i in seq_along(Observations)) 
  Observations[i] <- rejection(f_tri, 0, 2, 1)

# 2. 
m1 <- 1 + 10 * log(nreps) / 3
m2 <- sqrt(nreps)

layout(matrix(1:5, 1, 5))
for(b in c(5, 25, 500, m1, m2))
  hist(Observations, breaks = seq(0, 2, length.out = b + 1))

# [Ex. 3] Estimation de la densité par noyau ##################
# 1. 
nreps <- 100
Observations <- numeric(nreps)
for(i in seq_along(Observations)) 
  Observations[i] <- rejection(f_tri, 0, 2, 1)

#2.
minimum <- min(sd(Observations), IQR(Observations)/1.34)
bwValue <- 1.06*minimum*nreps^(-1/5)
plot(density(Observations,bw=bwValue))

#3 estime le risque J
riskkde <- function(obs, h){
  fhat=Vectorize(function(x) density(obs,from=x,to=x,n=1,bw=h)$y)
  fhati=Vectorize(function(i) density(obs[-i],from=obs[i],to=obs[i],n=1,bw=h)$y)
  F=fhati(1:length(obs))
  return(integrate(function(x) fhat(x)^2,-10,10)$value-2*mean(F))
}
J <- c()
for (h in seq(0.01,0.99,0.01))
  J <- c(J, riskkde(Observations, h))
plot(seq(0.01,0.99,0.01), J, type = 'l')

#argmin (h optimis�)
indexArgmin <- which.min(J)
argmin <- seq(0.01, 0.99, 0.01)[indexArgmin]

# [Ex. 4] Exercices additionnels ##################
#1.
library(datasets)
library(ks)
par(mfrow = c(1,2))
eruptions <- faithful$eruptions
waiting <- faithful$waiting
plot(density(faithful$eruptions))
plot(density((faithful$waiting)))

matrice <- matrix(c(eruptions,waiting), nrow = length(eruptions), ncol = 2)
plot(kde(matrice),display="persp", thin=3, border=1, col="white")
