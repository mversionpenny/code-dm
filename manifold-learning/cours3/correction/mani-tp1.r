## File: mani-tp2.r
## Description : Lab 2 of Manifold Learning 
## Date: October 2016 by jc

## 1. Exercise d'application ####
rm(list = ls())

#setwd("~/ownCloud/cours/unsupervised/data/mnist/")
all    <- as.matrix(read.table("data.txt"))
labels <- read.table("labels.txt", colClasses = 'integer')

showDigit <- function(line) {
  p <- sqrt(length(line))
  mat <- matrix(as.numeric(line), p)[, p:1] # inverse column order
  image(mat, col = grey(256:1/256))
}

i <- 8; showDigit(all[i, ]); labels$V1[i]; rm(i)

showDigit(colMeans(all))


all3 <- all[labels == 3, ]

cte <- which(apply(all3, 2, sd) == 0)
pca3 <- prcomp(scale(all3[, -cte]))
plot(pca3$rotation[, 1:2])

identify(pca3$rotation[, 1:2])

ids <- c(249, 289, 359, 435)

layout(matrix(1:4, 2))
showDigit(all3[249, ])
showDigit(all3[289, ])
showDigit(all3[359, ])
showDigit(all3[435, ])

## 2. Exercise ####

# D?finition des variables
k <- 100
D <- 2
# Construction du jeu de donn?es
mat <- matrix(rnorm(k * D), ncol = D)
# Calcul de la norme par ligne
res2 <- sqrt(rowSums(mat^2))
# Visu de r?sultats
hist(res2)



layout(matrix(1:6, 2, byrow = TRUE))
# D?finition des variables
k <- 1e5
for(D in c(2, 5, 10, 20, 50, 100)) {
  # Construction du jeu de donn?es
  mat <- matrix(rnorm(k * D), ncol = D)
  # Calcul de la norme par ligne
  res <- sqrt(rowSums(mat^2))
  # Visu de r?sultats
  hist(res, main = paste("Dimension =", D), xlim = c(0, 20))

  print(sprintf("Dim:%3.0f  Mean:%.2f  Var:%.2f  CV:%.2f", 
          D, mean(res), var(res), sd(res)/mean(res) ))
}


