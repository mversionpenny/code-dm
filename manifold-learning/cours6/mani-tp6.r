## File: mani-tp6.r
## Description : Lab 6 of Manifold Learning 
## Date: November 2016 by jc

library(rgl) # nice 3d graphs
library(vegan)

rm(list = ls())

n <- 1000 # Random position on the parameteric domain.
u <- matrix(runif(2 * n), ncol = 2)

v <- 3 * pi / 2 * (0.1 + 2 * u[, 1])

x <- -cos(v) * v
y <- 20 * u[, 2]
z <- sin(v) * v

swissroll <- cbind(x, y , z)
plot3d(swissroll[order(v), ], col = rainbow(n), size = 10)

# MDS
fit <- cmdscale(dist(swissroll), k = 2, eig = TRUE)

plot(fit$eig[1:20]/fit$eig[1], type = 'h', 
     main = "Normalized eigenvalues from MDS")
plot(fit$points[order(v), ], col = rainbow(n), pch = 19)

# Isomap
d <- dist(swissroll)
fit2 <- isomap(d, ndim = 2, k = 13)
plot(fit2$eig[1:20]/fit$eig[1], type = 'h', 
     main = "Normalized eigenvalues from Isomap")
plot(fit2$points[order(v), ], col = rainbow(n), pch = 19)

# Ex 2
...