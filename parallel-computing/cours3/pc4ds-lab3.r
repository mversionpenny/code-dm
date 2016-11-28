## File: pc4ds-lab3.r
## Description: Lab 3 of Parallel Computing for Data Science
## Date:  Nov 2016 by jc

rm(list = ls())

## Exercice 1 - 2 ####

# 1.
data(iris)
# 2.
leave.one.out <- function(i) {
 fit   <- lm(Petal.Width ~ Petal.Length, data = iris[-i, ]) 
 pred  <- predict(fit, data.frame(Petal.Length = iris[i, "Petal.Length"]))
 error <- (pred - iris[i, "Petal.Length"]) ^ 2
 return(error)
}
# 3. Sequential with for
for(i in 1:nrow(iris)) print(leave.one.out(i))
# error <- 0; for(i in 1:nrow(iris)) error <- error + leave.one.out(i)

# 4. Sequential with lapply
lapply(1:nrow(iris), FUN = function(i) leave.one.out(i))
# Reduce("+", lapply(1:nrow(iris), FUN = function(i) leave.one.out(i)))

# 5. Parallel with parallel (snow like)
library(parallel)

cl <- makeCluster(2)
clusterExport(cl, list("leave.one.out"))
res <- parLapply(cl, 1:150, fun = function(i) leave.one.out(i))
# Reduce("+", res)
stopCluster(cl)

# 6. foreach sequential
library(foreach)
foreach(i = 1:150, .combine = "+") %do% leave.one.out(i)

# 6. foreach parallel
library(foreach)    # provides foreach
library(doParallel) # provides %dopar%
registerDoParallel(cores = 4) 
foreach(i = 1:150, .combine = "+") %dopar% leave.one.out(i)
stopCluster(cl)

# More explicit (controls the stop of the cluster connection)
#cl <- makeCluster(4)
#registerDoParallel(cl) 
#foreach(i = 1:150, .combine = "+") %dopar% leave.one.out(i)
#stopCluster(cl)

## 2. kmeans avec multiple starts (sequential) ####
library(MASS)
res <- kmeans(Boston, 4, nstart = 2000)

## 2. kmeans avec multiple starts (parallel) ####
library(parallel)
seeds <- runif(2, 1L, .Machine$integer.max)

do.kmeans <- function(i) {
  set.seed(seeds[i])
  kmeans(Boston, 4, nstart = 1000)
}

cl <- makeCluster(2)
clusterExport(cl, list("seeds"))
clusterEvalQ(cl, library(MASS))
par.res <- parLapply(cl, 1:2, fun = do.kmeans)
stopCluster(cl)


