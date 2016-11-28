## File: pc4ds-lab4.r
## Description: Lab 4 of Parallel Computing for Data Science
## Date:  Nov 2016 by jc

rm(list = ls())

# Ex1. Que fait ce code? ####
dormir <- function(i) {
  Sys.sleep(i)
  return(paste("le fils", Sys.getpid(), "a dormi", i, "secondes"))
}

temps <- list(5, 30, 5, 10)
temps <- list(30, 5, 5, 10)

library(parallel)
# Avec multicore (si disponible)
# mclapply(temps, dormir, mc.cores = 2)
# Avec SNOW
cl <- makeCluster(2)
res <- parLapply(cl, temps, dormir)
res <- clusterApply(cl, list(c(30),c(5,5,10)), dormir)

## Exercice 2 ####
doichunk <- function(ichunk) {
  tot <- 0
  nr  <- nrow(lnks) # lnks global at worker
  
  for(i in ichunk) {
    tmp <- lnks[(i + 1):nr , ] %*% lnks[i , ]
    tot <- tot + sum(tmp)
  }
  
  tot
}

mutoutpar <- function(cls, lnks) {
  require(parallel)
  
  nr <- nrow(lnks) # lnks global at worker
  clusterExport(cls, "lnks")
  
  ichunks <- 1:(nr - 1) # each "chunk" has only 1 value of i , for now
  tots <- clusterApply(cls, ichunks, doichunk)
  
  Reduce(sum, tots ) / nr
}

mutoutparB <- function(cls, lnks) {
  require(parallel)
  
  nr <- nrow(lnks) # lnks global at worker
  clusterExport(cls, "lnks")
  
  ichunks <- (nr-1):1 # each "chunk" has only 1 value of i , for now
  tots <- clusterApply(cls, ichunks, doichunk)
  
  Reduce(sum, tots ) / nr

}

mutoutparC <- function(cls, lnks) {
  require(parallel)
  
  nr <- nrow(lnks) # lnks global at worker
  clusterExport(cls, "lnks")
  
  ichunks <- clusterSplit(cls,1:(nr-1)) 
  tots <- clusterApply(cls, ichunks, doichunk)
  
  Reduce(sum, tots ) / nr
}

mutoutparD <- function(cls, lnks) {
  require(parallel)
  
  nr <- nrow(lnks) # lnks global at worker
  clusterExport(cls, "lnks")
  
  sample <- sample(1:499,499)
  ichunks <- clusterSplit(cls,sample) # each "chunk" has only 1 value of i , for now
  tots <- clusterApply(cls, ichunks, doichunk)
  
  Reduce(sum, tots ) / nr
}

mutoutparE <- function(cls, lnks) {
  require(parallel)
  
  nr <- nrow(lnks) # lnks global at worker
  clusterExport(cls, "lnks")
  
  sequence1 <- seq(1,(nr-1),by=4)
  sequence2 <- seq(2,(nr-1), by=4)
  sequence3 <- seq(3,(nr-1), by=4)
  sequence4 <- seq(4,(nr-1), by=4)
  list <- cbind(sequence1,sequence2,sequence3,sequence4)
  ichunks <- clusterSplit(cls,list) # each "chunk" has only 1 value of i , for now
  tots <- clusterApply(cls, ichunks, doichunk)
  
  Reduce(sum, tots ) / nr
}

## Benchmark and timings with a simulated dataset ####

# Simulate a link matrix of size n (e.g. n = 500)
n   <- 500
lnk <- matrix(sample(0:1 , n^2, replace = TRUE), nrow = n)


## Paralle computation with snow
library(parallel)
cls <- makeCluster(4) 
lnks <- lnk
system.time(mutoutpar(cls, lnk))
system.time(mutoutparB(cls, lnk))
system.time(mutoutparC(cls, lnk))
stopCluster(cls); rm(cls)

# You may use microbenchmark to have more significant timmings

library(microbenchmark)
compare <- microbenchmark(mutoutpar(cls,lnks),mutoutparB(cls,lnks), mutoutparC(cls,lnks), mutoutparD(cls,lnks),  mutoutparE(cls,lnks), times = 10)
# install.packages("ggfortify")
library(ggfortify)
autoplot(compare) # Plot benchmark results

## Exercice 3 ####
data(iris)
leave.one.out <- function(i, dataset) {
  fit   <- lm(Petal.Width ~ Petal.Length, data = dataset[-i, ]) 
  pred  <- predict(fit, data.frame(Petal.Length = dataset[i, "Petal.Length"]))
  error <- (pred - dataset[i, "Petal.Length"]) ^ 2
  return(error)
}

compute.PRESS <- function(dataset) {
  Reduce("+", lapply(1:nrow(dataset), function (i) leave.one.out(i, dataset)))
}

simuData <- function(sizes){
  model <- lm(Petal.Width ~ Petal.Length, data = iris)
  lapply(sizes, function (n) {
    a <- min(iris[, "Petal.Length"])
    b <- max(iris[, "Petal.Length"])
    iris2 <- data.frame(Petal.Width = rep(NA, n),
                        Petal.Length = runif(n, a, b))
    iris2[, "Petal.Width"] <- 
      predict(model, data.frame(Petal.Length = iris2[, "Petal.Length"]))
    return(iris2)
  })
}


library(parallel)


# Situation 1 
multiple.iris <- simuData(rep(200, 8))


stopCluster(cl)

# foreach parallel
library(foreach)    # provides foreach
library(doParallel) # provides %dopar%
registerDoParallel(cores = 4) 
...
stopCluster(cl)
