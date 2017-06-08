## File: pc4ds-lab3.r
## Description: Lab 3 of Parallel Computing for Data Science Revisions
## Date:  2017 by ms

####• Parallelisme embarrassant ####

# 1.
data(iris)
# 2.
leave.one.out <- function(i) {
  fit   <- lm(Petal.Width ~ Petal.Length, data = iris[-i, ]) 
  pred  <- predict(fit, data.frame(Petal.Length = iris[i, "Petal.Length"]))
  error <- (pred - iris[i, "Petal.Length"]) ^ 2
  return(error)
}
# 3.
forLoop <- function(){
  sum <- 0
  n <- nrow(iris)
  for(i in 1:n){
    sum <- sum + leave.one.out(i)
  }
  return(sum/n)
}
# 4.
lapplyLoop <- function(){
  n <- nrow(iris)
  vec <- lapply(1:n, function(i) leave.one.out(i))
  return(sum(unlist(vec))/n)
}

# 5.
library(parallel)
nb.cores <- detectCores()
cl <- makeCluster(nb.cores-1)
clusterExport(cl = cl, list("leave.one.out"))
parlapplyLoop <- function(cl){
  vec <- parLapply(cl = cl,1:n, function(i) leave.one.out(i))
  return(sum(unlist(vec))/n)
}

# 6.
library(foreach)
foreachLoop <- function(){
  n <- nrow(iris)
  sum <- 0
  x <- foreach(i=1:n) %do% {
    leave.one.out(i)
  }
  return(sum(unlist(x))/n)
}

# 7.
library(doParallel)
registerDoParallel(cores = nb.cores-1) 
parforeachLoop <- function(){
  n <- nrow(iris)
  sum <- 0
  x <- foreach(i=1:n, .export = "leave.one.out") %dopar% {
    leave.one.out(i)
  }
  return(sum(unlist(x))/n)
}

