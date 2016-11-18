# ---------TP Paralel Computing --------- #
# ------------ Margot selosse ----------- #
# ------------- 18.11.2016 -------------- #

#### Loading data ####
# 1.
data("iris")
attach(iris)

#### ex1. #####
# 2.
library(MASS)
leave.one.out2 <- function(i){
  fit <- lm(Petal.Width~Petal.Length, data=iris[-i,])
  pred <- predict(fit, data.frame(Petal.Length = iris[i, "Petal.Length"]))
  return((pred-iris[i, "Petal.Width"])^2)
}

leave.one.out <- function(i) {
  fit   <- lm(Petal.Width ~ Petal.Length, data = iris[-i, ])
  pred  <- predict(fit, data.frame(Petal.Length = iris[i, "Petal.Length"]))
  error <- (pred - iris[i, "Petal.Length"]) ^ 2
  return(error)
}

# 3. error generalisation 
forLoop <- function(){
  error <- 0
  for(i in 1:nrow(iris)){
    error <- error + leave.one.out(i)
    
  }
}


vectorize <- function(){
  Reduce("+", sapply(1:nrow(iris), FUN = function(i) leave.one.out(i)))
}


# 4. Parallelisation
#install.packages("parallel")
library(parallel)

# Cluster initialization
cl <- makeCluster(4)
clusterExport(cl, list("leave.one.out"))

# parallelization with paLapply
parallel<- function(){
  res <- parLapply(cl,1:150, fun = function(i) leave.one.out(i))
  Reduce("+", res)
}


# parallelization with foreach
#install.packages("foreach")
library(foreach)
# install.packages("doParallel")
library(doParallel)

# IMPORTANT!
registerDoParallel(cl)
foreachLoop <- function(){
  foreach(i=1:nrow(iris), .combine = "+") %dopar% 
    leave.one.out(i)
  # if I want a vector with the foreach, I have to do :
  # vector <- foreach....
}



# comparison:

# install.packages("microbenchmark")
library(microbenchmark)
compare <- microbenchmark(parallel(), foreachLoop(), vectorize(), forLoop(), times = 10)
# install.packages("ggfortify")
library(ggfortify)
autoplot(compare) # Plot benchmark results

# Finalization clusters
stopCluster(cl)


#### ex. 2 ####
library(MASS)
attach(Boston)
# serial :
res <- kmeans(Boston, 4, nstart=2000)

# parallel :

initialization <- function(i){
  # take four lines
  return(as.matrix(Boston[sample(nrow(Boston), 4), ]))
}

cl <- makeCluster(4)
clusterExport(cl, list("initialization","Boston"))
res <- parLapply(cl,1:2000, fun = function(i) {
                                  initialization(i)
                                  kmeans(as.matrix(Boston), centers=initialization(i))
                                })

stopCluster(cl)
