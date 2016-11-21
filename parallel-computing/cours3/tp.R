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

# 4. use of sapply
vectorize <- function(){
  Reduce("+", sapply(1:nrow(iris), FUN = function(i) leave.one.out(i)))
}


# 5. Parallelisation with parallel
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

#

# 6. sequential Foreach
#install.packages("foreach")
library(foreach)
foreachLoopSeq <- function(){
  foreach(i=1:nrow(iris), .combine = "+") %do% 
    leave.one.out(i)
  # if I want a vector with the foreach, I have to do :
  # vector <- foreach....
}

# 7. parallel Foreach


# install.packages("doParallel")
library(doParallel)

# IMPORTANT!
registerDoParallel(cl)
foreachLoopPar <- function(){
  foreach(i=1:nrow(iris), .combine = "+") %dopar% 
    leave.one.out(i)
  # if I want a vector with the foreach, I have to do :
  # vector <- foreach....
}



# comparison:

# install.packages("microbenchmark")
library(microbenchmark)
compare <- microbenchmark(parallel(), foreachLoopPar(), foreachLoopSeq(), vectorize(), forLoop(), times = 10)
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

# /!\ Not ok because of seeds :S --> see seeds handling in correction

#### ex. 3 ####
# Utiliser le jeu de données diamonds du paquet ggplot2 pour entraîner un classifier de type CART avec bagging. 
# Paralléliser l'étape de construction selon les deux paradigmes vus précédemment.

# Loading data
library("ggplot2")
data(diamonds)
attach(diamonds)
# We suppose we want to classify the diamonds on CUT
dim(diamonds)
summary(diamonds)
# install.packages("tree")
library(randomForest)
train <- sample(1:nrow(diamonds), round(nrow(diamonds) * 2/3))
tree.bag.diamonds <- randomForest(cut~., data = diamonds, subset = train, mtry = (ncol(diamonds) -1), importance=TRUE)
summary(tree.bag.diamonds)
pred.bag.diamonds <- predict(tree.bag.diamonds, newdata = diamonds[-train,])

idx_cut <- which(colnames(diamonds)=="cut")
reality <- as.matrix(diamonds[-train,idx_cut])
table(reality, pred.bag.diamonds)


# problem with this code -> how do I parallelize it ?!
# clue -> use the "tree" function and realize bagging by myself
library(doParallel)
library(foreach)
library(randomForest) # else the cmobine function won't be recognize
cl <- makeCluster(4)
clusterExport(cl, list("randomForest"))
registerDoParallel(cl)
rf <- foreach(ntree=rep(250,4), .combine=combine) %dopar%
  randomForest(cut~., data = diamonds[train,], ntree=ntree) # forced to take only ttrain, else :  <simpleError: cannot allocate vector of size 156.6 Mb>
rf
stopCluster(cl)

