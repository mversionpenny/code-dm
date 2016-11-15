#### mclustDA ####
#install.packages('mclust')
library(mclust)
data("iris")
nrow(iris)
iris <- iris[sample(1:150,150),]
irisX <- iris[,-(5)]
irisY <- iris[,-(1:4)]
test_idx <- 1:50
train.X <- irisX[-test_idx,]
test.X <- irisX[test_idx,]
train.Y <-irisY[-test_idx]
test.Y <- irisY[test_idx]

iris.mclustDA <- MclustDA(train.X, train.Y, modelType="EDDA", modelNames="VVV")
summary(iris.mclustDA, parameters=TRUE)
summary(iris.mclustDA, newdata = test.X, newclass = test.Y)

# compared to knn : 

library(class)
knn.pred <- knn(train.X, test.X, train.Y, k=3)
table(knn.pred, test.Y)
1  - mean(knn.pred == test.Y)


# plot iris
plot(iris[,1:4], col=iris$Species)

# obj : predict versicolor (use of modelType in MclustDA)
iris$Versicolor[iris$Species == 'versicolor'] <- 1
iris$Versicolor[iris$Species != 'versicolor'] <- 2

plot(iris[,1:4], col=iris$Versicolor)
 
irisX <- iris[,-c(5,6)]
irisY <- iris[,-(1:5)]
test_idx <- 1:50
train.X <- irisX[-test_idx,]
test.X <- irisX[test_idx,]
train.Y <-irisY[-test_idx]
test.Y <- irisY[test_idx]

iris.mclustDA <- MclustDA(train.X, train.Y, modelType="MclustDA", modelNames="EEE")
summary(iris.mclustDA, parameters=TRUE)
summary(iris.mclustDA, newdata = test.X, newclass = test.Y)



iris.lda <- MclustDA(train.X, train.Y, modelType="EDDA", modelNames="EEE")
summary(iris.lda, parameters=TRUE)
summary(iris.lda, newdata = test.X, newclass = test.Y)
