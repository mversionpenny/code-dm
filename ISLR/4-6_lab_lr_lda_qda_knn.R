#------- Margot Selosse -------#
#-------- lab chap 4.6 --------#

install.packages("ISLR")
library(ISLR)

#### Stock Market Data ####

names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
cor(Smarket[,-9])
attach(Smarket)
plot(Volume)

#### Logistic Regression ####
# setting lr :
glm.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Volume, data=Smarket, family=binomial)
summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef
summary(glm.fit)$coef[,4]
contrasts(Direction)
# have a first guess :
glm.probs <- predict(glm.fit, type="response")
glm.probs[1:10]
glm.pred <- rep("Down",1250)
glm.pred[glm.probs>0.5] <- "Up"
glm.pred
# table of confuson (true negative, true positive..)
table(glm.pred, Direction)
# mean of rigtht results : 
mean(glm.pred==Direction)
# BUT! we've done that with only the training set, now we're going to use a cv 
train <- (Year<2005)
Smarket.2005 <- Smarket[!train,]
dim(Smarket.2005)
Direction.2005 <- Direction[!train]
Direction.2005
glm.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family = binomial, subset=train)
glm.probs <- predict(glm.fit, Smarket.2005, type="response")
glm.pred <- rep("Down",length(Direction.2005))
glm.pred[glm.probs>0.5] <- "Up"
table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005)
# mean is low ~0.48 --> worse than random prediction!
# here we use only lag1 and lag2:
glm.fit <- glm(Direction~Lag1+Lag2, data=Smarket,family=binomial, subset = train)
glm.probs <- predict(glm.fit, Smarket.2005, type="response")
glm.pred <- rep("Down",length(Direction.2005))
glm.pred[glm.probs>0.5] <- "Up"
table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005)
# to predict with (Lag1,Lag2) = (1.2,1.1) and (1.5,-0.8)
predict(glm.fit, newdata = data.frame(Lag1=c(1.2,1.5), Lag2=c(1.1,-0.8)), type="response")


#### Linear Discriminant Analysis ####

library(MASS)
lda.fit <- lda(Direction~Lag1+Lag2, data=Smarket,subset=train)
lda.fit
plot(lda.fit)
lda.pred <- predict(lda.fit,Smarket.2005)
names(lda.pred)
lda.class <- lda.pred$class
table(lda.class,Direction.2005)
mean(lda.class==Direction.2005)
#dim(lda.pred$posterior)
#lda.pred$posterior[1:10,]
lda.pred$posterior[1:10,]
lda.pred$class[1:20]

#### Quadratic Discriminant Analysis ####

qda.fit <- qda(Direction~Lag1+Lag2, data=Smarket, subset=train)
qda.fit
qda.class <- predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class==Direction.2005)

#### K-Nearest ####

library(class)
train.X <- cbind(Lag1, Lag2)[train,]
train.test <- cbind(Lag1, Lag2)[!train,]
train.Direction <- Direction[train] 
# reproducibility of results
set.seed(1)
# k = 1:
knn.pred <- knn(train.X, test.X, train.Direction, k=1)
table(knn.pred,Direction.2005)
mean(knn.pred == Direction.2005)
# k = 2:
knn.pred <- knn(train.X, test.X, train.Direction, k=3)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)


#### An application to Caravan Insurance Data ####

dim(Caravan)
attach(Caravan)
summary(Caravan)
colnames(Caravan)
summary(Purchase)
standardized.X <- scale(Caravan[,-86])
test <- 1:1000
test.X <- standardized.X[test,]
train.X <- standardized.X[-test,]
test.Y <- Purchase[test]
train.Y <- Purchase[-test]

# try with knn
# k = 1
knn.pred <- knn(train.X, test.X, train.Y, k=1)
table(knn.pred, test.Y)
mean(knn.pred == test.Y)
mean(test.Y!=knn.pred)
mean(test.Y!="No")
# k = 3
knn.pred <- knn(train.X, test.X, train.Y, k=3)
mean(test.Y != knn.pred)
# k = 5 
knn.pred <- knn(train.X, test.X, train.Y, k=5)
mean(test.Y != knn.pred)
table(knn.pred, test.Y)

# try with lr
glm.fit <- glm(Purchase~.,data=Caravan, family=binomial, subset=-test)
glm.probs <- predict(glm.fit, Caravan[test,],type="response")
glm.pred <- rep("No", length(test))
glm.pred[glm.probs>0.5] = "Yes"
table(glm.pred, test.Y)
mean(glm.pred == test.Y)

# Not in book lab : #
#### mclustDA ####
#install.packages('mclust')
library(mclust)
purchase.mclustDA <- MclustDA(train.X, train.Y, modelType = "EDDA", modelNames = "EEE")
summary(purchase.mclustDA, parameters=TRUE)
summary(purchase.mclustDA, newdata = test.X, newclass = test.Y)

#### rmixmod ####
# install.packages('Rmixmod')
library(Rmixmod)

