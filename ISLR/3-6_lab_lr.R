#------- Margot Selosse -------#
#-------- lab chap 3.6 --------#


#### Libraries ####

library(MASS)
library(ISLR)

#### Simple Linear Regression ####

fix(Boston) # Edit function!
names(Boston)
attach(Boston)
lm.fit <- lm(medv~lstat, data=Boston)

# Use of results : 
summary(lm.fit)
names(lm.fit)
lm.fit$coefficients
coef(lm.fit) # idem that just before
confint(lm.fit) # confidence interval for betas..
predict(lm.fit, data.frame(lstat=c(5,10,15)), interval="confidence")
predict(lm.fit, data.frame(lstat=c(5,10,15)), interval="prediction")

# plotting :
plot(lstat,medv)
abline(lm.fit)
# options:
abline(lm.fit, lwd=3) # lwd command make the line thicker
abline(lm.fit, lwd=3, col="red")
plot(lstat, medv, col="red")
plot(lstat, medv, pch=20)
plot(lstat, medv, pch="+")
plot(1:20,1:20,pch=1:20)
plot(lm.fit) # plot various things
# residuals: 
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

#### Multiple Linear Regression ####

lm.fit <- lm(medv~lstat+age, data=Boston)
summary(lm.fit)
lm.fit <- lm(medv~.,data=Boston)
summary(lm.fit)
summary(lm.fit)$r.squared
# install.packages("car")
library(car)
vif(lm.fit)
lm.fit1 <- lm(medv~.-age, data=Boston)
lm.fit1 <- update(lm.fit, ~.-age)
summary(lm.fit1)

#### Interaction Terms ####

summary(lm(medv~lstat*age, data=Boston))

#### Non-linear transformations of the predictors ####

lm.fit2 <- lm(medv~lstat+I(lstat^2))
summary(lm.fit2)
lm.fit <- lm(medv~lstat)
anova(lm.fit,lm.fit2)
par(mfrow=c(2,2))
plot(lm.fit2)
lm.fit5 <- lm(medv~poly(lstat,5))
summary(lm.fit5)
summary(lm(medv~log(lstat)))

##### Qualitative ####

fix(Carseats)
names(Carseats)
lm.fit <- lm(Sales~. + Income:Advertising + Price:Age, dat=Carseats)
summary(lm.fit)
attach(Carseats)
contrasts(ShelveLoc)

#### Writing Functions ####

loadLibraries <- function(){
  library(ISLR)
  library(MASS)
  print("The libraries have been  loaded.")
}
loadLibraries()
