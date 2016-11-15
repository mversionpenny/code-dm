#------- Margot Selosse -------#
#-------- lab chap 6.5 --------#

#### Best subset selection ####

# loading, observing data  : 
library(ISLR)
fix(Hitters)
names(Hitters)
dim(Hitters)
# is.na allows us to know if a variable is null
sum(is.na(Hitters$Salary))
# na.omit get rid of rows which contain a null variable
Hitters <- na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))

# subset selection :
install.packages('leaps')
library(leaps)
regfit.full <- regsubsets(Salary~., Hitters)
summary(regfit.full)
# regsubsets usually gives results for 8 best subsets, but we can change that with nvmax
regfit.full <- regsubsets(Salary~., Hitters, nvmax=19)
reg.summary <- summary(regfit.full)
# summary of regsubsets gives us the rss, R^2, BIC..etc or each subestsubset selection
names(reg.summary)
reg.summary$rsq

# plot adjusted R^2 ad RSS:
par(mfrow=c(2,2))
plot(reg.summary$rss, xlab="Number of Variables", ylab="RSS", type="l")
plot(reg.summary$adjr2, xlab="Number of Variables", ylab="Adjusted Rsq",  type="l")

# points() cmd = puts points on a plot that has already been created
index <- which.max(reg.summary$adjr2)
points(index, reg.summary$adjr2[index],col="red", cex=2, pch=20)

# in the book, there are also plots for Cp and BIC (p.246)
# regsubsets has built-in plot --> ?plot.regsubsets
plot(regfit.full, scale="r2")
plot(regfit.full, scale="adjr2")
plot(regfit.full, scale="Cp")
plot(regfit.full, scale="bic")

# get the coefficients estimates for an associated model (subset selection)
coef(regfit.full, 6)

#### Forward and Backward Stepwise selection ####

# use of backward and forward methods : 
regfit.fwd <- regsubsets(Salary~., Hitters, method = "forward")
summary(regfit.fwd)
regfit.bwd <- regsubsets(Salary~., Hitters, method="backward")
summary(regfit.bwd)


#### Choosing among models using the Validation Set Approach and Cross-Validation ####

# VALIDATION SET APPROACH

# reprodubility of simulation :
set.seed(1)

train <- sample(c(TRUE, FALSE), nrow(Hitters), rep=TRUE)
test <- (!train)

regfit.best <- regsubsets(Salary~., data=Hitters[train,], nvmax=19)

# building a model matrix from the test data /!\ remmember model.matrix!
test.mat <- model.matrix(Salary~., data=Hitters[test,])

val.errors <- rep(NA,19)
for(i in 1:19){
  coefi <- coef(regfit.best, i)
  # %*% = matricial product!!
  pred <- test.mat[,names(coefi)] %*% coefi
  val.errors[i] <- mean((Hitters$Salary[test] - pred)^2)
}
val.errors
which.min(val.errors) # answer = 10
coef(regfit.best,10)

# there is no function "predict" for regsubsets, so we build one
predict.regsubsets <- function(object, newdata, id, ...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id=id)
  xvars <- names(coefi)
  mat[,names(coefi)] %*% coefi
}

# we learnt that we need 10 variables to describe well the data
# know, we are going to use the full dataset to know which variables we are going to use
# We perform best subset selection on the full data set and select the best ten-variable model, 
# rather than simply using the variables that were obtained from the training set because the 
# best ten-variable model on the full data set may vary from the training set.

regfit.best <- regsubsets(Salary~.,data=Hitters, nvmax=19)
coef(regfit.best, 10)

# Now, we are going to use CROSS VALIDATION

k <- 10
set.seed(1)
folds <- sample(1:k,nrow(Hitters),replace=TRUE)
cv.errors <- matrix(NA,k,19, dimnames=list(NULL,paste(1:19)))
for(j in 1:k){
  best.fit <- regsubsets(Salary~., data=Hitters[folds!=j,], nvmax=19)
  for(i in 1:19){
    pred <- predict(best.fit,Hitters[folds==j,], id=i)
    cv.errors[j,i] <- mean((Hitters$Salary[folds==j]-pred)^2) 
  }
}
mean.cv.errors <- apply(cv.errors,2,mean)
mean.cv.errors
which.min(mean.cv.errors)
par(mfrow=c(1,1))
plot(mean.cv.errors, type = "b")
reg.best <- regsubsets(Salary~., data=Hitters, nvmax=19)
coef(reg.best, 11)
