#------- Margot Selosse -------#
#-------- lab chap 6.6 --------#

# install.packages("glmnet")
library(ISLR)
dim(Hitters)
Hitters <- na.omit(Hitters)

x <- model.matrix(Salary~., Hitters)[,-1]
y <- Hitters$Salary

#### Ridge Expression ####
library(glmnet)
# different values for lambda :
grid <- 10^seq(10,-2,length=100)
# alpha determines the used method (1 --> lasso, 0 --> ridge)
ridge.mod <-  glmnet(x,y, alpha=0, lambda=grid)
# playing with glmnet result
dim(coef(ridge.mod))
summary(ridge.mod)
ridge.mod$lambda[50]
coef(ridge.mod)[,50]

# use of predict function:
predict(ridge.mod, s=50, type="coefficients")[1:20,]

# separate training and test sets
# approach : randomly choose a subset of numbers between 1 and n

set.seed(1)
train <- sample(1:nrow(x),nrow(x)/2)
test <- (-train)
y.test <- y[test]

# training the model:
ridge.mod <- glmnet(x[train,], y[train], alpha = 0, lambda = grid, thresh=1e-12)
ridge.pred <- predict(ridge.mod, s=4, newx=x[test,])
mean((ridge.pred-y.test)^2)

# is lambda = 4 better than lambda = 0 (no regularization) ?
# exact = T argument = have the exact least suqare (else, predictdoes things with the grid given in glmnet)
ridge.pred <- predict(ridge.mod, s=0, newx=x[test,], exact=T)
mean((ridge.pred-y.test)^2)
# best way to have the least square result : with lm!
lm(y~x, subset=train)
predict(ridge.mod, s=0, exact=T, type="coefficients")

# We chose lambda = 4 arbitrarily, we shhould choose ot with cross validation
# built-in function for glmnet
set.seed(1)
cv.out <- cv.glmnet(x[train,],y[train], alpha=0)
plot(cv.out)
bestlam <- cv.out$lambda.min 

ridge.pred <- predict(ridge.mod, s=bestlam, newx = x[test,])
mean((ridge.pred-y.test)^2)
out <- glmnet(x,y,alpha=0)
predict(out, type="coefficients",s=bestlam)

#### The Lasso ####

lasso.mod <-  glmnet(x[train,], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)

set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train],alpha=1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s=bestlam, newx=x[test,])

# training for everyone : 
out <- glmnet(x,y, alpha=1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients", s=bestlam)
lasso.coef
