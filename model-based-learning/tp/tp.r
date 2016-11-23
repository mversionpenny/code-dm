# TP Classifiction : model-based learning #
# ------------ Margot selosse ----------- #
# ------------- 16.11.2016 -------------- #

##### Initialization ####
library(mclust) #for clusterinG and classification
library(leaps) # subset selection
setwd("D:/master-DM/cours/model-based-learning/tp/")

##### Loading data ####
data <- read.table("VisaPremier.txt", header=TRUE) # header=TRUE --> colnames at first line
colnames(data)
summary(data)
ncol(data) # 48


#### First clusters with mclust ####
# ncol = 48, it's a bit much! --> TODO : maybe think about pca or lasso later
model <- model.matrix(~.+0, data=data)
# now, model has 212 dimensions!! --> dummy variables
BIC <- mclustBIC(model) # takes a lot of time
plot(BIC) 
summary(BIC)

#### "Cleaning" data ####

# There are too much dimensions, we want to get rid of some of them
# After a reading of the description, we see that we can manually get rid of 
# cartevp : because it's like cartvpr
# sexe : because it's like sexer
idx_sexe <- which(colnames(data)=="sexe")
idx_cartevp <- which(colnames(data)=="cartevp")
data <- data[,-c(idx_sexe,idx_cartevp)]
model <- model.matrix(~.+0, data=data)
dim(model) # still 210 --> too much

# We note that nbimpaye is always 0
sum(which(data$nbimpaye !=0))
# se can delete this column
idx_nbimpaye <- which(colnames(data)=="nbimpaye")
data <- data[,-c(idx_nbimpaye)]

# We want to see what are the new variables
colnames(model)
# we note that nbpaiecb is generating a lot of dummies variables, but it's a quantitative value!
# What is happpening is that there are missing values "." --> R sees these values as string and 
# so considers that this attribute is qualitative 
sum(data$nbpaiecb==".") # there are 278 indiv. with missing values at nbpaiecb
# same for agemvt
sum(data$agemvt==".") # there are 6 indiv. with missing values at agemvt

# Solutions? 
# 1.replace "." with NA and change columns to numeric
# 2. do 1. and  impute values
library('mice') 
# first, we want to replace "." by NA
nbpaiecb <- replace(data$nbpaiecb,which(data$nbpaiecb=="."),NA)
data$nbpaiecb <- as.numeric(nbpaiecb)
agemvt <- replace(data$agemvt,which(data$agemvt=="."),NA)
data$agemvt <- as.numeric(agemvt)


mice_mod <- mice(data, method = "rf")
mice_output <- complete(mice_mod)

# comparison
idxes <- which(is.na(data$nbpaiecb))
par(mfrow=c(2,2))
hist(data$nbpaiecb, freq=F, main="nbpaiecb:Original Data", col="darkgreen", ylim=c(0,0.12))
hist(mice_output$nbpaiecb, freq = F, main="nbpaiecb:Imputed Data", col="lightgreen", ylim=c(0,0.12))

hist(data$agemvt, freq=F, main="nbpaiecb:Original Data", col="darkblue", ylim=c(0,0.12))
hist(mice_output$agemvt, freq = F, main="nbpaiecb:Imputed Data", col="lightblue", ylim=c(0,0.12))

data <-mice_output
# now we build again the matrix :
model <- model.matrix(~.+0, data=data)
dim(model) # we are at 93, which is better but not enough!

#### Latent class model ####
# see article : latent class in the models, Iwona kasprzyk
#for now, we just get rid f the department feature
idx_departem <- which(colnames(data)=="departem")
data <- data[,-c(idx_departem)]
model <- model.matrix(~.+0, data=data)
dim(model)

#### Subset selection ####
regfit.full <- regsubsets(cartevpr~., data, nvmax=20, method="forward")
reg.summary <- summary(regfit.full)
plot(reg.summary$rss, xlab="Number of Variables", ylab="RSS", type="l")


train <- sample(c(TRUE, FALSE), nrow(data), rep=TRUE)
test <- (!train)

regfit.best <- regsubsets(cartevpr~., data=data[train,], nvmax=20, method="forward")

# building a model matrix from the test data /!\ remmember model.matrix!
test.mat <- model.matrix(cartevpr~., data=data[test,])

val.errors <- rep(NA,19)
for(i in 1:19){
  coefi <- coef(regfit.best, i)
  # %*% = matricial product!!
  pred <- test.mat[,names(coefi)] %*% coefi
  val.errors[i] <- mean((data$cartevpr[test] - pred)^2)
}
val.errors
idx <- which.min(val.errors) # answer = 10
coef(regfit.best,10)



#### Applying PCA ####
# problem, some of features are not numeric: sitfamil, csp, codeqlt
# we change data 
idx_cartevpr <- which(colnames(data)=="cartevpr")
log.data <- log(data[,-c(idx_cartevpr)])
data.cartevpr <- data[, idx_cartevpr]

# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
ir.pca <- prcomp(log.ir,
                 center = TRUE,
                 scale. = TRUE)  


