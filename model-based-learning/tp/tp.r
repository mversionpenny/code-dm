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
# now, model has 212 dimensions!!
BIC <- mclustBIC(model) # takes a lot of time
plot(BIC) 
summary(BIC)

# There are too much dimensions, we want to get rid of some of them
# After a reading of the description, we see that we can manually get rid of 
# cartevp : because it's like cartvpr


#### Subset selection ####
regfit.full <- regsubsets(cartevpr~., data, nvmax=20, really.big=T)
summary(regfit.full)
