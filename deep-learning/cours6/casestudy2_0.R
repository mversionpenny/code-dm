## File: casestudy2.R
## Description : Predictive case study on MNIST 

#   Nous allons comparer la performance de plusieurs classifiers automatiques 
# sur les donnees reduits du MNIST (composé uniquement des chiffres 0, 3 et 8).
# Nous separons le jeu de données en deux. une partie apprentissage et une 
# partie validation

# data(iris)
# 
# library(h2o)
# cl <- h2o.init(max_mem_size = "4G", nthreads = 4)
# 
# smp <- sample(nrow(iris), floor(nrow(iris) * .8))
# iris.train <- iris[ smp, ]
# iris.test  <- iris[-smp, ]
# 
# h2o.iris.train <- as.h2o(iris.train, "h2oiristrain")
# h2o.iris.test  <- as.h2o(iris.test,  "h2oiristest")  
# 
# 
# system.time(ex1 <- h2o.deeplearning(
#   x = colnames(iris)[-5],
#   y = "Species",
#   training_frame= h2o.iris.train,
#   validation_frame = h2o.iris.test,
#   activation = "RectifierWithDropout",
#   hidden = c(6),
#   epochs = 10,
#   adaptive_rate = FALSE,
#   rate = .001,
#   input_dropout_ratio = 0,
#   hidden_dropout_ratios = c(0)
# ))
#
#ex1

rm(list = ls())
this.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this.dir)
library(readr)
library(h2o)
digits.train <- read_csv("mnist_train.zip", col_names = FALSE)
names(digits.train) <- c("label", paste0("p", 1:784))
digits.train$label <- as.factor(digits.train$label)

cl <- h2o.init(max_mem_size = "20G",
               nthreads = 4)

digits <- subset(digits.train, label == 0 | label == 3 | label == 8)
rm(digits.train)

h2odigits <- as.h2o(digits,
                    destination_frame = "h2odigits")

smp <- sort(sample(nrow(digits), 12000))
h2odigits.train <- h2odigits[smp,]
h2odigits.test  <- h2odigits[-smp,]



xnames <- colnames(h2odigits.train[,-1])
# 1. Deep Network ####

# A partir du code utilisé dans le dernier cours, entrainez un réseau de 
# neurones pour prévoir la variable digit à partir des piixels. chronometrez le 
# temps d apprentissage.
system.time(ex1 <- h2o.deeplearning(
  x = xnames,
  y = "label",
  training_frame= h2odigits.train,
  validation_frame = h2odigits.test,
  activation = "RectifierWithDropout",
  hidden = c(6),
  epochs = 10,
  adaptive_rate = FALSE,
  rate = .01,
  input_dropout_ratio = 0,
  hidden_dropout_ratios = c(0)
))
ex1
# 2. Svm ####
library(caret)
digits.train <- read_csv("mnist_train.zip", col_names = FALSE)
digits <- subset(digits.train, label == 0 | label == 3 | label == 8)
names(digits) <- c("label", paste0("", 1:784))
digits$label <- as.factor(digits$label)
digits.train <- digits[smp,]
digits.test <- digits[-smp,]

library(doParallel)
cl <- makeCluster(4)
# IMPORTANT!
registerDoParallel(cl)
#library(doMC)
#registerDoMC(2)
system.time(
  fitsvm <- train(label ~ ., data = digits.train, method = "svmRadial", 
                  trControl=
                    trainControl(method="cv",number=5, allowParallel = T))
)
stopCluster(cl)

hatsvm <- predict(fitsvm, newdata = digits.test)
confusionMatrix(hatsvm, digits.test$label)

# 3. Random Forest ####
system.time(
  fitrf<- train(label ~ ., data = digits.train, method = "rf", 
                  trControl=
                    trainControl(method="cv",number=5, allowParallel = T))
)
# 4. Comment mesurer l importance des variables dans chaque cas? ####
# pour le random forest --> variables qui changent le plus l'entropy 
# pour les autres --> c'est compliqué



