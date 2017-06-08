## File: casestudy1.R
## Description : MNIST autoencoder case study

rm(list = ls())
## 1. Download, & Load the reduced MNIST dataset from ##########################
#      http://eric.univ-lyon2.fr/~jcugliari/mnist.zip

# setwd("~/Téléchargements") 
this.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this.dir)
library(readr)
library(h2o)
digits.train <- read_csv("mnist_train.zip", col_names = FALSE)
names(digits.train) <- c("label", paste0("p", 1:784))
digits.train$label <- as.factor(digits.train$label)

# 2. Subset the data selecting only lables 0, 3 and 8 ##########################
digits <- subset(digits.train, label == 0 | label == 3 | label == 8)
rm(digits.train)

# 3. Create an h2o connection ##################################################
cl <- h2o.init(max_mem_size = "3G",
               nthreads = 4)

# 4. Parse your dataset to h2o #################################################
h2odigits <- as.h2o(digits,
                    destination_frame = "h2odigits")

# 5. Randomly split h2odigits into a train and a test dataset ##################
smp <- sort(sample(nrow(digits), 12000))
h2odigits.train <- h2odigits[smp, -1]
h2odigits.test  <- h2odigits[-smp, -1]


# 6. Models ####################################################################
xnames <- colnames(h2odigits.train)

# __m1 : 1 hidden layer with 50 units no regularization ####
m1 <- h2o.deeplearning(
  x = xnames,
  training_frame= h2odigits.train,
  validation_frame = h2odigits.test,
  activation = "TanhWithDropout", # "Tanh"
  autoencoder = TRUE,
  hidden = c(50),
  epochs = 10,
  sparsity_beta = 0,
  input_dropout_ratio = 0,
  hidden_dropout_ratios = c(0),
  l1 = 0,
  l2 = 0
)

# __m2a : 1 hidden layer with 100 units no regularization ####
m2a <- h2o.deeplearning(
  x = xnames,
  training_frame= h2odigits.train,
  validation_frame = h2odigits.test,
  activation = "TanhWithDropout",
  autoencoder = TRUE,
  hidden = c(100),
  epochs = 10,
  sparsity_beta = 0,
  input_dropout_ratio = 0,
  hidden_dropout_ratios = c(0),
  l1 = 0,
  l2 = 0
)

# __m2b : 1 hidden layer with 100 units, sparsity coef = 0.5 ####
m2b <- h2o.deeplearning(
  x = xnames,
  training_frame= h2odigits.train,
  validation_frame = h2odigits.test,
  activation = "TanhWithDropout",
  autoencoder = TRUE,
  hidden = c(100),
  epochs = 10,
  sparsity_beta = .5,
  input_dropout_ratio = 0,
  hidden_dropout_ratios = c(0),
  l1 = 0,
  l2 = 0
)

# __m2c : 1 hidden layer with 100 units, input dropout = 0.2 ####
m2c <- h2o.deeplearning(
  x = xnames,
  training_frame= h2odigits.train,
  validation_frame = h2odigits.test,
  activation = "TanhWithDropout",
  autoencoder = TRUE,
  hidden = c(100),
  epochs = 10,
  sparsity_beta = 0,
  input_dropout_ratio = .2,
  hidden_dropout_ratios = c(0),
  l1 = 0,
  l2 = 0
)

train_supervised_feature = h2o.deepfeatures(m2c, h2odigits.train, layer=2)


m1
m2a
m2b
m2c
