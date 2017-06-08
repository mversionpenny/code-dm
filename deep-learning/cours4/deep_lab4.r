## File : deep_lab4.r
## Description : Lab 4 course Deep Learning
## Date : dec 2016 by jc


## #############################################################################
##                      Ex. 1                                               ####
## #############################################################################
rm(list=ls())
this.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this.dir)
# install.packages("autoencoder")
library(autoencoder)

## Train the autoencoder on unlabeled set of 5000 image patches of 
## size Nx.patch by Ny.patch, randomly cropped from 10 nature photos:
## Load a training matrix with rows corresponding to training examples, 
## and columns corresponding to input channels (e.g., pixels in images):
data('training_matrix_N=5e3_Ninput=100')  ## the matrix contains 5e3 image 
                                          ## patches of 10 by 10 pixels

## Set up the autoencoder architecture:
nl = 3                         ## number of layers (default is 3: input, hidden, output)
unit.type = "tanh"             ## specify the network unit type, i.e., the unit's 
                               ## activation function ("logistic" or "tanh")
Nx.patch <- 10                 ## width of training image patches, in pixels
Ny.patch <-10                  ## height of training image patches, in pixels
N.input <- Nx.patch * Ny.patch ## number of units (neurons) in the input layer (one unit per pixel)
N.hidden <- 5 * 5              ## number of units in the hidden layer
lambda <- 0.0002               ## weight decay parameter     
beta <- 6                      ## weight of sparsity penalty term 
rho <- 0.01                    ## desired sparsity parameter
epsilon <- 0.001               ## a small parameter for initialization of weights 
                               ## as small gaussian random numbers sampled from N(0,epsilon^2)
max.iterations = 2000          ## number of iterations in optimizer

## Train the autoencoder on training.matrix using BFGS optimization method 
## (see help('optim') for details):
## WARNING: the training can take as long as 20 minutes for this dataset!

## Not run: 
system.time(
autoencoder.object <- autoencode(X.train = training.matrix,
                                 nl = nl,
                                 N.hidden = N.hidden,
                                 unit.type = unit.type,
                                 lambda = lambda,
                                 beta = beta,
                                 rho = rho,
                                 epsilon = epsilon,
           optim.method="BFGS",max.iterations=max.iterations,
           rescale.flag=TRUE,rescaling.offset=0.001)
)
##D           
## End(Not run)
## N.B.: Training this autoencoder takes a long time, so in this example we do 
## not run the above autoencode function, but instead load the corresponding 
## pre-trained autoencoder.object.
## Don't show: 
data('autoencoder_Ninput=100_Nhidden=25_rho=1e-2')
## End(Don't show)

## Report mean squared error for training and test sets:
cat("autoencode(): mean squared error for training set: ",
     round(autoencoder.object$mean.error.training.set,3),"\n")

## Visualize hidden units' learned features:
visualize.hidden.units(autoencoder.object, Nx.patch, Ny.patch)



## #############################################################################
##                      Ex. 2                                               ####
## #############################################################################

# From: http://h2o-release.s3.amazonaws.com/h2o/rel-tibshirani/8/index.html#R

# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload = TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download packages that H2O depends on.
if (! ("methods" %in% rownames(installed.packages()))) { install.packages("methods") }
if (! ("statmod" %in% rownames(installed.packages()))) { install.packages("statmod") }
if (! ("stats" %in% rownames(installed.packages()))) { install.packages("stats") }
if (! ("graphics" %in% rownames(installed.packages()))) { install.packages("graphics") }
if (! ("RCurl" %in% rownames(installed.packages()))) { install.packages("RCurl") }
if (! ("jsonlite" %in% rownames(installed.packages()))) { install.packages("jsonlite") }
if (! ("tools" %in% rownames(installed.packages()))) { install.packages("tools") }
if (! ("utils" %in% rownames(installed.packages()))) { install.packages("utils") }

# Now we download, install and initialize the H2O package for R.
install.packages("h2o", type="source", 
                 repos=(c("http://h2o-release.s3.amazonaws.com/h2o/rel-tibshirani/8/R")))


library(h2o)
localH2O <- h2o.init(nthreads = -1)

fit <- h2o.deeplearning(x = 1:100,
                        training_frame = as.h2o(training.matrix),
                        hidden = c(400, 200, 2, 200, 400 ) ,
                        epochs = 600,
                        activation = 'Tanh',
                        autoencoder = TRUE)



## #############################################################################
##                  Ex. 3 Real data example                                 ####
## #############################################################################


MNIST_DIGITStrain <- read.table("minimnist/data.txt", header = FALSE)
MNIST_DIGITSlabel <- read.table("minimnist/labels.txt", header = FALSE)$V1
# library(readr)  ## much faster !!!!
# MNIST_DIGITStrain <- read_delim('data.txt', " ", col_names = FALSE,)

par( mfrow = c(10,10), mai = c(0,0,0,0))
for(i in 1:100){
  y = as.matrix(MNIST_DIGITStrain[i, ])
  dim(y) = c(28, 28)
  image( y[,nrow(y):1], axes = FALSE, col = gray(255:0 / 255))
  text( 0.2, 0, MNIST_DIGITSlabel[i], cex = 3, col = 2, pos = c(3,4))
}


library(h2o)
h2o.init(nthreads = 8)
MDIG <- h2o.importFile(path = "D:\\master-DM\\cours\\deep-learning\\cours4\\minimnist\\data.txt", sep = ' ')

# Show the data objects on the H2O platform
h2o.ls()

NN_model = h2o.deeplearning(
  x = 1:784,
  training_frame = MDIG,
  hidden = c(400, 200, 2, 200, 400 ),
  epochs = 600,
  activation = 'Tanh',
  autoencoder = TRUE
)

