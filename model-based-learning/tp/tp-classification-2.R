# TP classification 2 : model-based learning #
# ------------- Margot selosse ------------ #
# -------------- 08.12.2016 --------------- #

#### Loading data ####
# Load the MNIST digit recognition dataset into R
# http://yann.lecun.com/exdb/mnist/
# assume you have all 4 files and gunzip'd them
# creates train$n, train$x, train$y  and test$n, test$x, test$y
# e.g. train$x is a 60000 x 784 matrix, each row is one digit (28x28)
# call:  show_digit(train$x[5,])   to see a digit.
# brendan o'connor - gist.github.com/39760 - anyall.org

load_mnist <- function() {
  load_image_file <- function(filename) {
    ret = list()
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    ret$n = readBin(f,'integer',n=1,size=4,endian='big')
    nrow = readBin(f,'integer',n=1,size=4,endian='big')
    ncol = readBin(f,'integer',n=1,size=4,endian='big')
    x = readBin(f,'integer',n=ret$n*nrow*ncol,size=1,signed=F)
    ret$x = matrix(x, ncol=nrow*ncol, byrow=T)
    close(f)
    ret
  }
  load_label_file <- function(filename) {
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    n = readBin(f,'integer',n=1,size=4,endian='big')
    y = readBin(f,'integer',n=n,size=1,signed=F)
    close(f)
    y
  }
  train <<- load_image_file('train-images-idx3-ubyte')
  test <<- load_image_file('t10k-images-idx3-ubyte')
  
  train$y <<- load_label_file('train-labels-idx1-ubyte')
  test$y <<- load_label_file('t10k-labels-idx1-ubyte')  
}


show_digit <- function(x, col=gray(12:1/12), title='image') {
  image(matrix(x, nrow=28)[,28:1], col=col, main=title)
}


# chargeons les donnees
# attention, les fichiers ubyte doivent être dans le même dossier que ce fichier
# et il faut dire a R de se placer dans le repertoire courant 
# session -> set working directory -> to source file location 
setwd("D:\\master-DM\\cours\\model-based-learning\\tp")
load_mnist()

# le fichier train contient une image vectorialisé sur chaque ligne de train$x, 
# le chiffre apparaissant dans cette image etant contenu dans train$y

# affichage de l'image
numero=2
show_digit(train$x[numero,],title=train$y[numero])

#### Gaussian mixture model ####
# testons avec le BIC pour savoir ce qui définit mieux nos covarianes :

library(mclust)
# begin with just a few (because it's very long)
mclustda <- MclustDA(train$x[1:1000,], train$y[1:1000], modelType="EDDA", modelNames="VII")
# mclustda <- MclustDA(train$x, train$y, modelType="EDDA", modelNames="EEE")
library(HDclassif)
prms <- hdda(train$x[1:10000,], train$y[1:10000], model="all")
resTrain <- predict(prms, train$x[1:10000,], train$y[1:10000])
resTest <- predict(prms, test$x[1:1000,], test$y[1:1000])

#### tests coclustering ####
library(blockcluster)
data(binarydata)
out<-coclusterBinary(binarydata,nbcocluster=c(2,3))
summary(out)
plot(out)
