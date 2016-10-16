# Manifold Learning TP2. Margot Selosse

# Ex1. Détection automatique de chiffres ####

#1.download data

#2.load data into R
label <- read.table("D:\\master-DM\\cours\\manifold learning\\cours2\\minimnist\\labels.txt")
data <- read.table("D:\\master-DM\\cours\\manifold learning\\cours2\\minimnist\\data.txt")

#3.
rowToImage<- function(rowVec){
  cubeSize <- sqrt(length(rowVec))
  mat <- matrix(as.numeric(rowVec), nrow=cubeSize,ncol = cubeSize)[,cubeSize:1]
  print(mat)
  image(mat,col=grey.colors(100))
}

#prof
showDigits <-function(line){
    p<-sqrt(length(line))
    mat<-matrix(as.numeric(line),p)[,p:1]
    print(mat)
    image(mat, col=grey(256:1/256))
}


#4.
indices3 <- which(label == 3, arr.ind=TRUE)
images3 <-data[indices3,]



#Ex2 ####
constructData <- function(nrPoints,nrDim){
  x <-  matrix(runif(nrPoints*nrDim,-1,1),ncol=nrDim)
  x
}

#dim>=2!!
normComputing <- function(x){
  result <-c()
  for (i in 1:dim(x)[1]) {
    result <- c(result, sqrt(sum(x[i,]^2)))
  }
  return(result)
}

#prof
#norm <- sqrt(rowSums(x^2))


#plot 
k<-1000
layout(matrix(1:6, 2, byrow = TRUE))
#par(mfrow=c(2,2))
for (d in c(2, 5, 20, 100, 1000)) {
  simulation <- constructData(k, d)
  simulationNorm <- normComputing(simulation)
  plot(density(simulationNorm), main = paste("Dimension =", d), xlim=c(0,20))
  print(sprintf("Dim:%3.0f  Mean:%.2f  Var:%.2f  CV:%.2f", 
                d, mean(simulationNorm), var(simulationNorm), sd(simulationNorm)/mean(simulationNorm) ))
}


#better code for that :
#fix scale (0:20, to see that mean is increased)
#titles to plots
#print table with values

