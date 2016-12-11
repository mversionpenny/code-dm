# TP Clustering : model-based learning #
# ------------ Margot selosse ----------- #
# ------------- 03.12.2016 -------------- #

#1. install packages and load data ####
# install.packages("funFEM")
library(funFEM)
data("velib")
summary(velib)
attach(velib)
library(ggplot2)
library('ggthemes') # visualization

#2. plot data ####


# changing data to matrix
data.matrix <- as.matrix(data)
data.matrix <- matrix(data.matrix, ncol = ncol(data), dimnames = NULL)
x  <- seq(1:181)
station1 <- data.matrix[1,]
station2 <- data.matrix[2,]
station3 <- data.matrix[3,]
df <- data.frame(x,station1,station2,station3)
# plot with several lines : 
g <- ggplot(df, aes(x))  +                  # basic graphical object
 geom_line(aes(y=station1), colour="red") + # first layer
 geom_line(aes(y=station2), colour="green") +
 geom_line(aes(y=station3), colour="blue") 
 #theme(axis.text.x = element_text(angle = 90))
g

# plots with one line : 
par(mfrow=c(2,2))
dates
plot(x, data.matrix[5,], type="o")
plot(x, data.matrix[6,], type="o")
plot(x, data.matrix[7,], type="o")
plot(x, data.matrix[8,], type="o")
dates[150]
which.max(data.matrix[5,120:160])

# --> we see waves

#3. pca ####
data.pca <- prcomp(data.matrix,center = TRUE, scale = TRUE) 
# plot method
plot(data.pca, type = "l")
# the elbow i on 3 dimensions 
data.important <- data.pca$x[,1:5]
x.important <- 1:5
plot(x.important, data.important[9,], type="o")
plot(x.important, data.important[10,], type="o")
plot(x.important, data.important[11,], type="o")
plot(x.important, data.important[12,], type="o")

#4. use of funFEM algortihm ####

basis<- create.fourier.basis(c(0, 181), nbasis=25)
fdobj <- smooth.basis(1:181,t(velib$data),basis)$fd
res <- funFEM(fdobj, K=6, disp=TRUE,init='kmeans')

fdmeans = fdobj; fdmeans$coefs = t(res$prms$my)
plot(fdmeans,col=1:res$K,xaxt='n',lwd=2)
axis(1,at=seq(5,181,6),labels=velib$dates[seq(5,181,6)],las=2)

#5. Spatial visualization of the clustering (with library ggmap)
library(ggmap)
Mymap = get_map(location = 'Paris', zoom = 12, maptype = 'terrain')
ggmap(Mymap) + geom_point(data=velib$position,aes(longitude,latitude), colour = I(res$cl), size = I(3))


#6. Other approach : 
