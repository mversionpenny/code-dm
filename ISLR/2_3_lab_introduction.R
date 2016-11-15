#------- Margot Selosse -------#
#-------- lab chap 2.3 --------#

#### Basic commands ####

x <- c(1,2,3,4)
y <- c(2,3,8,7)
length(x)
length(y)
# remove objects  : 
rm(list=ls())
# create matrice :
x <- matrix(c(1, 2, 3, 4), 2, 2, byrow = TRUE)

# simulate normal data :
x <- rnorm(50)
y <- x + rnorm(50,mean=50, sd=.1)
cor(x,y)

#set.seed reproduces same results for random generation
set.seed(3)
y <- rnorm(100)
mean(y)
var(y)


#### Graphics ####

x <- rnorm(100)
y <- rnorm(100)
plot(x,y)
plot(x,y, xlab="this is the x axis", ylab="This is the y axis", main="Plot of X vs Y")
pdf("Figure.pdf")
plot(x,y, col="green")
dev.off()

# sequences :
x <- seq(1,10)
x <- seq(-pi, pi, length=50)

# 3d dataets representations
y <- x
f = outer(x,y, function(x,y)cos(y)/(1+x^2))
contour(x,y,f)
contour(x,y,f,nlevels=45,add=T)
fa <- (f-t(f))/2
contour(x,y,fa, nlevels = 15)
# heat map : 
image(x,y,f)
# 3d
persp(x,y,f, col="green")

#### Indexing data ####

A <- matrix(1:16,4,4)
A[2,3]
A[c(1,3), c(2,4)]
A[1:3,2:4]
A[1:2,]
A[,1:2]
A[-c(1,3),]

#### Loading Data ####

Auto <- read.table("Auto.data")
