# model-based learning. tp1. Margot Selosse

data(iris)
plot(iris[,1:4],col=iris$Species)

#### kmeans

clusters <- kmeans(iris[,1:4],3)$cluster
new_iris <- cbind(iris[,1:4],clusters)
plot(new_iris[,1:4], col=new_iris[,5])

#tableau de contingence
table(iris$Species,clusters)


#### cah
par(mfrow=c(1,1))
result_cah <- hclust(d=dist(iris[,1:4]),method="ward.D2")
plot(result_cah,hang=-1)
cluster_cah <-cutree(result_cah, k=3)

#tableau de contingence avec kmeans :
table(clusters,cluster_cah)

#tableau de contingence avec les vraies classes
table(iris$Species, cluster_cah)