# installer le package, charger le package et les donnees 
#install.packages('funFEM')
library(funFEM)
data(velib)
# voir ce que contient ce fichier
help(velib)
print(str(velib))

# afficher differentes courbes de charge, interpreter les donnees, rechercher des comportements typiques 
station=4
plot(1:181,velib$data[station,],type='l',main=velib$names[station],
        ylab="proportion de Velib disponibles",,xaxt='n',lwd=2,ylim=c(0,1))
axis(1,at=seq(5,181,6),labels=velib$dates[seq(5,181,6)],las=2)

# === representation des data sur le premier axe de l'ACP ===
# essayer de mettre en évidence les phénomènes observés par une ACP
d=c(23+24*0:6,33+24*0:6)
#
d=1:181
acp <- princomp(velib$data[,d],cor = T, scores = T)
plot(acp)

#**** corrélation variables-facteurs ****
c1 <- acp$loadings[,1]*acp$sdev[1]
c2 <- acp$loadings[,2]*acp$sdev[2]
par(mfrow=c(1,1))
#*** cercle des corrélations ***
plot(c1,c2,xlim=c(-1,+1),ylim=c(-1,+1),type="n")
abline(h=0,v=0)
text(c1,c2,labels=velib$dates[d],cex=1,font=1.3)
symbols(0,0,circles=1,inches=F,add=T)
#*** representation des individus ***
plot(acp$scores[,1],acp$scores[,2],type="n",xlab="Comp.1",ylab="Comp.2")
abline(h=0,v=0) 
text(acp$scores[,1],acp$scores[,2],labels=velib$names,cex=0.5)


# ==== modele de melanges ======
library(mclust)
# testons le nombre de cluster a l'aide du critere BIC
M=Mclust(velib$data,G=1:10)
plot(M)
# le critere BIC ne semble pas tres bien fonctionner ici
# choisissons 6 classes comme precedemment
M=Mclust(velib$data,G=6)
# examinons quel modele a ete choisi
summary(M)
# examinons la partition obtenue
membmclust=M$classification
moyenne=matrix(0,nrow=K,ncol=ncol(velib$data))
for (k in 1:K) moyenne[k,]=colMeans(velib$data[membmclust==k,])
plot(1:181,moyenne[1,],type='l',ylab="proportion de Velib disponibles",xaxt='n',lwd=2,ylim=c(0,1))
axis(1,at=seq(5,181,6),labels=velib$dates[seq(5,181,6)],las=2)
for (k in 2:K) lines(1:181,moyenne[k,],col=k,font=4)
legend("topright",c('1','2','3','4','5','6'), pch = '-',col=1:6,title='cluster')


# bonus : représenter spatiallement les résultats du clustering
library(ggmap)
qmap(location = 'Paris', zoom = 12, maptype = 'toner')+ geom_point(data=velib$position,aes(longitude,latitude), colour = velib$bonus+1, size = I(3))





