
###########    LAB_1 parallel computing for DS ########

### exo2 optimisation ##############

### 1) fonction
funz<-function(x)
{
  a<-sin(x)^2
  b=abs(x-3)
  c=sqrt(b)
  w<- a+c
  return (w)
}
funz

## 2)courbe
curve(funz(x), from=-6, to=4, col="red",lwd=2, main="curbe fonction f(x)")
grid(lty=3)

## 3)integrale 
integrate(funz,lower=-6, upper=4)

### 4) une valeur du min
help(optimise)
opMin=optimise(funz, interval=c(-6,4))

### 5) une valeur du max 
opMax=optimise(funz, interval=c(-6,4),maximum =TRUE)

###### Remarque pourquoi les choses ne se comportement pas comme prevue: 
## je trace la ligne du min et max
abline(h=opMin$objective,col="green")
abline(h=opMax$objective, col="yellow")

### R se trompe sur le max et le min de la fonction (voir le graphe) 
##car la function n'est pas derivable
## au point x = 3, en plus il fait des erreurs de calcul sur l'integrale


########## exo 3 ###### 

## 1) fonction simData je simule y--> Uniforme(5,10) de taille 100
n<-100
SimData<-runif(n,5, 10)
SimData

#### données simulées sont contenue dans le vecteur donc je peux faire
### quelque stat.

y=SimData
summary(y)
hist(y, col="yellow", prob=TRUE)
help("plot")
grid(lty=2)


#### la fonction perte
pertes<- function(s,y,p)
{
 a= abs(s-y)
 b= sum(a^p)^(1/p)
return(b)
}

#  Min.   1st Qu.  Median   Mean   3rd Qu.   Max. 
# 5.146   6.633    7.917   7.812   9.038   10.000 

minY=5.146
maxY=10
help(optimize)
#### case p=1 nous avons la distance de manhattan qui
## coincide avec la median de y
p1=1

distance_MAN=optimize(pertes,interval=c(minY, maxY), y=SimData, p=p1)
## cest ok car minimun de la function optimize = 7.916539 coincide avec la mediane de y
distance_MAN$minimum
median(SimData)

#case p=2 distance euclidienne
p2=2
distance_EUCL=optimize(pertes,interval=c(minY, maxY), y=SimData, p=p2)

###cest ok car minimun de la function optimize =7.812208   coincide avec la moyenne de y
distance_EUCL$minimum
mean(y)

#case p=5 
p3=5
distance_A=optimize(pertes,interval=c(minY, maxY), y=SimData, p=p3)
distance_A


#case p=5 
p4=1/2
distance_B=optimize(pertes,interval=c(minY, maxY), y=SimData, p=p4)
distance_B

## curbe fonction de pertes et valeur optimal
### je vais ecrire ma fonction perte y-->Uniforme(5,3, 10)


y =runif(5,3,10)
summary(y)

#### cas de la p= 1 ==> S coincide avec la mediane
pertes1<- function(s,p)
{
  p=1
  a=abs(s-y[1])^p + abs(s-y[2])^p + abs(s-y[3])^p + abs(s-y[4])^p + abs(s-y[5])^p
  b=a^(1/p)
}
curve(pertes1, from=-2 , to =10, col="red", main="distance Manhattan P=1")
grid(lty=2)

op1=optimize(pertes1,lower =3.282 , upper=8.528 , p=1)
op1$minimum  ##7.569019 = median(y)
op1$objective ### 5.344703 

###### distance Euclidienne p=2
pertes2<- function(s,p)
{
  p=2
  a=abs(s-y[1])^p + abs(s-y[2])^p + abs(s-y[3])^p + abs(s-y[4])^p + abs(s-y[5])^p
  b=a^(1/p)
}
curve(pertes2, from=-2 , to =10, col="red", main="distance Euclidienne P=2")
grid(lty=2)
op2=optimize(pertes2,lower =3.282 , upper=8.528 , p=2)
op2$minimum ##[1] 7.04145 = mean(y)
op2$objective ###[1] 3.212347

 par(mfrow=c(1,2))
 curve(pertes1, from=-10 , to =15, col="red", main="distance Manhattan P=1")
 grid(lty=2)
 abline(h=op1$objective,col="green")
 
 curve(pertes2, from=-10 , to =15, col="red", main="distance Euclidienne P=2")
 grid(lty=2)
 abline(h=op2$objective, col="green")
 par(mfrow=c(1,1))

 #### valeur optimal
 abline(h=op1$objective,col="green")
 abline(h=op2$objective, col="green")
 
