# -------------- 14 oct. 2016 --------------
# ------------- Margot Selosse -------------
# ------- Parallel computing : TP1 --------

# Ex 1 ####

is.integer(2)
#2==2L  est TRUE car R parse avec la classe la plus generique 
if(sqrt(2)*sqrt(2)!=2) print("what??")
#erreurs de calculs sur sqrt(2) --> approximation qui est fait
if(0.1+0.2 == 0.3) print("result is ok")
if(0.1+0.2 != 0.3) print("no way")


# Ex 2 Optimisation ####

#1.
fex2q1 <- function(x){
  (sin(x))^2 + sqrt(abs(x-3))
}

#2.
retour <- c()
for (variable in seq(-6, 4, 0.1)) {
  retour <- c(retour, fex2q1(variable))
}
plot(seq(-6,4,0.1),retour, type='l')
#prof :
curve(fex2q1, from=-6, to=4,lwd=2)
grid(lty =1)

#3.
integrateValue <- integrate(fex2q1, -6, 4)

#4.
#my way
indexMin <- which.min(retour)
minimum <- retour[indexMin]
argmin <- seq(-6,4, 0.1)[indexMin]

#use of optimize
minf <- optimise(fex2q1,lower=-6,upper=4)
xmin <-minf$minimum
ymin <- minf$objective

#for maximum  
maxf <- optimise(fex2q1,lower=-6,upper=-0, maximum=TRUE)
xmax <-maxf$maximum
ymax <- maxf$objective

#prof : see the lines with max and min

abline(h=ymin,col="red")
abline(h=ymax,col="blue")

#explication : point stationnaire (non dérivabilité en x = 3)

# Ex 3 : Probleme ####

#1.
simuData <-function(n){
 runif(n)
}

#2.
# calcule la distance de Mikowski de param p et entre la valeur
#s et le ecteur de données y
perte <-function(s,y,p){
  (sum((abs(s-y))^p))^(1/p)
}

#3.
par(mfrow=c(2,2)) #--> configuer les plots (partager la fenetre de plots)
Y <- simuData(100)
pvec <- c(1,2,5,1/2)
bestY <- c()
for (pvar in pvec) {
  plot(Vectorize(function(x) perte(x,Y,pvar)), from=min(Y), to=max(Y))
  bestYi <- optimize(perte, c(min(Y),max(Y)), y=Y, p=pvar)$minimum
  abline(v=bestYi,col="red")
  #5.
  if(pvar==1) abline(v=median(Y),col="green")
  if(pvar==2) abline(v=mean(Y),col="blue")
  #end of 5.
  print(bestYi)
  #bestY <-c(bestY, bestYi)
}

#4.
print("with p=1, we are looking for the median : ")
print(median(Y))
print("with p=2, we are looking for the mean : ")
print(mean(Y))


