# -------------- 14 oct. 2016 --------------
# ------------- Margot Selosse -------------
# ------- Parallel compuuting : TP2 --------


# Ex1. Code Propre ####
#1.
f1<-function(n)for(i in 1:n)res<-res+1

#2.
integerSum <- function(n) {
  res <- 0
  for(i in 1:n){
    res <- res+i;
  }
  return(res)
}

#3
test1 <- integerSum(2)
sprintf("result test1 = %.f, expected result = 3", test1)
test2 <- integerSum(3)
sprintf("result test2 = %.f, expected result = 6", test1)
test3 <- integerSum(5)
sprintf("result test3 = %.f, expected result = 11", test1)

# Ex2. Chronométrage de la fonction ####
f2 <- function(n) sum(as.double(1:n))
f3 <- function(n) n * (n + 1) / 2

#Chrono f1:
T1<-Sys.time()
integerSum(100000)
T2<-Sys.time()
Tdiff1 <- difftime(T1, T2) 
sprintf("result for function integerSum %.5f", abs(Tdiff1))

#chrono f2:
T1<-Sys.time()
f2(100000)
T2<-Sys.time()
Tdiff2 <- difftime(T1, T2) 
sprintf("result for function f2 %.5f", abs(Tdiff2))

#chrono f3:
T1<-Sys.time()
f3(100000)
T2<-Sys.time()
Tdiff3 <- difftime(T1, T2) 
sprintf("result for function f3 %.5f", abs(Tdiff3))

# teacher solution :
n <- 1e5
system.time(replicate(20,integerSum(n)))
system.time(replicate(20,f2(n)))
system.time(replicate(20,f3(n)))

# Ex.3 Session de debug
joe=function(x,k){
  n=length(x)
  r=NULL
  for(i in 1:(n-k+1)) {
    if(all(x[i:(i+k-1)]==1)) r<-c(r,i)
  }
  r
}

#test
X <- c(0,0,1,1,1,1,1,0,0,1,1,0,0,1,1)
#expected values for k=2: 3,4,5,6,10,14
debug(joe)
undebug(joe)
