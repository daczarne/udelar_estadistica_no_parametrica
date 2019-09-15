## estudio de potencia ejercicio 2.6

alpha <- 0.05
pi0 <- rep(0.1, 10)
k <- length(pi0)

N <- 100000 #numero de realizaciones del vector U
ta <- 200 # tamano de la muestra en cada realizacion
U <- rmultinom(n = N, size = ta, prob = pi0)
#U
#U[,3]
x2 <- apply(U, 2, function(x) sum(((x-ta*pi0)^2)/(ta*pi0)) )
#x2
sx2<-sort(x2)

rcritap <- sx2[N*(1-alpha)]
rcritap  # nivel critico aproximado
qchisq(0.95, df=k-1, ncp = 0)  # nivel critico asintotico

nivap <- 1-pchisq(sx2[N*(1-alpha)], df=k-1, ncp = 0) # aproximacion nivel de significacion
nivap

## bajo la alternativa

pi1 <- c(rep(0.11,5),rep(0.09,5))
N1 <- 100000
to <- 200
U1 <- rmultinom(n = N1, size = to, prob = pi1)
x21 <- apply(U1, 2, function(x) sum(((x-to*pi1)^2)/(to*pi1)) )
sx21<-sort(x21)

nn <-500

d <- 5*((0.11 - 0.1)^2/0.1) + 5*((0.09 - 0.1)^2/0.1)

potap <- 1-pchisq(sx21[N1*(1-alpha)], df=k-1, ncp = nn*d)
potap

## Calculo potencia con el paquete pwr
library(pwr)
pwr.chisq.test(w = 0.1, N =500, df = 9, sig.level = 0.05, power = NULL)

pwr.chisq.test(w = 0.1, N =NULL, df = 9, sig.level = 0.05, power = 0.50) 
# se obtiene N = 881.0379
pwr.chisq.test(w = 0.1, N =NULL, df = 9, sig.level = 0.05, power = 0.95) 
# se obtiene N = 2358.944


## estudio de potencia chi-cuadrado dado

alpha <- 0.05
pi0d <- rep(1/6, 6)
k <- length(pi0d)

N <- 100000 #numero de realizaciones del vector U
ta <- 200 # tamano de la muestra
Ud <- rmultinom(n = N, size = ta, prob = pi0d)
#Ud
x2d <- apply(Ud, 2, function(x) sum(((x-ta*pi0d)^2)/(ta*pi0d)) )
#x2d
sx2d<-sort(x2d)

rcritapd <- sx2d[N*(1-alpha)]
rcritapd  # nivel critico aproximado 
qchisq(0.95, df=k-1, ncp = 0)  # nivel critico asintotico 11.0705

nivap <- 1-pchisq(sx2d[N*(1-alpha)], df=k-1, ncp = 0) # aproximacion nivel de significacion
nivap 

## bajo la alternativa

pi1d <- c(rep(3/20,5),5/20)
N1 <- 100000
to <- 200
U1d <- rmultinom(n = N1, size = to, prob = pi1d)
x21d <- apply(U1d, 2, function(x) sum(((x-to*pi1d)^2)/(to*pi1d)) )
sx21d<-sort(x21d)
sx21d[N1*(1-alpha)]

 
dd <- 5*((3/20 - 1/6)^2/(1/6)) + (5/20 - 1/6)^2/(1/6)
ncpard<- 120*dd  # 120 como en el articulo de Guenther
potapd <- 1-pchisq(sx21d[N1*(1-alpha)], df=k-1, ncp = ncpard)
potapd  # 0.4375431


## Calculo potencia con el paquete pwr
library(pwr)
pwr.chisq.test(w = sqrt(dd), N =NULL, df = 5, sig.level = 0.05, power = 0.4329) 
# se obtiene N = 120.007
pwr.chisq.test(w = sqrt(dd), N =NULL, df = 5, sig.level = 0.05, power = 0.90) 
# se obtiene N = 329.3893 


