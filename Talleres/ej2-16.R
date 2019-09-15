## Ejercicio 2.16

data <- read.csv("datos2-16.csv", sep="\t", dec=",", header=FALSE)
summary(data)

nn<-length(data$V1) # 100
k <- 8

pi <- rep(1/8, 8)
Pi<-cumsum(pi)
muest <- mean(data$V1) 
muest # 324.57
sdest <- sd(data$V1) 
sdest # 20.93919
sdest <- sdest*sqrt((nn-1)/nn) 
sdest # 20.83423 (valor en el libro)

z<-qnorm(Pi); 
z<-c(-Inf, z[1:7], +Inf);
aa <- muest + z*sdest

UU<-table(cut(data$V1,breaks=aa))
UU2 <- as.numeric(UU) 

Xn2 <- ((k/nn)*sum(UU2^2)) - nn
Xn2  # 8


## calculo de Qn
dnorm(z)
#c1 <- (1/sd2)*diff(dnorm(z))
b1 <- diff(dnorm(z))
b1
z1 <- c(-100000, z[2:8], 100000)
#c2 <- (1/sdest)*diff(z1*dnorm(z1))
b2 <- diff(z1*dnorm(z1))
b2

alpha <- b1 %*% (UU2/pi) 
alpha  # 3.729075

beta <- b2 %*% (UU2/pi) 
beta   # 4.65333

# valores de j02, j12, j22 + 2j11 + 1 ver el libro, p.44
j02 <- 1
j12 <- 0
ll <- 2

lam1 <- j02 - b1 %*% (b1/pi) 
lam1  # 0.05496573
lam2 <- ll - b2 %*% (b2/pi) 
lam2  # 0.7212177
lam3 <- j12 - b1 %*% (b2/pi) 
lam3  # 4.445229e-18 

Qn <- (lam1*alpha^2 -2*lam3*alpha*beta + lam2*beta^2)/(nn*(lam1*lam2 - lam3^2))
Qn  # 4.132263  (en el libro Qn = 4.1339)


Yn2 <- Xn2 + Qn # estadístico de Nikulin-Rao-Robson
Yn2 # 12.13226  (en el libro Yn2 = 12.1339)

1-pchisq(12.13226, df=k-1, ncp = 0)  # 0.09629246 (en el libro 0.0962)

# Se rechaza la hipótesis nula 
# si el nivel de significación es mayor que 0.0962

