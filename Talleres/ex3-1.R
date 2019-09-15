## Ejemplo 3.1 p.86

we <- c(0.9473, 0.9655, 0.9703, 0.9757, 0.9775, 0.9788, 0.9861, 0.9887, 0.9964, 0.9974, 1.0002, 
        1.0016, 1.0077, 1.0084, 1.0102, 1.0132, 1.0182, 1.0225, 1.0248, 1.0306)

summary(we)

# Min.    1st Qu.     Median      Mean        3rd Qu.     Max. 
# 0.9473   0.9785      0.9988      0.9961      1.0110      1.0310 

############################
#### KOLMOGOROV-SMIRNOV ####
############################

ks.test(we, "pnorm", mean = 1, sd = 0.025, exact=TRUE)

#  One-sample Kolmogorov-Smirnov test

# data:  we
# D = 0.1106, p-value = 0.945
# alternative hypothesis: two-sided

# con exact=FALSE se eval?a el p-valor asint?tico
ks <- ks.test(we, "pnorm", mean = 1, sd = 0.025, exact=FALSE)
ks
#  One-sample Kolmogorov-Smirnov test

# data:  we
# D = 0.1106, p-value = 0.9673
# alternative hypothesis: two-sided

ks$statistic   # 0.1105988  p.86
ks$p.value    # 0.9672865  p.86


# comentario sobre el p-valor asintótico

n <- 20
v <- sqrt(n) * ks$statistic

# p-valor asintótico de la prueba de K-S
# $pv_{a} = 1 - K(\sqrt{n} D_n)$ p.84

kk <- seq(1, 10, 1)
tk <- exp(-2 * v^2 * kk^2)
Kx <- 1 + 2 * sum(tk%*%((-1)^kk))
pva <- 1 - Kx # p-valor asint?tico
pva # 0.9672865


# Cálculo del estadístico K-S 
# para hipótesis nula simple alternativa bilateral

aa <- sort(we)
UU <- table(cut(aa, breaks=aa, include.lowest=TRUE, right=TRUE))
UU1 <- cumsum(as.numeric(UU))
UUmas <- c(1, UU1)  # contiene frecuencias absolutas de empírica en X_(i) 

Dnmas <- max((1/n) * UUmas - pnorm(aa, mean=1, sd=0.025))
Dnmas  # 0.1105988

UUmenos <- c(0, UUmas[-20])
Dnmenos <- max(pnorm(aa, mean=1, sd=0.025) - (1/n)*UUmenos)
Dnmenos  # 0.04275023

Dn <- max(Dnmas,Dnmenos)
Dn  #  0.1105988

# Otra manera de calcular el estadístico K-S 
# para hipótesis nula simple alternativa bilateral
# con proceso empírico de v.a. uniformes

aa <- sort(we)
F0aa <- pnorm(aa, mean = 1, sd = 0.025)
gridy <- seq(0,1,length=150000)

UU <- table(cut(F0aa, breaks=gridy, include.lowest=TRUE, right=TRUE))
UU1 <- cumsum(as.numeric(UU))
UUmas <- c(1,UU1)
Dnmas <- max((1/n)*UUmas - gridy)
Dnmas  # 0.1105944

UUmenos <- c(0, UUmas[-20])
Dnmenos <- max(gridy - (1/n)*UUmenos)
Dnmenos  # 0.04274962

Dn <- max(Dnmas,Dnmenos)
Dn  #  0.1105944

############################
#### CRAMÉR - VON MISES ####
############################

aa <- sort(we)
F0aa <- pnorm(aa, mean=1, sd=0.025)
u <- seq(1, 2*n-1, 2)
n <- 20

## Fórmula [3.21], p.87 
cvm3 <- (1/(12*n)) + sum((sort(F0aa) - (u/(2*n)))^2)
cvm3 # 0.05255652  p.90

library(goftest)

cvm3 <- cvm.test(F0aa, null = "punif")
cvm3
#  Cramer-von Mises test of goodness-of-fit
#        Null hypothesis: uniform distribution

#  data:  F0aa
#  omega2 = 0.052557, p-value = 0.8661

############################
#### ANDERSON - DARLING ####
############################

n <- length(we)
w <- seq(1, 2*n-1, 2)
F0 <- pnorm(sort(we), mean=1, sd=0.025)

## Fórmula [3.22], p.87 
ad3 <- -n - (1/n) * sum( w * (log(sort(F0)) + log(1 - sort(F0, decreasing=TRUE))) )
ad3 # 0.3970706  p.90

ad3 <- ad.test(F0aa, null = "punif")
ad3
#  Anderson-Darling test of goodness-of-fit
#        Null hypothesis: uniform distribution

# data:  F0aa
# An = 0.39707, p-value = 0.8501

###

