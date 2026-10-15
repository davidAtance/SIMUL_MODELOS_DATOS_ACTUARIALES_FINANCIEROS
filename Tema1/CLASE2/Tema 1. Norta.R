rm(list=ls())
set.seed(123)
n <- 5000

# =====================================================
# 1. Lognormales independientes
# =====================================================

X1 <- rlnorm(n, meanlog = 0, sdlog = 0.5)
X2 <- rlnorm(n, meanlog = 0, sdlog = 0.5)
cor(X1, X2)

# =====================================================
# 2. Normales correlacionadas mediante Cholesky
# =====================================================

R <- matrix(c(1,0.7,0.7,1),2,2)
L <- t(chol(R))
Z <- matrix(rnorm(2*n),2,n)
Y <- L %*% Z

cor(Y[1,], Y[2,])
# =====================================================
# 3. NORTA
# =====================================================

U <- pnorm(Y)
N1 <- qlnorm(U[1,],
             meanlog = 0,
             sdlog = 0.5)
N2 <- qlnorm(U[2,],
             meanlog = 0,
             sdlog = 0.5)

cor(N1, N2)

# =====================================================
# Gráficos
# =====================================================
par(mfrow=c(1,3))

plot(X1,X2,
     pch=19,
     cex=0.3,
     col=rgb(0,0,1,0.2),
     main="Independientes",
     xlab="Lognormal 1",
     ylab="Lognormal 2")

plot(Y[1,],Y[2,],
     pch=19,
     cex=0.3,
     col=rgb(1,0,0,0.2),
     main="Cholesky",
     xlab="Normal 1",
     ylab="Normal 2")

plot(N1,N2,
     pch=19,
     cex=0.3,
     col=rgb(0,0.6,0,0.2),
     main="Objetivo (NORTA)",
     xlab="Lognormal 1",
     ylab="Lognormal 2")


###########################################################################3
#Ejemplo actuarial
#Supongamos una aseguradora que cubre tres riesgos distintos:

####################
#Riesgo 1: Automóviles
#La severidad de los siniestros de automóvil suele presentar:
#-valores siempre positivos,
#-fuerte asimetría a la derecha,
#-muchos siniestros pequeños y unos pocos muy grandes.

#Por ello modelizamos:
#X1∼Lognormal(8,0.7^2)

########################
#Riesgo 2: Hogar
#Las pérdidas por hogar suelen concentrarse alrededor de un valor medio, 
#pero con cierta dispersión positiva. Una distribución Gamma resulta adecuada:
#  
#X2∼Gamma(α=5,β=1500)
#donde:
#  E(X_2)=αβ=5^1500 = 7500; 

####################################
#Riesgo 3: Responsabilidad Civil
#En responsabilidad civil pueden aparecer indemnizaciones excepcionalmente elevadas.
#Las colas pesadas hacen recomendable una Pareto:
#
#  X3∼Pareto(xm=5000,α=3)

#Ahora sabemos que la dependencia es la siguiente matriz:
rm(list=ls())
set.seed(123)

#--------------------------------------------------
# Matriz de correlaciones
#--------------------------------------------------
R <- matrix(c(1.00, 0.55, 0.40,
              0.55, 1.00, 0.65,
              0.40, 0.65, 1.00), 3,3,byrow=TRUE)

#--------------------------------------------------
# Cholesky
#--------------------------------------------------
L <- t(chol(R))
n <- 10000
Z <- matrix(rnorm(3*n),3,n)
Y <- L %*% Z

mean(Y[1,])
mean(Y[2,])
mean(Y[3,])

var(Y[1,])
var(Y[2,])
var(Y[3,])

cor(Y[1,], Y[2,])
cor(Y[1,], Y[3,])
cor(Y[3,], Y[2,])

#--------------------------------------------------
# Uniformes correlacionadas
#--------------------------------------------------
U <- pnorm(Y)

cor(U[1,], U[2,])
cor(U[1,], U[3,])
cor(U[3,], U[2,])

#--------------------------------------------------
# Transformaciones marginales
#--------------------------------------------------
# Automóviles (Lognormal)
X1 <- qlnorm(U[1,],
             meanlog = 8,
             sdlog = 0.7)

# Hogar (Gamma)
X2 <- qgamma(U[2,],
             shape = 5,
             scale = 1500)

# Responsabilidad Civil (Pareto)
xm <- 5000
alpha <- 3
X3 <- xm*(1-U[3,])^(-1/alpha)

#--------------------------------------------------
# Correlaciones observadas
#--------------------------------------------------
cor(cbind(X1,X2,X3))

par(mfrow=c(1,3))
hist(X1,
     prob=TRUE,
     col="lightblue",
     main="Automóviles\n(Lognormal)")

hist(X2,
     prob=TRUE,
     col="lightgreen",
     main="Hogar\n(Gamma)")

hist(X3,
     prob=TRUE,
     col="lightpink",
     main="Responsabilidad Civil\n(Pareto)")


CosteTotal <- X1 + X2 + X3

#Prima Pura estimada
mean(CosteTotal)

quantile(CosteTotal, 0.995)

#VaR 99.5%
mean(CosteTotal) - quantile(CosteTotal, 0.995)

par(mfrow=c(1,1))
PrimaPura <- mean(CosteTotal)

VaR995 <- quantile(CosteTotal, 0.995)

hist(CosteTotal, probability = TRUE, breaks = 40,
     col = "lightblue", border = "white",
     main = "Distribución del coste agregado",
     xlab = "Coste total")

lines(density(CosteTotal), col = "darkgreen", lwd = 3)
abline(v = PrimaPura, col = "blue", lwd = 3)
abline(v = VaR995, col = "red", lwd = 3)

legend("topright",
       legend = c("Prima pura", "VaR 99.5%"),
       col = c("blue","red"),
       lwd = 3,
       bty = "n")