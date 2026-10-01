#Aplicacion de montecarlo utilizando Cholesky
library(quantmod)

# Descargar datos de SANTANDER (SAN), IBERDROLA (IBE) y INDITEX (ITX)
getSymbols(c("SAN.MC",
             "IBE.MC",
             "ITX.MC"), src = "yahoo", from = "2020-01-01")

SAN <- Ad(`SAN.MC`)
IBE <- Ad(`IBE.MC`)
ITX <- Ad(`ITX.MC`)

#Precios diarios de las acciones de SAN, IBE, ITX
par(mfrow=c(1,3))
plot(SAN)
plot(IBE)
plot(ITX)

#Unificamos la serie: 
precios <- na.omit(merge(SAN, IBE, ITX))
#save(precios, file = "PreciosAcciones.RData")
#load(file = "PreciosAcciones.RData")

colnames(precios) <- c("Santander", "Iberdrola", "Inditex")

head(precios)

#Para trabajar con Montecarlo conviene recordar que: 
#St = S_{t-1} exp{(mu-sigma^2/2)At + (sigma·raiz(At)·N(0;1))}
#Necesitamos obtener mu y sigma de los rendimientos no del precio del activo

rend <- na.omit(diff(log(precios)))
head(rend)

par(mfrow=c(1,3))
plot(rend[,1])
plot(rend[,2])
plot(rend[,3])

#Calculamos las medias y las dt
mu <- colMeans(rend)
mu

#Generamos matriz var-covar
Sigma <- cov(rend)

#También podríamos sacar la matriz de correlaciones (y aplicar el metodo anterior)
#R <- cor(rend)

#Aplicamos Chol para obtener L --> Sigma = L·L^T
L <- t(chol(Sigma))

#Ahora una vez tenemos cholesky sobre L, vamos obtener las Z~N(0,1) no correlacionadas
n <- 10000
Z <- matrix(rnorm(3*n), nrow = 3)
X <- L %*% Z

#Valor estimado
cov(t(X))

#valor real -- Sigma
cov(rend)

#Diferencia entre una y otra
round(cov(t(X)) - cov(rend), 6)

#Grafico de las variables generadas no correlaciondas y si correlacionadas: 
par(mfrow = c(1,2))
plot(Z[1,], Z[2,],
     pch = 19, cex = 0.4, col = "lightblue",
     xlab = "Santander", ylab = "Iberdrola",
     main = "Shocks no correlacionados")

plot(X[1,], X[2,],
     pch = 19, cex = 0.4, col = "lightblue",
     xlab = "Santander", ylab = "Iberdrola",
     main = "Shocks correlacionados")

#ahora activo 1 vs 3
par(mfrow = c(1,2))
plot(Z[1,], Z[3,],
     pch = 19, cex = 0.4, col = "darkgreen",
     xlab = "Santander", ylab = "Inditex",
     main = "Shocks no correlacionados")

plot(X[1,], X[3,],
     pch = 19, cex = 0.4, col = "darkgreen",
     xlab = "Santander", ylab = "Inditex",
     main = "Shocks correlacionados")

#Ahora ya tenemos las variables generadas X~N(0, Sigma)
mean(X[1,])
mean(X[2,])
mean(X[3,])

apply(X,1,var)
Sigma

#Ahora ya una vez tenemos las N(0, Sigma) ya podemos aplicar Montecarlo
#St = S_{t-1} exp{(mu-sigma^2/2)At + (sigma·raiz(At)·N(0;1))}

Precio_fut <- matrix(NA, 10000, ncol = 3)
colnames(Precio_fut) <- c("Santander", "Iberdrola", "Inditex")

sd1 <- sqrt(Sigma[1,1])
sd2 <- sqrt(Sigma[2,2])
sd3 <- sqrt(Sigma[3,3])

S0_1 <- as.numeric(last(precios$Santander))
S0_2 <- as.numeric(last(precios$Iberdrola))
S0_3 <- as.numeric(last(precios$Inditex))

#Quitamos sd1 del segundo parentesis pq ya he multiplicado por sigma la N(0,sigma)

Precio_fut[,1] <- S0_1 * 
  exp((mu[1] - sd1^2/2)*1 + (sqrt(1)*X[1,]))

Precio_fut[,2] <- S0_2 * 
  exp((mu[2] - sd2^2/2)*1 + (sqrt(1)*X[2,]))

Precio_fut[,3] <- S0_3 * 
  exp((mu[3] - sd3^2/2)*1 + (sqrt(1)*X[3,]))


#Grafico visual de la proyeccion
ultimos20 <- tail(precios, 20)

par(mfrow=c(1,1))

min1 <- min(ultimos20$Santander, Precio_fut[,1])
max1 <- max(ultimos20$Santander, Precio_fut[,1])

par(mfrow=c(1,1))
# Últimos 20 datos reales
plot(1:20, ultimos20$Santander,
     type="l", lwd=3, col="black",
     xlim=c(1,21), ylim=c(min1, max1),
     xlab="Tiempo", ylab="Precio",
     main="Banco Santander: datos reales y proyección Monte Carlo")

# Unimos con el precio actual
points(20, tail(ultimos20$Santander, 1),
       pch=19, col="red", cex=1.3)

# Simulaciones para t+1
segments(x0=20, y0=tail(ultimos20$Santander, 1), 
         x1=21, y1=Precio_fut[,1], col="lightblue")

#Prediccion a futuro:
last <- as.numeric(tail(ultimos20$Santander, 1))
media <- as.numeric(quantile(Precio_fut[,1], 0.50))

quantile(Precio_fut[,1], 0.40)
quantile(Precio_fut[,1], 0.60)

quantile(Precio_fut[,1], 0.995)

lines(c(20:21), c(last, media), col = "red")

#VaR-99.5
tail(ultimos20$Santander, 1) - quantile(Precio_fut[,1], 0.995)

par(mfrow = c(1,1))
hist(Precio_fut[,1], breaks = 20, probability = T, col = "lightblue",
     xlab = "precio", ylab="", main="Escenarios precio Futuro Santander")
#Recordar que St -- log(n)--- N(ln(S0)+mu-sigma^2/2, sigma^2)
meanlog <- log(S0_1) + mu[1] - sd1^2/2
sdlog   <- sd1

curve(dlnorm(x, meanlog = meanlog, sdlog   = sdlog),
      add = TRUE, col = "red", lwd = 3)

#######################################################
#Voy hacer una proyeccion a más largo Plazo
#para que se vea como funciona el aleatorio: 
n <- 10      # numero de sendas
T <- 10      # horizonte temporal (10 días)

# Precio actual Santander
S0 <- as.numeric(last(precios$Santander))

# Media y volatilidad Santander
mu_SAN <- mu[1]
sigma_SAN <- sqrt(Sigma[1,1])

# Matriz para guardar trayectorias
ST <- matrix(NA,
             nrow = n,
             ncol = T + 1)

# Columna inicial
ST[,1] <- S0


for(t in 2:(T+1)){
  Z <- sample(X[1,], 10)
  ST[,t] <- ST[,t-1] * exp((mu_SAN - sigma_SAN^2/2) + sigma_SAN * Z)
  
}
dim(ST)

matplot(t(ST[1:10,]), type = "l", lty = 1, 
        col = "lightblue", xlab = "Días", ylab = "Precio", 
        main = "Santander: 10.000 trayectorias Monte Carlo")

ultimos20 <- tail(precios$Santander,20)
min1 <- min(ultimos20, ST)
max1 <- max(ultimos20, ST)

plot(1:20, ultimos20, type="l", lwd=3, col="black", 
     xlim=c(1,30), ylim=c(min1,max1),
     xlab="Tiempo", ylab="Precio",
     main="Banco Santander: histórico + Monte Carlo")

# Trayectorias simuladas
for(i in 1:10){
  lines(20:(20+T), ST[i,], col="red4")}
