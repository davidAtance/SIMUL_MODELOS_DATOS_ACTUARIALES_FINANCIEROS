#Variables no correlacionadas
set.seed(123)

n <- 5000

#Generamos dos variables normales, independientes a partir de los aleatorios. 
x <- rnorm(n)
y <- rnorm(n)

plot(x,y,
     pch = 16,
     cex = 0.4,
     col = rgb(0,0,1,0.25),
     xlab = expression(X[1]),
     ylab = expression(X[2]),
     main = "Variables independientes")

cor(x,y)

#Sin embargo si tenemos dos variables normales que están correlacionadas la ilustración 
#es diferente: 
set.seed(123)

n <- 5000
x <- rnorm(n)
rho <- 0.8

y <- rho*x + sqrt(1-rho^2)*rnorm(n)

plot(x,y,
     pch = 16,
     cex = 0.4,
     col = rgb(1,0,0,0.25),
     xlab = expression(X[1]),
     ylab = expression(X[2]),
     main = expression(rho==0.8))

cor(x,y)

#########################################################################
#Cholesky a partir matriz correlaciones: 
#########################################################################
#Supongamos que queremos generar escenarios correlacionados para:
#X1=Coste de siniestros de Auto
#X2=Coste de siniestros de Hogar

#la cual tiene la siguiente matriz de correlaciones:
R <- matrix(c(1.00, 0.65, 
              0.65, 1.00), nrow = 2, byrow = TRUE)

R

#Nuestro objetivo es a partir de R generar dos variables normales multivariantes 
#correlacionadas X = Z · L (siendo L la matriz de Cholesky)
??chol

L <- t(chol(R))

#Ahora verificamos que L sea correcta => R = L·L^T
L%*%t(L)

#muy parecida a nuestra matriz de correlaciones 
R

#Ahora una vez que tenemos L, vamos a generar las Z~N(0,1) 
set.seed(123)
n <- 10000
Z <- matrix(rnorm(2*n), nrow = 2, ncol = n)

mean(Z[1,]); mean(Z[2,]) 
sd(Z[1,]); sd(Z[2,])

par(mfrow = c(1,2))
hist(Z[1,], probability = TRUE, breaks = 30, col = "lightblue", border = "white",
     main = "N(0,1)", xlab = "Euros")
curve(dnorm(x, mean = mean(Z[1,]), sd = sd(Z[1,])),
      add = TRUE, col = "red", lwd = 3)

hist(Z[2,], probability = TRUE, breaks = 30, col = "lightblue", border = "white",
     main = "N(0,1)", xlab = "Euros")
curve(dnorm(x, mean = mean(Z[2,]), sd = sd(Z[2,])),
      add = TRUE, col = "red", lwd = 3)

#Con la matriz Z ya podemos introducir la correlación X = Z · L
X <- L%*%Z

mean(X[1,]); mean(X[2,])
sd(X[1,]); sd(X[2,])

#Por último verificamos la matriz de correlación: 
cor(t(X))

#Visualizacion
par(mfrow = c(1,1))
plot(X[1,], X[2,], pch = 16,
     cex = 0.4, col = "darkred",
     xlab = "Auto", ylab = "Hogar",
     main = "Costes correlacionados")


#Si suponemos que la cartera de autos tiene: 
mu_1 = 10000
sigma_1 = 2500

#Cartera hogar
mu_2 = 7000
sigma_2 = 1800 

#prodriamos construir Y1 = X1·sigma1 + mu; Y2 = X2·L + mu
Y <- matrix(0,nrow=2,ncol=n)

Y[1,] <- mu_1 + sigma_1*X[1,]
Y[2,] <- mu_2 + sigma_2*X[2,]

#Comprobacion de media y sd
mean(Y[1,])
sd(Y[1,])

mean(Y[2,])
sd(Y[2,])

cor(t(Y))

#Visualizacion de las variables creadas: 
par(mfrow = c(1,2))

# Auto
hist(Y[1,], probability = TRUE, breaks = 30,
     col = "lightblue", border = "white",
     main = "Coste anual Auto", xlab = "Coste (€)")

curve(dnorm(x, mean = mu_1, sd = sigma_1),
      add = TRUE, col = "red", lwd = 3)

# Hogar
hist(Y[2,], probability = TRUE, breaks = 30,
     col = "lightgreen", border = "white",
     main = "Coste anual Hogar", xlab = "Coste (€)")

curve(dnorm(x, mean = mu_2, sd = sigma_2),
      add = TRUE, col = "red", lwd = 3)

#Vamos a calcular el VaR, recordar que es la perdida dada un nivel de confianza
#primero percentil
VaR995_Auto  <- quantile(Y[1,], 0.995)
VaR995_Hogar <- quantile(Y[2,], 0.995)

VaR995_Auto
VaR995_Hogar

#Lo incorporo al grafico visual: 
par(mfrow = c(1,2))

# AUTO
hist(Y[1,], probability = TRUE, breaks = 30,
     col = "lightblue", border = "white",
     main = "Auto", xlab = "Coste (€)")

curve(dnorm(x, mean = mu_1, sd = sigma_1),
      add = TRUE, col = "red", lwd = 3)

abline(v = VaR995_Auto, col = "darkgreen", lwd = 3)

# HOGAR
hist(Y[2,], probability = TRUE, breaks = 30,
     col = "lightgreen", border = "white",
     main = "Hogar", xlab = "Coste (€)")

curve(dnorm(x, mean = mu_2, sd = sigma_2),
      add = TRUE, col = "red", lwd = 3)

abline(v = VaR995_Hogar, col = "darkgreen", lwd = 3)

#ahora a partir de los percentiles y las medias podemos calcular el VaR:
Capital_Auto  <- VaR995_Auto  - mu_1
Capital_Hogar <- VaR995_Hogar - mu_2

Capital_Auto
Capital_Hogar

#También podemos hacer el coste agregado de ambos seguros: 
Coste_Total <- Y[1,] + Y[2,]

#Obtener también el VaR al 99.5% de toda la cartera 
VaR995_Total <- quantile(Coste_Total, 0.995) - mean(Coste_Total)

VaR995_Total

par(mfrow = c(1,1))
hist(Coste_Total, probability = TRUE, breaks = 30,
     col = "lightgray", border = "white",
     main = "Coste agregado de la aseguradora", xlab = "Coste (€)")

curve(dnorm(x, mean = mean(Coste_Total), sd = sd(Coste_Total)),
      add = TRUE, col = "red", lwd = 2)

abline(v = VaR995_Total, col = "darkgreen",
       lwd = 3)

