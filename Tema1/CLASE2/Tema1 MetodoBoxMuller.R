#=============================================
# Metodo de Box-Muller
#=============================================
set.seed(123)#semilla
n <- 10000 #numero de simulaciones

# Uniformes U(0,1)
u1 <- runif(n)
u2 <- runif(n)

# Transformacion Box-Muller para conseguir que u1 y u2 sigan una N(0,1) indep
z1 <- sqrt(-2*log(u1))*cos(2*pi*u2)
z2 <- sqrt(-2*log(u1))*sin(2*pi*u2)

#Comprobar que cumplen requisitos media y var
mean(z1)
var(z1)

mean(z2)
var(z2)

cor(z1, z2)

plot(z1, z2, main = "Box-Muller")

#Histograma y densidad teórica
hist(z1, probability = TRUE, breaks = 30,
     col = "lightblue", border = "white", xlab = "z",
     main = "Normal estandar: Metodo Box-Muller")

curve(dnorm(x), add = TRUE, col = "red", lwd = 3)

#Densidad teórica vs real
par(mfrow = c(1,2))
dens <- density(z1)
dens2 <- density(z2)
plot(dens, lwd = 3, col = "black", main = "Box-Muller", xlab = "z")
curve(dnorm(x), add = TRUE, col = "blue", lwd = 3)
legend("topright", legend = c("Generada", "Teorica"),
       col = c("black", "blue"), lwd = 3, cex = 0.5)

plot(dens2, lwd = 3, col = "black", main = "Box-Muller", xlab = "z")
curve(dnorm(x), add = TRUE, col = "blue", lwd = 3)
legend("topright", legend = c("Generada", "Teorica"),
       col = c("black", "blue"), lwd = 3, cex = 0.5)

############################################################
#Ejercicio actuarial: importe agregado de siniestros
#
#Suponga que una compañía aseguradora ha estimado que el coste anual 
#agregado de los siniestros de dos cartera puede aproximarse mediante
#X1∼N(10000,2500^2)
#X2∼N(50000,3500^2)
#donde:
#Cartera 1
#μ1=10000 → coste medio anual esperado.
#σ1=2500 → volatilidad de los costes
#Cartera 2
#μ2=50000 → coste medio anual esperado.
#σ2=3500 → volatilidad de los costes
#Se supone que ambas líneas de negocio son independientes.
#Calcule: 
#1. Utilice el método de Box--Müller para generar 10.000 
#observaciones de las variables normales estándar necesarias.
#2. Transforme las variables obtenidas en el paso anterior
# para muestras de las dos carteras X1 y X2.
#3. Obtenga la pérdida agregada de la cartera definida por: S = X1 + X2
#4. Calcule el VaR y tVaR de dicha cartera
###################################################################


#Concepto de VaR:
#El Value at Risk (VaR) representa el nivel de confianza alpha que representa la pérdida mínima 
#que será superada únicamente con probabilidad $1-\alpha$.

#Ejemplo actuarial: El $VaR_{99\%}$ 
#representa la pérdida que únicamente será superada en el 1\% de los escenarios posibles

#VaR99 -- Pérdida maxima que excede en el 1% de los escenarios

