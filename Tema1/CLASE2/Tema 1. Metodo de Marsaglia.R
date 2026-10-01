#=========================================
# Método de Marsaglia
#=========================================
set.seed(123)
n <- 10000

# Uniformes
u1 <- runif(n)
u2 <- runif(n)

mean(u1); mean(u2)
sd(u2); sd(u2)

#Ahora transformamos u1, u2 a U(-1,1) => U(a,b) = a + u1(b-a)
v1 <- 2*u1-1
v2 <- 2*u2-1

mean(v1); mean(v2)
sd(v2); sd(v2)
r2 <- v1^2 + v2^2

datos <- data.frame(v1, v2, r2)
aceptados <- datos$r2 <= 1
rechazados <- datos$r2 > 1

#Visualizacion aceptados-rechazados
par(mfrow=c(1,1))
plot(datos$v1[rechazados], datos$v2[rechazados],
     col="darkred", pch=19, cex=0.5,
     xlab="v1", ylab="v2", main="Método Polar de Marsaglia")
points(datos$v1[aceptados], datos$v2[aceptados],
       col="lightblue", pch=19, cex=0.5)

theta <- seq(0,2*pi,length=500)
lines(cos(theta), sin(theta), lwd=3, col = "grey")


#Ahora vamos con los datos aceptados 
reales_r2 <- datos[datos$r2 <= 1,]

reales_r2$x1 <- reales_r2$v1*sqrt((-2*log(reales_r2$r2))/reales_r2$r2)
reales_r2$y1 <- reales_r2$v2*sqrt((-2*log(reales_r2$r2))/reales_r2$r2)

mean(reales_r2$x1)
sd(reales_r2$x1)

mean(reales_r2$y1)
sd(reales_r2$y1)

plot(reales_r2$x1, reales_r2$y1,
     pch = 19, cex = 0.4, col = "lightblue",
     xlab = expression(Z[1]), ylab = expression(Z[2]),
     main = "Normales generadas mediante Marsaglia")

par(mfrow = c(1,2))
hist(reales_r2$x1, probability = TRUE, breaks = 30,
     col = "lightblue",
     main = "N(0,1) por Marsaglia",
     xlab = "x1")
curve(dnorm(x, mean = 0, sd = 1),
      add = TRUE, col = "red", lwd = 3)

hist(reales_r2$y1, probability = TRUE, breaks = 30,
     col = "lightblue",
     main = "N(0,1) por Marsaglia",
     xlab = "y1")
curve(dnorm(x, mean = 0, sd = 1),
      add = TRUE, col = "red", lwd = 3)


#############################################################
#A través de funcion más rapido
marsaglia <- function(n){
  z1 <- numeric(0)
  z2 <- numeric(0)
  v1_ <- numeric(0)
  v2_ <- numeric(0)
  
  while(length(z1) < n){
    # Uniformes U(0,1)
    u1 <- runif(1)
    u2 <- runif(1)
    # Transformación a U(-1,1)
    v1 <- 2*u1 - 1
    v2 <- 2*u2 - 1
    w <- v1^2 + v2^2
    
    if(w > 0 && w < 1){
      z1_nuevo <- v1*sqrt(-2*log(w)/w)
      z1 <- c(z1, z1_nuevo)
      
      z2_nuevo <- v2*sqrt(-2*log(w)/w)
      z2 <- c(z2, z2_nuevo)
      
      v1_ <- c(v1_, v1)
      v2_ <- c(v2_, v2)
    }
  }
  list(z1 = z1, z2 = z2, v1 = v1_, v2 = v2_)
}

#Generador de normales estandar con marsaglia
set.seed(123)

z <- marsaglia(10000)

mean(z$z1)
var(z$z1)

mean(z$z2)
var(z$z2)

cor(z$z1,z$z2)

#Visualizacion
par(mfrow = c(1,1))
plot(z$v1, z$v2, pch = 19, cex = 0.4, asp = 1, col = "darkgreen",
     xlab = expression(V[1]), ylab = expression(V[2]),
     main = "Puntos aceptados por el método de Marsaglia")

# Circunferencia unidad
theta <- seq(0, 2*pi, length.out = 1000)

lines(cos(theta), sin(theta), col = "red", lwd = 3)

# Líneas auxiliares
abline(h = 0, lty = 2, col = "gray70")
abline(v = 0, lty = 2, col = "gray70")

legend("topright", legend = c("Puntos aceptados", "Círculo unidad"),
       col = c("darkgreen","red"), pch = c(19,NA), cex = 0.7, lwd = c(NA,3))


plot(z$z1, z$z2, pch = 16, cex = 0.4, col = "blue",
     xlab = expression(Z[1]), ylab = expression(Z[2]),
     main = "Normales estándar generadas con Marsaglia")

abline(h = 0, col = "red", lty = 2)
abline(v = 0, col = "red", lty = 2)

#Funcion de densidad de los valores generados por z1 y z2
par(mfrow=c(1,2))
#Z1
hist(z$z1, probability = TRUE, breaks = 40,
     col = "lightblue", border = "white",
     main = expression(Z[1]~" generado por Marsaglia"), xlab = "")
curve(dnorm(x), add = TRUE, col = "red", lwd = 3)
#Z2
hist(z$z2, probability = TRUE, breaks = 40,
     col = "lightblue", border = "white",
     main = expression(Z[2]~" generado por Marsaglia"), xlab = "")
curve(dnorm(x), add = TRUE, col = "red", lwd = 3)

########################################################################
#EJERCICIO:
#Una compañía aseguradora ha estimado que el coste agregado anual de los 
#siniestros de una cartera de seguros del hogar puede modelizarse mediante 
#una distribución Normal~N(15000, 3000^2).
#Se pide: 
#1. Utilice el método polar de Marsaglia para generar 10.000 observaciones 
#de una variable normal estándar Z~N(0,1)
#2. Certifique de varias formas que la muestra sigue una distribución normal estándar.
#3. Transforme las observaciones generadas para obtener simulaciones 
#de dicha cartera de seguros de hogar
#4. Represente gráficamente la distribución simulada de la cartera
#5. Calcule: (a) El coste medio esperado. y (b) El VaR al 99\%. 
