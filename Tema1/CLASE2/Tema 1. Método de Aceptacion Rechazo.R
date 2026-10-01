#=========================================
# Metodo de Aceptacion-Rechazo
# Gamma(2,1)
#=========================================
#Semilla
set.seed(123)
n <- 5000

#====================================
# Método de Aceptación-Rechazo
#====================================
###################################################################
f_gamma <- function(x){
  x*exp(-x)}

#Dominio Gamma
x_dominio_gamma <- seq(0,8,length=1000)

# Dominio
x <- seq(0,8,length=1000)
u1 <- runif(1000)
u2 <- runif(1000)
# Parametros
a <- 0
b <- 8
k <- exp(-1) #valor maximo

# Puntos del ejemplo
U1 <- a + (b-a)*u1 #trasnformo U(0,1) -- U(a, b)
U2 <- k*u2 #transformo U(0,1) -- U(0, k)

#Ejemplo sencillo
#U1 <- c(0.80, 6.40, 2.00, 3.20)
#U2 <- c(0.1214, 0.1472, 0.1803, 0.2502)

# Valor de la función
fx <- f_gamma(U1)

#toma de decisión
head(data.frame(U2, fx))

# Aceptados / rechazados
aceptado <- U2 <= fx

# Curva
plot(x_dominio_gamma, f_gamma(x_dominio_gamma),
     type="l", lwd=3, col="blue",
     ylim=c(0,0.4), xlab="x", ylab="f(x)",
     main="Método de aceptación-rechazo")

# Puntos aceptados
points(U1[aceptado], U2[aceptado],
       pch=19, col="darkgreen", cex=1)

# Puntos rechazados
points(U1[!aceptado], U2[!aceptado],
       pch=19, col="red", cex=1)

#Valores maximos de nuestra generacion de variables
lines(density(U1[aceptado])$x, density(U1[aceptado])$y,
      col = "black",
      lwd = 3)

# Rectángulo de simulación
abline(h=exp(-1), lty=2, col="gray50")

legend("topright",
       legend=c("f(x)=x exp(-x)", "f(x) Generada",
                "Aceptado",
                "Rechazado"),
       col=c("blue", "black", "darkgreen","red"),
       lwd=c(3, 3, NA, NA),
       pch = c(NA, NA, 19, 19), cex = 0.8)


####################################################################
#Vamos a suponer una distribucion Beta(4,3)
####################################################################
#Funcion de densidad:
#f(x) = 1/B(4,3)·x^{4-1}·(1-x)^{3-1}
#Si sabemos que B(4,3) = 1/60
beta(4, 3)

#podemos despejar f(x)
#f(x) = 1/(1/60)·x^{3}(1-x)^{2}

#media E(x) = alpha/(alpha + beta)
#varianza Var(x) = (alpha · beta)/((alpha + beta)^2 (alpha + beta + 1))

#Creamos funcion de antes: 

set.seed(123)
f_beta <- function(x){
  60*x^3*(1-x)^2
}
#Dominio Beta
x_dominio_beta <- seq(0,1,length=1000)

u1 <- runif(5000)
u2 <- runif(5000)

a = 0
b = 1
k = 2.0736

# Puntos del ejemplo
U1 <- a + (b-a)*u1
U2 <- k*u2

#Ejemplo sencillo
#U1 <- c(0.80, 6.40, 2.00, 3.20)
#U2 <- c(0.1214, 0.1472, 0.1803, 0.2502)

# Valor de la función
fx <- f_beta(U1)

head(round(data.frame(U2, fx), 5))

# Aceptados / rechazados
aceptado <- U2 <= fx

# Curva
par(mfrow=c(1,1))
plot(x_dominio_beta, f_beta(x_dominio_beta),
     type="l", lwd=3, col="blue", ylim=c(0,2.20),
     xlab="x", ylab="f(x)",
     main="Método de aceptación-rechazo")

# Puntos aceptados
points(U1[aceptado], U2[aceptado],
       pch=19, col="darkgreen", cex=1)

# Puntos rechazados
points(U1[!aceptado], U2[!aceptado],
       pch=19, col="red", cex=1)

#Valores maximos de nuestra generacion de variables
lines(density(U1[aceptado])$x, density(U1[aceptado])$y,
      col = "black",
      lwd = 3)

# Rectángulo de simulación
abline(h=max(f_beta(x_dominio_beta)), lty=4, col="grey", lwd = 4)

legend("topleft",
       legend=c("Beta(4,3) Teorica", "Beta(4,3) Generada",
                "Aceptado",
                "Rechazado"),
       col=c("blue", "black", "darkgreen","red"),
       lwd=c(3, 3, NA, NA),
       pch = c(NA, NA, 19, 19), cex = 0.8)

