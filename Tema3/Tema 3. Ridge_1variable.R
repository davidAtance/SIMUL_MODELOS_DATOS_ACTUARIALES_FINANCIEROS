##############################################################
#Ridge 
##############################################################

#load(file = "Tema2Datos.RData")
load(file = "datos_Ridge.Rdata")

Datos <- datos_Ridge
head(Datos)
summary(Datos)

head(Datos)
# Potencia: Potencia del vehículo (CV)
# Cilindrada: Cilindrada (cc) -> Muy correlacionada con Potencia
# ValorVehiculo: Valor venal del vehículo (€) -> Muy correlacionada con Potencia y Cilindrada
# Edad: Edad del conductor
# AntiguedadCarnet: Antigüedad del carnet
# CosteSiniestralidad: coste de siniestralidad
# MarcaVehiculo: 8 marcas diferentes de vehiculos

#Vamos a ver las variables gráficamente. 
#Variables Numercias
hist(Datos$Potencia, breaks = 30, col = "lightblue", 
     xlab="Potencia")
mean(Datos$Potencia); sd(Datos$Potencia)

hist(Datos$Cilindrada, breaks = 30, col = "lightblue", 
     xlab="Cilindrada")
mean(Datos$Cilindrada); sd(Datos$Cilindrada)

hist(Datos$ValorVehiculo, breaks = 30, col = "lightblue", 
     xlab="Valor Vehiculo")
mean(Datos$ValorVehiculo); sd(Datos$ValorVehiculo)

hist(Datos$Edad, breaks = 30, col = "lightblue", 
     xlab="Edad Asegurado")
mean(Datos$Edad); sd(Datos$Edad)

hist(Datos$AntiguedadCarnet, breaks = 30, col = "lightblue", 
     xlab="Antiguedad Carnet")
mean(Datos$AntiguedadCarnet); sd(Datos$AntiguedadCarnet)

hist(Datos$CosteSiniestralidad, breaks = 30, col = "lightblue", 
     xlab="Coste Siniestralidad")
mean(Datos$CosteSiniestralidad); sd(Datos$CosteSiniestralidad)

#Variable numérica
#MarcaVehiculo
tabla_frec <- table(Datos$MarcaVehiculo)
tabla_pct  <- prop.table(tabla_frec) * 100
print(cbind(Frecuencia = tabla_frec, Porcentaje = round(tabla_pct, 2)))

# Gráfico de frecuencias por Marca
barplot(tabla_pct, col = "darkred", border = "white",
        main = "Distribución Porcentual por Marca de Vehículo",
        ylab = "Porcentaje de Pólizas (%)", 
        las = 2, 
        ylim = c(0, 25))
grid(nx = NA, ny = NULL)

#ver categorias de MarcaVehiculo
unique(Datos$MarcaVehiculo)

#Solo generamos 7 variables dicotómicas, la 8 la elinamos porque esta incluida
Datos$Tesla <- ifelse(Datos$MarcaVehiculo == "Tesla", 1, 0)
Datos$Seat <- ifelse(Datos$MarcaVehiculo == "Seat", 1, 0)
Datos$BMW <- ifelse(Datos$MarcaVehiculo == "BMW", 1, 0)
Datos$Volkswagen <- ifelse(Datos$MarcaVehiculo == "Volkswagen", 1, 0)
Datos$Toyota <- ifelse(Datos$MarcaVehiculo == "Toyota", 1, 0)
Datos$Volvo <- ifelse(Datos$MarcaVehiculo == "Volvo", 1, 0)
Datos$Hyundai <- ifelse(Datos$MarcaVehiculo == "Hyundai", 1, 0)

#Para entender Ridge vamos a montar la obtencion de los parametros 
#de un modelo de OLS por SSE y por Log-Likelihood para luego incorporar
#ridge penalization 

#Modelo uni-variable: 
y <- Datos$CosteSiniestralidad
n <- length(Datos$CosteSiniestralidad)
x1 <- Datos$Edad

#Recordar que intercepto - constante no se penaliza en Ridge: 
#y = beta0 + beta1 * X1 => beta0 = y - beta1*X1
#al centrar la variable tenemos hat_beta0 = hat_y - hat_beta1*X1
#(aunque se podría penalizar el beta0)
x1_centrado <- x1 - mean(x1)
y_centrado  <- y - mean(y)

########################################################
# Definimos la función de coste SSE a minimizar respecto a beta1
f_sse_ols_1var <- function(betas, x, y) {
  x <- as.matrix(x)
  x_c <- scale(x, center = TRUE, scale = FALSE)
  y_c <- y - mean(y)
  
  #lo dejamos montado para varias variables
  residuos <- y_c - x_c %*% betas
  sse <- sum(residuos^2)
  return(sse)
}

# Minimización directamente sobre beta1 mediante optim()
opt_ols_1var <- optim(par = 0, 
                      fn = f_sse_ols_1var, 
                      x = x1, 
                      y = y, method = "BFGS")

beta1_OLS <- opt_ols_1var$par
beta0_OLS <- mean(y) - beta1_OLS * mean(x1)
beta1_OLS
beta0_OLS

######################################################333
#Procedimiento clasico por OLS de forma directa 
modelo1 <- lm(y ~ x1)
summary(modelo1)
coef(modelo1)

beta0_OLS
coef(modelo1)[1]

beta1_OLS
coef(modelo1)[2]

######################################################333
#Recordar que en Ridge añadimos penalizacion sobre los betas; 
#aunque en este caso solo tenemos 1 beta y el intercept (cte)
#min(SSE)_beta1 = Sum((y - (beta0 + beta1*X1))^2)+ lambda(beta1_2)
#Este hecho hace que la función de Log-Likelihood cambie. 

# Definimos la función de coste SSE Penalizado
f_sse_ridge_1var <- function(betas, x, y, lambda) {
  x_c <- scale(x, center = TRUE, scale = FALSE)
  y_c <- y - mean(y)
  
  sse <- sum((y_c - x_c%*%betas)^2)
  penalizacion <- lambda * sum(betas^2)
  return(sse + penalizacion)
}

#Voy a crear una rejilla para probar diferentes valores de 
#lambda y ver cuál obtiene los mejores resultados: 
grid_lambda <- 10^seq(-2, 5, length.out = 100)

#voy a generar una matriz para almacenar los valores: 
sse_ridge <- matrix(NA, nrow = 100, ncol = 4, 
                    dimnames = list(c(1:100), c("b0", "b1", "SSE", "SSE-ridge")))

# Bucle sobre los 100 valores de lambda
i <- 1
for (i in 1:100) {
  if (i %% 10 == 0) {
    cat("Iteración:", i, "de 100\n") #Para medir el tiempo que queda
  }
  lam <- grid_lambda[i] #almaceno el valor de lambda de cada iteracion
  
  # Minimizamos el SSE penalizado numéricamente para el lambda i-ésimo
  opt <- optim(par = 0, fn = f_sse_ridge_1var,
               x = x1, y = y, lambda = lam, 
               method = "BFGS")
  b1 <- opt$par
  b0 <- mean(y) - b1 * mean(x1)
  
  # Guardamos los coeficientes y el SSE no penalizado (medida de ajuste del modelo)
  sse_ridge[i, 1] <- b0
  sse_ridge[i, 2] <- b1
  sse_ridge[i, 3] <- sum((y - (b0 + b1 * x1))^2)
  sse_ridge[i, 4] <- f_sse_ridge_1var(betas = b1, 
                                      x = x1, y = y, lambda = lam)
  
}

# Logaritmo de lambda para los ejes X para que se vea mejor la distancia
log_lambda_vec <- log(grid_lambda)

# ==============================================================================
# VISUALIZACIÓN GRÁFICA
# ==============================================================================
par(mfrow = c(1, 2))

# Gráfico 1: Evolución del Intercepto (beta0) frente a Log(Lambda)
plot(log_lambda_vec, sse_ridge[, 2], type = "l", col = "darkblue", lwd = 2.5,
     xlab = expression(log(lambda)), ylab = expression(hat(beta)[1]),
     main = expression("Evolución de " * hat(beta)[1] * " vs. " * log(lambda)))
grid()
#La penalización cuadrática penaliza los coeficientes grandes. 
#Cuando \lambda tiende a infinito, la minimización del error pierde peso 
#frente a la penalización, obligando a que beta1 tienda a 0.


# Gráfico 2: Evolución del SSE frente a Log(Lambda)
plot(log_lambda_vec, sse_ridge[, 4], type = "l", col = "darkred", lwd = 2.5,
     xlab = expression(log(lambda)), ylab = "SSE",
     main = expression("SSE vs. " * log(lambda)))
grid()

#A mayor valor de $\lambda$, peor es el ajuste dentro de la muestra 
#(SSE más alto), pero esto permite prevenir el sobreajuste (overfitting) 
#fuera de muestra.


# ==============================================================================
# PASO ADICIONAL: Comparación de la Siniestralidad Promedio por Tramos de Edad
# ==============================================================================
edades <- sort(unique(round(Datos$Edad,0)))
max(edades)
min(edades)
edades2 <- edades
edades <- c(edades, 80)
coste_real <- matrix(NA, nrow = length(edades)-1, ncol = 1)
coste_OLS <- matrix(NA, nrow = length(edades)-1, ncol = 1)

Datos$hat_OLS <- modelo1$fitted.values

i <- 2
for(i in 1:(length(edades)-1)){
  coste_real[i,] <- mean(Datos[Datos$Edad > edades[i] &
                                 Datos$Edad < edades[(i+1)], ]$CosteSiniestralidad, na.rm = T)
  coste_OLS[i,] <- mean(Datos[Datos$Edad > edades[i] & 
                                Datos$Edad < edades[(i+1)], ]$hat_OLS, na.rm = T)
}

par(mfrow = c(1,1))
max1 <- max(coste_real, coste_OLS)
min1 <- min(coste_real, coste_OLS)
plot(edades2, coste_real, xlab="edades", ylab = "Prima", 
     type ="p", pch = 19, col = "blue")
lines(edades2, coste_OLS, col = "red", lwd = 2)

#Y ahora vamos a ver como se comportarían los valores de lambda: 

par(mfrow = c(1,1))
max1 <- max(coste_real, coste_OLS)
min1 <- min(coste_real, coste_OLS)
plot(edades2, coste_real, xlab="edades", ylab = "Prima", 
     type ="p", pch = 19, col = "blue")
lines(edades2, coste_OLS, col = "red", lwd = 2)

i <- lam <- 1
coste_lambda <- matrix(NA, nrow = length(edades2), ncol = 1)
for(lam in 1:length(grid_lambda)){
  if (lam %% 10 == 0) {
    cat("Iteración:", lam, "de 100\n") #Para medir el tiempo que queda
  }
  Datos$lambda <- sse_ridge[lam, 1] + sse_ridge[lam, 2]*x1
  
  for(i in 1:length(edades2)){  
    coste_lambda[i,] <- mean(Datos[Datos$Edad > edades[i] & 
                                     Datos$Edad < edades[i+1], ]$lambda, na.rm = T)
  }
  lines(edades2, coste_lambda, col = "red4", lwd = 1)
}  

#Existe poca diferencia porque solo tenemos un valor 
#de beta_1 y al haber uno los valores son muy parecidos: 
sse_ridge[,2]

##############################################################################
# Ridge por Log-Verosimilitud (Log-Likelihood) - 
##############################################################################
f_loglik <- function(params, x, y) {
  x <- as.matrix(x)
  p <- ncol(x)
  
  betas <- params[1:p]
  sigma <- params[p+1]
  
  # Control para evitar valores no válidos de la desviación típica
  if (sigma <= 0) return(1e10)
  
  # Centrado idéntico a f_sse_ridge_1var
  x_c <- scale(x, center = TRUE, scale = FALSE)
  y_c <- y - mean(y)
  mu <- x_c %*% betas
  
  # 1. Log-Verosimilitud Negativa Gaussiana pura (sin penalizar)
  # -log L = (n/2)*log(2*pi*sigma^2) + SSE / (2*sigma^2)
  nll <- -sum(dnorm(y_c, mean = mu, sd = sigma, log = TRUE))
  
  # 2. Penalización Ridge L2 sobre betas escalada por la varianza sigma^2
  
  return(nll)
}

lam <- 10
opt_mle <- optim(par = c(0, sd(y)),
                 fn = f_loglik, 
                 x = x1, y = y, 
                 method = "BFGS")

# Extracción de parámetros óptimos
beta1_mle_opt <- opt_mle$par[1]
sigma_mle_opt <- opt_mle$par[2]
beta0_mle_opt <- mean(y) - beta1_mle_opt * mean(x1)

modelo1$coef[1]
beta0_mle_opt

modelo1$coef[2]
beta1_mle_opt
#Los resultados tienen que coincidir con SSE


#############################################################
#Ridge - OLS 
#############################################################
# Definimos la función de coste de Log-Verosimilitud Negativa Penalizada
f_loglik_ridge <- function(params, x, y, lambda) {
  x <- as.matrix(x)
  p <- ncol(x)
  
  betas <- params[1:p]
  sigma <- params[p+1]
  
  # Control para evitar valores no válidos de la desviación típica
  if (sigma <= 0) return(1e10)
  
  # Centrado idéntico a f_sse_ridge_1var
  x_c <- scale(x, center = TRUE, scale = FALSE)
  y_c <- y - mean(y)
  mu <- x_c %*% betas
  
  # 1. Log-Verosimilitud Negativa Gaussiana pura (sin penalizar)
  # -log L = (n/2)*log(2*pi*sigma^2) + SSE / (2*sigma^2)
  nll <- -sum(dnorm(y_c, mean = mu, sd = sigma, log = TRUE))
  
  # 2. Penalización Ridge L2 sobre beta1 escalada por la varianza sigma^2
  penalizacion <- (lambda / (2 * sigma^2)) * sum(betas^2)
  
  return(nll + penalizacion)
}

# ----------------------------------------------------------------------------
# Minimización numérica directa sobre params = c(beta1, sigma)
# ----------------------------------------------------------------------------
#Primero asignamos un valor lambda
lam <- 10
opt_ridge_mle <- optim(par = c(0, sd(y)), 
                       fn = f_loglik_ridge, 
                       x = x1, y = y, 
                       lambda = lam, 
                       method = "BFGS")

# Extracción de parámetros óptimos
beta1_mle_opt <- opt_ridge_mle$par[1]
sigma_mle_opt <- opt_ridge_mle$par[2]
beta0_mle_opt <- mean(y) - beta1_mle_opt * mean(x1)

#Ahora ya no van a coincidir con el modelo 1 
modelo1$coef[1]
beta0_mle_opt

modelo1$coef[2]
beta1_mle_opt


#Ahora vamos a ver la rejilla de valores de lambda 
#lambda
grid_lambda <- 10^seq(-2, 5, length.out = 100)
mle_ridge <- matrix(NA, nrow = 100, ncol = 4, 
                    dimnames = list(1:100, c("b0", "b1", "sigma", "NegLogLik")))

for (i in 1:100) {
  if (i %% 10 == 0) {
    cat("Iteración LogLik:", i, "de 100\n")
  }
  lam <- grid_lambda[i]
  
  # Optimización numérica de la log-verosimilitud penalizada
  opt <- optim(
    par = c(0, sd(y)), 
    fn = f_loglik_ridge, 
    x = x1, 
    y = y, 
    lambda = lam, 
    method = "BFGS"
  )
  
  b1 <- opt$par[1]
  sig <- opt$par[2]
  b0 <- mean(y) - b1 * mean(x1)
  
  mle_ridge[i, 1] <- b0
  mle_ridge[i, 2] <- b1
  mle_ridge[i, 3] <- sig
  mle_ridge[i, 4] <- opt$value # Log-Verosimilitud Negativa Penalizada
}


#ahora vamos a verlo gráficamente 
par(mfrow = c(1,1))
max1 <- max(coste_real, coste_OLS)
min1 <- min(coste_real, coste_OLS)
plot(edades2, coste_real, xlab="edades", ylab = "Prima", 
     type ="p", pch = 19, col = "blue")
lines(edades2, coste_OLS, col = "red", lwd = 2)

#Ahora incorporamos la estimación con ridge
i <- lam <- 1
coste_ridge <- matrix(NA, nrow = length(edades2), ncol = 1)
for(lam in 1:length(grid_lambda)){
  if (lam %% 10 == 0) {
    cat("Iteración:", lam, "de 100\n") #Para medir el tiempo que queda
  }
  Datos$ridge <- mle_ridge[lam, 1] + mle_ridge[lam, 2]*x1
  
  for(i in 1:length(edades2)){  
    coste_ridge[i,] <- mean(Datos[Datos$Edad > edades[i] & 
                                     Datos$Edad < edades[i+1], ]$ridge, na.rm = T)
  }
  lines(edades2, coste_ridge, col = "red4", lwd = 1)
}  

