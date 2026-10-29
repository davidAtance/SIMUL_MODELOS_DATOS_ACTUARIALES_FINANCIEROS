##############################################################
#ELASTIC - NET
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

#Para entender Elastic-Net vamos a montar la obtencion de los parametros 
#de un modelo de OLS por SSE y por Log-Likelihood para luego incorporar
#Elastic-Net penalization 

#Modelo uni-variable: 
y <- Datos$CosteSiniestralidad
n <- length(Datos$CosteSiniestralidad)
x1 <- Datos$Edad

#Recordar que intercepto - constante no se penaliza en Elastic-Net: 
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
#Recordar que en Elastic-Net añadimos penalizacion sobre los betas; 
#aunque en este caso solo tenemos 1 beta y el intercept (cte)
#min(SSE)_beta1 = Sum((y - (beta0 + beta1*X1))^2) + lambda[alpha abs(beta) + (1-alpha) sum(beta^2)]
#Este hecho hace que la función de Log-Likelihood cambie. 

# Definimos la función de coste SSE Penalizado
f_sse_elasticnet <- function(betas, x, y, lambda, alpha) {
  x_c <- scale(x, center = TRUE, scale = FALSE)
  y_c <- y - mean(y)
  
  sse <- sum((y_c - x_c%*%betas)^2)
  penalizacion <- lambda * (alpha * sum(abs(betas)) +
                              (1 - alpha) * sum(betas^2)
  )
  return(sse + penalizacion)
}

#Voy a crear una rejilla para probar diferentes valores de 
#lambda y ver cuál obtiene los mejores resultados: 
grid_lambda <- 10^seq(-2, 5, length.out = 100)
#Vamos a dar una rejilla pequeña de valores de alpha, 
#el cuál también podemos optimizar, pero no lo incluímos para 
#mantener valores interpretables dentro y fuera de muestra
grid_alpha <- c(0, 0.25, 0.5, 0.75, 1) 

#voy a generar una matriz para almacenar los valores: 
elastic_sse <- matrix(NA,
                      nrow = length(grid_alpha) * length(grid_lambda),
                      ncol = 6, 
                      dimnames = list(c(1:(length(grid_lambda)*length(grid_alpha))), 
                                      c("alpha", "lambda", "b0", "b1", "SSE", "SSE-pen")))
# Bucle sobre los 100 valores de lambda
i <- 1
# Inicializamos el contador global de iteraciones
iter_count <- 0
total_iter <- length(grid_alpha) * length(grid_lambda) # p. ej. 5 * 100 = 500

k <- 1
for(a in 1:length(grid_alpha)){
  
  alpha <- grid_alpha[a]
  
  for (i in 1:length(grid_lambda)) {
    # Incrementamos el contador global en cada paso de lambda
    iter_count <- iter_count + 1
    
    # Mostramos el progreso cada 10 iteraciones globales
    if (iter_count %% 10 == 0) {
      cat("Iteración:", iter_count, "de", total_iter, "\n") # Mide el tiempo restante
    }
    
    lam <- grid_lambda[i]
    
    # Minimizamos el SSE penalizado numéricamente para el lambda i-ésimo
    opt <- optim(par = 0, fn = f_sse_elasticnet,
                 x = x1, y = y, lambda = lam, alpha = alpha,
                 method = "BFGS")
    b1 <- opt$par
    b0 <- mean(y) - b1 * mean(x1)
    
    # Guardamos los coeficientes y el SSE no penalizado (medida de ajuste del modelo)
    elastic_sse[k, ] <- c(alpha, lam, 
                          b0, b1, 
                          sum((y - (b0 + b1*x1))^2),
                          opt$value)
    
    k <- k + 1
  }}


#Ahora vamos a ver como varia el coeficiente según los valores de 
#alpha
cols <- c("red","blue","green","orange","black")
df.elastic_sse <- as.data.frame(elastic_sse)

plot(NA, xlim = range(log(grid_lambda)),
     ylim = range(df.elastic_sse$b1),
     xlab = "log(lambda)", ylab = expression(hat(beta)[1]))

for(a in 1:length(grid_alpha)){
  tmp <- df.elastic_sse[df.elastic_sse$alpha == grid_alpha[a], ]
  
  lines(log(tmp$lambda), tmp$b1, col = cols[a],
    lwd = 2)
  }

legend("topleft", legend = paste("alpha =", grid_alpha),
  col = cols, lwd = 2, cex = 0.8)


# Logaritmo de lambda para los ejes X para que se vea mejor la distancia
log_lambda_vec <- log(grid_lambda)

# ==============================================================================
# VISUALIZACIÓN GRÁFICA
# ==============================================================================
#Vamos a ver gráficamente como cambia según valor de alpha y lambda

par(mfrow = c(1, 2))

# Gráfico 1: Evolución del Intercepto (beta0) frente a Log(Lambda)
plot(NA, xlim = range(log_lambda_vec), 
     ylim = range(df.elastic_sse$b1), 
     xlab = expression(log(lambda)), ylab = expression(hat(beta)[1]),
     main = expression("Evolución de " * hat(beta)[1] * " vs. " * log(lambda)))
for(a in 1:length(grid_alpha)){
  tmp <- df.elastic_sse[df.elastic_sse$alpha == grid_alpha[a], ]
  
  lines(log_lambda_vec, tmp$b1, col = cols[a], lwd = 2)
}

legend("bottomleft", legend = paste("alpha =", grid_alpha),
       col = cols, lwd = 2, cex = 0.8)

#A medida que aumenta λ, la penalización tiene mayor influencia sobre
#los coeficientes del modelo. En el caso univariable, todos los valores
#de α producen trayectorias muy similares, aunque la intensidad de la
#regularización varía según el peso asignado a las componentes Ridge y Lasso.
#
#Cuando λ es pequeño, las estimaciones prácticamente coinciden con OLS.
#Para valores elevados de λ, los coeficientes comienzan a contraerse,
#siendo más acusado el efecto para algunos valores de α.

# Gráfico 2: Evolución del SSE frente a Log(Lambda)
plot(NA, xlim = range(log_lambda_vec), 
     ylim = range(df.elastic_sse$SSE), 
     xlab = expression(log(lambda)), ylab = "SSE",
     main = expression("Evolución de SSE vs. " * log(lambda)))
for(a in 1:length(grid_alpha)){
  tmp <- df.elastic_sse[df.elastic_sse$alpha == grid_alpha[a], ]
  
  lines(log_lambda_vec, tmp$SSE, col = cols[a], lwd = 2)
}

legend("topleft", legend = paste("alpha =", grid_alpha),
       col = cols, lwd = 2, cex = 0.8)

#A mayor valor de $\lambda$, peor es el ajuste dentro de la muestra 
#(SSE más alto), pero esto permite prevenir el sobreajuste (overfitting) 
#fuera de muestra.

#Para valores pequeños de λ, todas las configuraciones de Elastic Net
#presentan prácticamente el mismo ajuste que OLS. Sin embargo, cuando
#λ aumenta, la regularización gana importancia y el SSE comienza a crecer.
#
#En este ejemplo univariable, las diferencias entre valores de α son
#reducidas, lo que sugiere que la principal utilidad de Elastic Net
#aparece en modelos con múltiples variables correlacionadas.

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
coste_EN <- matrix(NA, nrow = length(edades)-1, ncol = 1)

Datos$hat_OLS <- modelo1$fitted.values

#Vamos a buscar la mejor combinación de EN (alpha y lambda)
which.min(df.elastic_sse$`SSE-pen`)

df.elastic_sse[401,]
modelo1$coef


Datos$hat_EN <- df.elastic_sse[401, 3] + df.elastic_sse[401, 4]*Datos$Edad

i <- 2
for(i in 1:(length(edades)-1)){
  coste_real[i,] <- mean(Datos[Datos$Edad > edades[i] &
                                 Datos$Edad < edades[(i+1)], ]$CosteSiniestralidad, na.rm = T)
  coste_OLS[i,] <- mean(Datos[Datos$Edad > edades[i] & 
                                Datos$Edad < edades[(i+1)], ]$hat_OLS, na.rm = T)
  coste_EN[i,] <- mean(Datos[Datos$Edad > edades[i] & 
                                Datos$Edad < edades[(i+1)], ]$hat_EN, na.rm = T)
  
}

par(mfrow = c(1,1))
max1 <- max(coste_real, coste_OLS, coste_EN)
min1 <- min(coste_real, coste_OLS, coste_EN)
plot(edades2, coste_real, xlab="edades", ylab = "Prima", 
     type ="p", pch = 19, col = "blue")
lines(edades2, coste_OLS, col = "red", lwd = 2)
lines(edades2, coste_EN, col = "green", lwd = 2)


##############################################################################
# Elastic Net por Log-Verosimilitud (Log-Likelihood)
##############################################################################
f_loglik <- function(params, x, y) {
  x <- as.matrix(x)
  p <- ncol(x)
  
  betas <- params[1:p]
  sigma <- params[p+1]
  
  # Control para evitar valores no válidos de la desviación típica
  if (sigma <= 0) return(1e10)
  
  # Centrado idéntico a f_sse_elasticnet_1var
  x_c <- scale(x, center = TRUE, scale = FALSE)
  y_c <- y - mean(y)
  mu <- x_c %*% betas
  
  # 1. Log-Verosimilitud Negativa Gaussiana pura (sin penalizar)
  # -log L = (n/2)*log(2*pi*sigma^2) + SSE / (2*sigma^2)
  nll <- -sum(dnorm(y_c, mean = mu, sd = sigma, log = TRUE))
  
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
#Elastic-Net - OLS 
#############################################################
# Definimos la función de coste de Log-Verosimilitud Negativa Penalizada
f_loglik_elasticnet <- function(params, x, y, lambda, alpha) {
  x <- as.matrix(x)
  p <- ncol(x)
  
  betas <- params[1:p]
  sigma <- params[p+1]
  
  # Control para evitar valores no válidos de la desviación típica
  if (sigma <= 0) return(1e10)
  
  # Centrado idéntico a f_sse_elasticnet_1var
  x_c <- scale(x, center = TRUE, scale = FALSE)
  y_c <- y - mean(y)
  mu <- x_c %*% betas
  
  # 1. Log-Verosimilitud Negativa Gaussiana pura (sin penalizar)
  # -log L = (n/2)*log(2*pi*sigma^2) + SSE / (2*sigma^2)
  nll <- -sum(dnorm(y_c, mean = mu, sd = sigma, log = TRUE))
  
  # 2. Penalización Elastic-Net L2 sobre beta1 escalada por la varianza sigma^2
  penalizacion <- (lambda/(2*sigma^2)) * (alpha * sum(abs(betas))
                                          + (1-alpha) * sum(betas^2)
  )
  return(nll + penalizacion)
}

# ----------------------------------------------------------------------------
# Minimización numérica directa sobre params = c(beta1, sigma)
# ----------------------------------------------------------------------------
#Primero asignamos un valor lambda
lam <- 10
alpha <- 0.5
opt_elasticnet_mle <- optim(par = c(0, sd(y)), 
                       fn = f_loglik_elasticnet, 
                       x = x1, y = y, 
                       lambda = lam, alpha = alpha,
                       method = "BFGS")

# Extracción de parámetros óptimos
beta1_mle_opt <- opt_elasticnet_mle$par[1]
sigma_mle_opt <- opt_elasticnet_mle$par[2]
beta0_mle_opt <- mean(y) - beta1_mle_opt * mean(x1)

#Ahora ya no van a coincidir con el modelo 1 ya que lm -- OLS
modelo1$coef[1]
beta0_mle_opt

modelo1$coef[2]
beta1_mle_opt


#Ahora vamos a ver la rejilla de valores de lambda y alpha
#lambda
grid_lambda <- 10^seq(-2, 5, length.out = 100)
grid_alpha <- c(0, 0.25, 0.5, 0.75, 1)
elastic_ll <- matrix(NA,
                      nrow = length(grid_alpha) * length(grid_lambda),
                      ncol = 7, 
                      dimnames = list(c(1:(length(grid_lambda)*length(grid_alpha))), 
                                      c("alpha", "lambda", "b0", "b1", "sigma", "SSE", "LL-pen")))
# Bucle sobre los 100 valores de lambda
i <- 1
# Inicializamos el contador global de iteraciones

total_iter <- length(grid_alpha) * length(grid_lambda) # p. ej. 5 * 100 = 500

k <- 1; iter_count <- 0
a <- i <- 1 
for(a in 1:length(grid_alpha)){
  
  alpha <- grid_alpha[a]
  
  for (i in 1:length(grid_lambda)) {
    # Incrementamos el contador global en cada paso de lambda
    iter_count <- iter_count + 1
    
    # Mostramos el progreso cada 10 iteraciones globales
    if (iter_count %% 10 == 0) {
      cat("Iteración:", iter_count, "de", total_iter, "\n") # Mide el tiempo restante
    }
    lam <- grid_lambda[i]
    
    # Optimización numérica de la log-verosimilitud penalizada
    opt <- optim(par = c(0, sd(y)), 
                 fn = f_loglik_elasticnet, 
                 x = x1, y = y, 
                 lambda = lam, alpha = alpha,
                 method = "BFGS")
    
    b1 <- opt$par[1]
    sig <- opt$par[2]
    b0 <- mean(y) - b1 * mean(x1)
    
    elastic_ll[k, ] <- c(alpha, lam, 
                        b0, b1, sig, 
                        sum((y - (b0 + b1*x1))^2),
                        opt$value)
    k <- k + 1
    }}

df.elastic_ll <- as.data.frame(elastic_ll)
which.min(df.elastic_ll$SSE)
which.min(df.elastic_ll$`LL-pen`)

df.elastic_ll[401, ]
modelo1$coef

# ==============================================================================
# VISUALIZACIÓN GRÁFICA
# ==============================================================================
#Vamos a ver gráficamente como cambia según valor de alpha y lambda

par(mfrow = c(1, 2))

# Gráfico 1: Evolución del Intercepto (beta0) frente a Log(Lambda)
plot(NA, xlim = range(log_lambda_vec), 
     ylim = range(df.elastic_ll$b1), 
     xlab = expression(log(lambda)), ylab = expression(hat(beta)[1]),
     main = expression("Evolución de " * hat(beta)[1] * " vs. " * log(lambda)))
for(a in 1:length(grid_alpha)){
  tmp <- df.elastic_ll[df.elastic_ll$alpha == grid_alpha[a], ]
  
  lines(log_lambda_vec, tmp$b1, col = cols[a], lwd = 2)
}

legend("topleft", legend = paste("alpha =", grid_alpha),
       col = cols, lwd = 2, cex = 0.8)

#A medida que aumenta λ, la penalización tiene mayor influencia sobre
#los coeficientes del modelo. En el caso univariable, todos los valores
#de α producen trayectorias muy similares, aunque la intensidad de la
#regularización varía según el peso asignado a las componentes Ridge y Lasso.
#
#Cuando λ es pequeño, las estimaciones prácticamente coinciden con OLS.
#Para valores elevados de λ, los coeficientes comienzan a contraerse,
#siendo más acusado el efecto para algunos valores de α.

# Gráfico 2: Evolución del SSE frente a Log(Lambda)
plot(NA, xlim = range(log_lambda_vec), 
     ylim = range(df.elastic_ll$SSE), 
     xlab = expression(log(lambda)), ylab = "SSE",
     main = expression("Evolución de SSE vs. " * log(lambda)))
for(a in 1:length(grid_alpha)){
  tmp <- df.elastic_ll[df.elastic_ll$alpha == grid_alpha[a], ]
  
  lines(log_lambda_vec, tmp$SSE, col = cols[a], lwd = 2)
}

legend("topleft", legend = paste("alpha =", grid_alpha),
       col = cols, lwd = 2, cex = 0.8)

#A mayor valor de $\lambda$, peor es el ajuste dentro de la muestra 
#(SSE más alto), pero esto permite prevenir el sobreajuste (overfitting) 
#fuera de muestra.

#Para valores pequeños de λ, todas las configuraciones de Elastic Net
#presentan prácticamente el mismo ajuste que OLS. Sin embargo, cuando
#λ aumenta, la regularización gana importancia y el SSE comienza a crecer.
#
#En este ejemplo univariable, las diferencias entre valores de α son
#reducidas, lo que sugiere que la principal utilidad de Elastic Net
#aparece en modelos con múltiples variables correlacionadas.


