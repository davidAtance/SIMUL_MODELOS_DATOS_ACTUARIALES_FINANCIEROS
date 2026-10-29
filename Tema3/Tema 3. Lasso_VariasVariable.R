##############################################################
#Ridge 
##############################################################
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


###############################################################
#Recordar que lo que nos interesa es ver como funcionan fuera de muestra
#Por ello, vamos a dividir la muestra en dos partes: 
y <- Datos$CosteSiniestralidad
names(Datos)

#vamos a eliminar la variable "MarcaVehiculo"
cols_excluir <- c("MarcaVehiculo")
X <- Datos[, !names(Datos) %in% cols_excluir]
colnames(X)

#Ahora hacemos división de los datos 
set.seed(301026)
train_idx <- createDataPartition(y, p = 0.80, list = FALSE)

#Training
X_train <- X[train_idx, ]

#Testing 
X_test <- X[-train_idx, ]

#Recordamos entrenamos el modelos y comprobamos como funciona 
#dentro de muestra para luego testarlo fuera de muestra.

#####################################################
#Modelo normal OLS 
modelo1 <- lm(CosteSiniestralidad ~ ., data = X)
summary(modelo1)

modelo1 <- lm(CosteSiniestralidad ~ Potencia + 
                Cilindrada + Edad + AntiguedadCarnet, 
              data = X_train)
summary(modelo1)

#Test dentro de muestra 
residuos <- X_train$CosteSiniestralidad - modelo1$fitted.values
#MAPE = 100* 1/n sum(abs((y_real - y_hat)/y_real ))
mape_ols_in <- 100* mean(abs(residuos/X_train$CosteSiniestralidad))

#Test fuera de muestra 
colchosen <- c("Potencia", "Cilindrada", "Edad", "AntiguedadCarnet")
names(X_test)
out <- X_test[, names(X_test) %in% colchosen]

yhat_out <- predict(modelo1, newdata = out)
residuos_out <- X_test$CosteSiniestralidad - yhat_out
#MAPE = 100* 1/n sum(abs((y_real - y_hat)/y_real ))
mape_ols_out <- 100* mean(abs(residuos_out/X_test$CosteSiniestralidad))

########################################################
#Vale ahora una vez montado el modelo1 con LINEAR REGRESSION 
#Vamos con el Lasso 
# Definimos la función de coste SSE Penalizado 
#(la que hemos generado antes)
f_sse_lasso <- function(betas, x, y, lambda) {
  x <- as.matrix(x)
  
  x_c <- scale(x, center = TRUE, scale = TRUE)
  y_c <- y - mean(y)
  
  sse <- sum((y_c - x_c%*%betas)^2)
  penalizacion <- lambda * sum(abs(betas))
  return(sse + penalizacion)
}


#Voy a crear una rejilla para probar diferentes valores de 
#lambda y ver cuál obtiene los mejores resultados: 
grid_lambda <- 10^seq(-2, 5, length.out = 100)
#Lo vamos hacer para dentro de muestra y fuera de muestra 
#y eligiremos el lambda basado en ambas cosas 

#Defino el x_train quitando los valores de Y
x_train <- X[, !names(X) %in% "CosteSiniestralidad"]
names(x_train)

#voy a generar una matriz para almacenar los valores: 
mape_lasso <- list(betas = matrix(NA, nrow = 100, ncol = 13, 
                                  dimnames = list(c(1:100), 
                                                  c("b0", names(x_train)))), 
                   errores = matrix(NA, nrow = 100, ncol = 4, 
                                    dimnames = list(c(1:100), 
                                                    c("SSE-in-lasso", "SSE-in", "MAPE-in", "MAPE-out"))))



#Genero una serie de cosas que voy a usar dentro:
y_train <- X_train$CosteSiniestralidad
x_train <- X_train[, !names(X_train) %in%
                     "CosteSiniestralidad"]

X_mat <- as.matrix(x_train)

p_vars <- ncol(X_mat) # Número de variables (p. ej. 12)
x_mean <- colMeans(X_mat)
x_sd   <- apply(X_mat, 2, sd)
# Bucle sobre los 100 valores de lambda
i <- 1
for (i in 1:100) {
  if (i %% 10 == 0) {
    cat("Iteración:", i, "de 100\n") #Para medir el tiempo que queda
  }
  lam <- grid_lambda[i] #almaceno el valor de lambda de cada iteracion
  
  # Minimizamos el SSE penalizado numéricamente para el lambda i-ésimo
  
  opt <- optim(par = rep(0, p_vars), fn = f_sse_lasso,
               x = X_mat, y = y_train, lambda = lam, 
               method = "BFGS")
  # Nota:
  # En la práctica glmnet utiliza
  # coordinate descent debido a la
  # no diferenciabilidad de |beta| en 0.
  # recomendado con lasso -- method = "Nelder-Mead"
  
  beta_std <- opt$par[1:p_vars]
  beta_orig <- beta_std / x_sd
  beta0_orig <- mean(y_train) - sum(beta_orig * x_mean)
  
  # Guardamos los coeficientes 
  mape_lasso$betas[i, 1] <- beta0_orig
  mape_lasso$betas[i, 2:13] <- beta_orig
  
  #y el SSE no penalizado (medida de ajuste del modelo)
  yhat_in <- beta0_orig + X_mat %*% beta_orig 
  
  X_test2 <- X_test[, !names(X_test) %in% "CosteSiniestralidad"] 
  X_test2 <- as.matrix(X_test2)
  yhat_out <- beta0_orig + X_test2 %*% beta_orig
  
  resid_in <- (y_train - yhat_in)
  resid_out <- (X_test$CosteSiniestralidad - yhat_out)
  
  mape_lasso$errores[i, 1] <- opt$value
  mape_lasso$errores[i, 2] <- sum((y_train - yhat_in)^2)
  mape_lasso$errores[i, 3] <- 100* mean(abs(resid_in/y_train))
  mape_lasso$errores[i, 4] <- 100* mean(abs(resid_out/X_test$CosteSiniestralidad))
  
}

# Logaritmo de lambda para los ejes X para que se vea mejor la distancia
log_lambda_vec <- log(grid_lambda)

which.min(mape_lasso$errores[, 1])
which.min(mape_lasso$errores[, 2])
which.min(mape_lasso$errores[, 3])
which.min(mape_lasso$errores[, 4])

head(mape_lasso$betas)
tail(mape_lasso$betas)

#Visualización de los resultados
matplot(
  log(grid_lambda),
  abs(mape_lasso$betas[, -1]),
  type="l", lwd = 2,
  lty=1, xlab = "Log-lambda", ylab = "betas")
#Las líneas discontinuas corresponden a los coeficientes 
#obtenidos en el modelo OLS final tras el proceso de selección
#de variables.
abline(h = abs(coef(modelo1)), lty = 2,
       col = 1:length(coef(modelo1)))
abline(v = log(grid_lambda[53]),
       col = "red", lwd = 3, lty = 2)


matplot(
  mape_lasso$errores[, 3],
  abs(mape_lasso$betas[, -1]),
  type="l", lwd = 2,
  lty=1, xlab = "MAPE-in", ylab = "betas")

matplot(
  mape_lasso$errores[, 4],
  abs(mape_lasso$betas[, -1]),
  type="l", lwd = 2,
  lty=1, xlab = "MAPE-out", ylab = "betas")

matplot(
  log(grid_lambda),
  mape_lasso$errores[, 4],
  type="l", lwd = 2,
  lty=1, xlab = "log-lambda", ylab = "MAPE-out")
abline(v=log(grid_lambda[71]),
       col="blue", lty=2, lwd=2)
points(log(grid_lambda[71]), min(mape_lasso$errores[,4]),
       pch=19, col="blue")


#Ahora vamos a ver los valores finales y los 
#vamos a comparar con OLS 
mape_lassoSSE_in <- mape_lasso$errores[71, 3]
mape_lassoSSE_out <- mape_lasso$errores[71, 4]

mape_ols_in
mape_lassoSSE_in

mape_ols_out 
mape_lassoSSE_out



#############################################################
#Lasso - OLS 
#############################################################
# Definimos la función de coste de Log-Verosimilitud Negativa Penalizada
f_loglik_lasso <- function(params, x, y, lambda) {
  x <- as.matrix(x)
  p <- ncol(x)
  
  betas <- params[1:p]
  sigma <- params[p+1]
  
  # Control para evitar valores no válidos de la desviación típica
  if (sigma <= 0) return(1e10)
  
  # Centrado idéntico a f_sse_ridge_1var
  x_c <- scale(x, center = TRUE, scale = TRUE)
  y_c <- y - mean(y)
  mu <- x_c %*% betas
  
  # 1. Log-Verosimilitud Negativa Gaussiana pura (sin penalizar)
  # -log L = (n/2)*log(2*pi*sigma^2) + SSE / (2*sigma^2)
  nll <- -sum(dnorm(y_c, mean = mu, sd = sigma, log = TRUE))
  
  # 2. Penalización Ridge L2 sobre beta1 escalada por la varianza sigma^2
  penalizacion <- (lambda / (2 * sigma^2)) * sum(abs(betas))
  
  return(nll + penalizacion)
}

#voy a generar una matriz para almacenar los valores: 
mape_lasso_LL <- list(betas = matrix(NA, nrow = 100, ncol = 13, 
                                  dimnames = list(c(1:100), 
                                                  c("b0", names(x_train)))), 
                   errores = matrix(NA, nrow = 100, ncol = 4, 
                                    dimnames = list(c(1:100), 
                                                    c("LL-in-lasso", "SSE-in", "MAPE-in", "MAPE-out"))))



#Genero una serie de cosas que voy a usar dentro: 
y_train <- X_train$CosteSiniestralidad

x_train <- X_train[, !names(X_train) %in%
                     "CosteSiniestralidad"]

X_mat <- as.matrix(x_train)
p_vars <- ncol(X_mat) # Número de variables (p. ej. 12)
x_mean <- colMeans(X_mat)
x_sd   <- apply(X_mat, 2, sd)
X_test2 <- X_test[, !names(X_test) %in% "CosteSiniestralidad"] 
X_test2 <- as.matrix(X_test2)

# Bucle sobre los 100 valores de lambda
i <- 1
for (i in 1:100) {
  if (i %% 10 == 0) {
    cat("Iteración:", i, "de 100\n") #Para medir el tiempo que queda
  }
  lam <- grid_lambda[i] #almaceno el valor de lambda de cada iteracion
  
  # Minimizamos el SSE penalizado numéricamente para el lambda i-ésimo
  opt <- optim(par = c(rep(0, p_vars), sd(y)), 
               fn = f_loglik_lasso,
               x = X_mat, y = y_train, lambda = lam, 
               method = "BFGS")
  
  beta_std <- opt$par[1:p_vars]
  beta_orig <- beta_std / x_sd
  beta0_orig <- mean(y_train) - sum(beta_orig * x_mean)
  
  # Guardamos los coeficientes 
  mape_lasso_LL$betas[i, 1] <- beta0_orig
  mape_lasso_LL$betas[i, 2:13] <- beta_orig
  
  #y el SSE no penalizado (medida de ajuste del modelo)
  yhat_in <- beta0_orig + X_mat %*% beta_orig 
  
  yhat_out <- beta0_orig + X_test2 %*% beta_orig
  
  resid_in <- (y_train - yhat_in)
  resid_out <- (X_test$CosteSiniestralidad - yhat_out)
  
  mape_lasso_LL$errores[i, 1] <- opt$value
  mape_lasso_LL$errores[i, 2] <- sum((y_train - yhat_in)^2)
  mape_lasso_LL$errores[i, 3] <- 100* mean(abs(resid_in/y_train))
  mape_lasso_LL$errores[i, 4] <- 100* mean(abs(resid_out/X_test$CosteSiniestralidad))
  
}

# Logaritmo de lambda para los ejes X para que se vea mejor la distancia
log_lambda_vec <- log(grid_lambda)

which.min(mape_lasso_LL$errores[, 1])
which.min(mape_lasso_LL$errores[, 2])
which.min(mape_lasso_LL$errores[, 3])
which.min(mape_lasso_LL$errores[, 4])

head(mape_lasso_LL$betas)
tail(mape_lasso_LL$betas)

#Visualización de los resultados
matplot(
  log(grid_lambda),
  abs(mape_lasso_LL$betas[, -1]),
  type="l", lwd = 2,
  lty=1, xlab = "Log-lambda", ylab = "betas")
#Las líneas discontinuas corresponden a los coeficientes 
#obtenidos en el modelo OLS final tras el proceso de selección
#de variables.
abline(h = abs(coef(modelo1)), lty = 2,
       col = 1:length(coef(modelo1)))
abline(v = log(grid_lambda[51]),
       col = "red", lwd = 3, lty = 2)


matplot(
  mape_lasso_LL$errores[, 3],
  abs(mape_lasso_LL$betas[, -1]),
  type="l", lwd = 2,
  lty=1, xlab = "MAPE-in", ylab = "betas")

matplot(
  mape_lasso_LL$errores[, 4],
  abs(mape_lasso_LL$betas[, -1]),
  type="l", lwd = 2,
  lty=1, xlab = "MAPE-out", ylab = "betas")

matplot(
  log(grid_lambda),
  mape_lasso_LL$errores[, 4],
  type="l", lwd = 2,
  lty=1, xlab = "log-lambda", ylab = "MAPE-out")
abline(v=log(grid_lambda[53]),
       col="blue", lty=2, lwd=2)
points(log(grid_lambda[53]), min(mape_lasso_LL$errores[,4]),
       pch=19, col="blue")

#Ahora vamos a ver los valores finales y los 
#vamos a comparar con OLS 
mape_lassoLL_in <- mape_lasso_LL$errores[53, 3]
mape_lassoLL_out <- mape_lasso_LL$errores[53, 4]

mape_ols_in
mape_lassoSSE_in
mape_lassoLL_in

mape_ols_out 
mape_lassoSSE_out
mape_lassoLL_out

# ==============================================================================
# PASO ADICIONAL: Comparación de la Siniestralidad Promedio por Tramos de Edad
# ==============================================================================
edades <- sort(unique(round(Datos$Edad,0)))
max(edades)
min(edades)
edades2 <- edades
edades <- c(edades, 80)
#Dentro
coste_real_in <- matrix(NA, nrow = length(edades)-1, ncol = 1)
coste_OLS_in <- matrix(NA, nrow = length(edades)-1, ncol = 1)
coste_LL_in <- matrix(NA, nrow = length(edades)-1, ncol = 1)
coste_SSE_in <- matrix(NA, nrow = length(edades)-1, ncol = 1)

#Fuera
coste_real_out <- matrix(NA, nrow = length(edades)-1, ncol = 1)
coste_OLS_out <- matrix(NA, nrow = length(edades)-1, ncol = 1)
coste_LL_out <- matrix(NA, nrow = length(edades)-1, ncol = 1)
coste_SSE_out <- matrix(NA, nrow = length(edades)-1, ncol = 1)

X_train$hat_OLS <- modelo1$fitted.values
X_train$hat_SSE <- mape_lasso$betas[51, 1] + X_mat %*% mape_lasso$betas[51, 2:13] 
X_train$hat_LL <- mape_lasso_LL$betas[53, 1] + X_mat %*% mape_lasso_LL$betas[53, 2:13] 

X_test$hat_OLS <- predict(modelo1, newdata = out)
X_test$hat_SSE <- mape_lasso$betas[51, 1] + X_test2 %*% mape_lasso$betas[51, 2:13] 
X_test$hat_LL <- mape_lasso_LL$betas[53, 1] + X_test2 %*% mape_lasso_LL$betas[53, 2:13]
  
i <- 2
for(i in 1:(length(edades)-1)){
  #Dentro 
  coste_real_in[i,] <- mean(X_train[X_train$Edad > edades[i] &
                                      X_train$Edad < edades[(i+1)], ]$CosteSiniestralidad, na.rm = T)
  coste_OLS_in[i,] <- mean(X_train[X_train$Edad > edades[i] & 
                                     X_train$Edad < edades[(i+1)], ]$hat_OLS, na.rm = T)
  coste_SSE_in[i,] <- mean(X_train[X_train$Edad > edades[i] & 
                                     X_train$Edad < edades[(i+1)], ]$hat_SSE, na.rm = T)
  coste_LL_in[i,] <- mean(X_train[X_train$Edad > edades[i] & 
                                    X_train$Edad < edades[(i+1)], ]$hat_LL, na.rm = T)
  ##################
  #Fuera
  coste_real_out[i,] <- mean(X_test[X_test$Edad > edades[i] &
                                      X_test$Edad < edades[(i+1)], ]$CosteSiniestralidad, na.rm = T)
  coste_OLS_out[i,] <- mean(X_test[X_test$Edad > edades[i] & 
                                X_test$Edad < edades[(i+1)], ]$hat_OLS, na.rm = T)
  coste_SSE_out[i,] <- mean(X_test[X_test$Edad > edades[i] & 
                                     X_test$Edad < edades[(i+1)], ]$hat_SSE, na.rm = T)
  coste_LL_out[i,] <- mean(X_test[X_test$Edad > edades[i] & 
                                    X_test$Edad < edades[(i+1)], ]$hat_LL, na.rm = T)
  
}

par(mfrow = c(1,1))
max1 <- max(coste_real_in, coste_OLS_in, coste_LL_in, coste_SSE_in)
min1 <- min(coste_real_in, coste_OLS_in, coste_LL_in, coste_SSE_in)

plot(edades2, coste_real_in, xlab="edades", ylab = "Prima", 
     type ="p", pch = 19, col = "blue", main = "In-Sample-Lasso")
lines(edades2, coste_OLS_in, col = "red", lwd = 2)
lines(edades2, coste_LL_in, col = "grey", lwd = 2, lty = 2)
lines(edades2, coste_SSE_in, col = "lightgreen", lwd = 2, lty = 2)
legend("bottomleft", c("real", "lineal", "lasso-OLS", "lasso-LL"),
       col = c("blue", "red", "grey", "lightgreen"), 
       lwd = 2, lty = c(NA, 1, 2, 2), pch = c(19, NA, NA, NA))


#Y ahora vamos a ver los valores fuera de muestra
plot(edades2, coste_real_out, xlab="edades", ylab = "Prima", 
     type ="p", pch = 19, col = "blue", main = "Out-of-Sample-Lasso")
lines(edades2, coste_OLS_out, col = "red", lwd = 2)
lines(edades2, coste_LL_out, col = "grey", lwd = 2, lty = 2)
lines(edades2, coste_SSE_out, col = "lightgreen", lwd = 2, lty = 2)
legend("bottomleft", c("real", "lineal", "lasso-OLS", "lasso-LL"),
       col = c("blue", "red", "grey", "lightgreen"), 
       lwd = 2, lty = c(NA, 1, 2, 2), pch = c(19, NA, NA, NA))

#Aunque Ridge puede modificar los coeficientes del modelo
#las predicciones finales pueden variar muy poco. 
#Esto se debe a que diferentes combinaciones de parámetros 
#pueden generar predicciones similares, especialmente 
#cuando existe correlación entre las variables explicativas.

#El beneficio principal de Ridge no es producir curvas distintas, 
#sino mejorar la estabilidad de los coeficientes y 
#la capacidad predictiva fuera de muestra.