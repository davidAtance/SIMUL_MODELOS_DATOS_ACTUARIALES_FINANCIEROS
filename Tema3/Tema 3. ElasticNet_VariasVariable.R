##############################################################
#ELASTIC - NET (varias variables)
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
library(caret)
train_idx <- createDataPartition(y, p = 0.80, list = FALSE)

#Training
X_train <- X[train_idx, ]

#Testing 
X_test <- X[-train_idx, ]

#Recordamos entrenamos el modelos y comprobamos como funciona 
#dentro de muestra para luego testarlo fuera de muestra.

#####################################################
#Modelo normal OLS 
modelo1 <- lm(CosteSiniestralidad ~ ., data = X_train)
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
#Vamos con el Elastic-Net
# Definimos la función de coste SSE Penalizado 
#(la que hemos generado antes)
f_sse_EN <- function(betas, x, y, lambda, alpha) {
  x <- as.matrix(x)
  
  x_c <- scale(x, center = TRUE, scale = TRUE)
  y_c <- y - mean(y)
  
  sse <- sum((y_c - x_c%*%betas)^2)
  penalizacion <- lambda * (alpha * sum(abs(betas)) + 
                              (1 - alpha) * sum(betas^2))
  return(sse + penalizacion)
}

#Voy a crear una rejilla para probar diferentes valores de 
#lambda y alpha, para ver cuál obtiene los mejores resultados: 
grid_lambda <- 10^seq(-2, 5, length.out = 100)
grid_alpha <- c(0, 0.25, 0.5, 0.75, 1)

#Lo vamos hacer para dentro de muestra y fuera de muestra 
#y eligiremos el lambda y alpha basado en ambas cosas 

#Defino el x_train quitando los valores de Y
x_train <- X_train[, !names(X_train) %in% "CosteSiniestralidad"]
names(x_train)

nrep <- length(grid_alpha)*length(grid_lambda)

#voy a generar una matriz para almacenar los valores: 
mape_elasticnet <- list(betas = matrix(NA, nrow = nrep, ncol = 15, 
                                       dimnames = list(c(1:nrep), 
                                                       c("b0", names(x_train), "alpha", "lambda"))), 
                        errores = matrix(NA, nrow = nrep, ncol = 4, 
                                         dimnames = list(c(1:nrep), 
                                                         c("SSE-in-ridge", "SSE-in", "MAPE-in", "MAPE-out"))))



#Genero una serie de cosas que voy a usar dentro: 
X_mat <- as.matrix(x_train)
p_vars <- ncol(X_mat) # Número de variables (p. ej. 12)
x_mean <- colMeans(X_mat)
x_sd   <- apply(X_mat, 2, sd)
y_train <- X_train$CosteSiniestralidad
X_test2 <- X_test[, !names(X_test) %in% "CosteSiniestralidad"] 
X_test2 <- as.matrix(X_test2)

# Bucle sobre los 100 valores de lambda y de 5 valores de alpha
# Inicializamos el contador global de iteraciones
total_iter <- nrep # p. ej. 5 * 100 = 500

k <- 1; iter_count <- 0
i <- a <- 1
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
  
    opt <- optim(par = rep(0, p_vars), fn = f_sse_EN,
                 x = X_mat, y = y_train, 
                 lambda = lam, alpha = alpha,
                 method = "BFGS")
    
    beta_std <- opt$par[1:p_vars]
    beta_orig <- beta_std / x_sd
    beta0_orig <- mean(y_train) - sum(beta_orig * x_mean)
    
    # Guardamos los coeficientes 
    mape_elasticnet$betas[k, ] <- c(beta0_orig, 
                                    beta_orig, 
                                    alpha, lam)
    
    #y el SSE no penalizado (medida de ajuste del modelo)
    yhat_in <- beta0_orig + X_mat %*% beta_orig 
    yhat_out <- beta0_orig + X_test2 %*% beta_orig
    
    resid_in <- (y_train - yhat_in)
    resid_out <- (X_test$CosteSiniestralidad - yhat_out)
    
    mape_elasticnet$errores[k, ] <- c(opt$value, 
                                      sum((y_train - yhat_in)^2), 
                                      100* mean(abs(resid_in/y_train)), 
                                      100* mean(abs(resid_out/X_test$CosteSiniestralidad)))

    k <- k + 1
    }}

# Logaritmo de lambda para los ejes X para que se vea mejor la distancia
log_lambda_vec <- log(grid_lambda)

which.min(mape_elasticnet$errores[, 1])
which.min(mape_elasticnet$errores[, 2])
which.min(mape_elasticnet$errores[, 3])
which.min(mape_elasticnet$errores[, 4])

head(mape_elasticnet$betas)
tail(mape_elasticnet$betas)

##############################################################3
#Visualización de los resultados
##############################################################3

elastic_betas <- as.data.frame(mape_elasticnet$betas)
names(elastic_betas)

alpha_sel <- 1
par(mfrow=c(1,3))
for(alpha_sel in c(0,0.5,1)){
  
  tmp <- elastic_betas[elastic_betas$alpha == alpha_sel, ]
  
  matplot(log(tmp$lambda), abs(as.matrix(tmp[,2:10])),
          type="l", lty=1, lwd=2, 
          xlab="log(lambda)", ylab="|beta|",
          ylim = range(abs(as.matrix(tmp[,2:10]))),
          main=paste("alpha =", alpha_sel))
  
  legend("topleft", legend=colnames(tmp)[2:10],
         cex=0.5, lwd=2, col=1:(ncol(tmp)-3))
}


#Otra visualización 
par(mfrow = c(1,1))
idx_lambda <- c(1, 25, 50, 75, 100)
cols <- c("red","blue","green","orange","black")

plot(NA, xlim = c(0,1), 
     ylim = range(elastic_betas$Potencia),
     xlab = expression(alpha), ylab = expression(hat(beta)[Potencia]),
     main = expression("Evolución de " * hat(beta)[Potencia] * " según " * alpha))

for(i in 1:length(idx_lambda)){
  
  lam_i <- grid_lambda[idx_lambda[i]]
  
  tmp <- elastic_betas[abs(elastic_betas$lambda - lam_i) < 1e-10, ]
  
  lines(tmp$alpha, tmp$Potencia, col = cols[i], lwd = 2)
  points(tmp$alpha, tmp$Potencia, col = cols[i], pch = 19)
}

#El gráfico muestra que el efecto de α depende del valor de λ:
#Para λ pequeños, todos los métodos producen resultados prácticamente idénticos.
#Para λ grandes, la elección de α se vuelve crítica.
#Al aumentar α provoca coeficientes mayores, aproximando las estimaciones a Lasso.
#La componente Ridge (alpha cercano a 0) genera una contracción más intensa del coeficiente de Potencia.

#Ahora vamos a ver los valores finales y los 
#vamos a comparar con OLS 
mape_ridgeSSE_in <- mape_elasticnet$errores[401, 3]
mape_ridgeSSE_out <- mape_elasticnet$errores[53, 4]

mape_ols_in
mape_ridgeSSE_in

mape_ols_out 
mape_ridgeSSE_out

#############################################################
#Ridge - OLS 
#############################################################
# Definimos la función de coste de Log-Verosimilitud Negativa Penalizada
f_loglik_elasticnet <- function(params, x, y, lambda, alpha) {
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
  
  # 2. Penalización Elastic-Net L2 sobre beta1 escalada por la varianza sigma^2
  penalizacion <- (lambda/(2*sigma^2))*(alpha * sum(abs(betas)) + 
                                          (1-alpha) * sum(betas^2))
  return(nll + penalizacion)
}

#voy a generar una matriz para almacenar los valores: 
mape_elasticnet_LL <- list(betas = matrix(NA, nrow = nrep, ncol = 16, 
                                          dimnames = list(c(1:nrep), 
                                                          c("b0", names(x_train), "sigma", "alpha", "lambda"))), 
                           errores = matrix(NA, nrow = nrep, ncol = 4, 
                                            dimnames = list(c(1:nrep), 
                                                            c("LL-in-ridge", "SSE-in", "MAPE-in", "MAPE-out"))))



#Genero una serie de cosas que voy a usar dentro: 
X_mat <- as.matrix(x_train)
p_vars <- ncol(X_mat) # Número de variables (p. ej. 12)
x_mean <- colMeans(X_mat)
x_sd   <- apply(X_mat, 2, sd)
y_train <- X_train$CosteSiniestralidad
X_test2 <- X_test[, !names(X_test) %in% "CosteSiniestralidad"] 
X_test2 <- as.matrix(X_test2)

# Bucle sobre los 100 valores de lambda y de 5 valores de alpha
# Inicializamos el contador global de iteraciones
total_iter <- nrep # p. ej. 5 * 100 = 500

k <- 1; iter_count <- 0
i <- a <- 1
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
    
    opt <- optim(par = c(rep(0, p_vars), sd(y_train)), 
                 fn = f_loglik_elasticnet,
                 x = X_mat, y = y_train, 
                 lambda = lam, alpha = alpha,
                 method = "BFGS")
    
    beta_std <- opt$par[1:p_vars]
    beta_orig <- beta_std / x_sd
    beta0_orig <- mean(y_train) - sum(beta_orig * x_mean)
    sigma_hat <- opt$par[p_vars+1]
    
    # Guardamos los coeficientes 
    mape_elasticnet_LL$betas[k, ] <- c(beta0_orig, 
                                       beta_orig, 
                                       sigma_hat,
                                       alpha, lam)
    
    #y el SSE no penalizado (medida de ajuste del modelo)
    yhat_in <- beta0_orig + X_mat %*% beta_orig 
    yhat_out <- beta0_orig + X_test2 %*% beta_orig
    
    resid_in <- (y_train - yhat_in)
    resid_out <- (X_test$CosteSiniestralidad - yhat_out)
    
    mape_elasticnet_LL$errores[k, ] <- c(opt$value, 
                                         sum((y_train - yhat_in)^2), 
                                         100* mean(abs(resid_in/y_train)), 
                                         100* mean(abs(resid_out/X_test$CosteSiniestralidad)))
    
    k <- k + 1
  }}

# Logaritmo de lambda para los ejes X para que se vea mejor la distancia
log_lambda_vec <- log(grid_lambda)

which.min(mape_elasticnet_LL$errores[, 1])
which.min(mape_elasticnet_LL$errores[, 2])
which.min(mape_elasticnet_LL$errores[, 3])
which.min(mape_elasticnet_LL$errores[, 4])

head(mape_elasticnet_LL$betas)
tail(mape_elasticnet_LL$betas)


#Ahora vamos a ver los valores finales y los 
#vamos a comparar con OLS 
mape_ridgeLL_in <- mape_ridge_LL$errores[53, 3]
mape_ridgeLL_out <- mape_ridge_LL$errores[53, 4]

mape_ols_in
mape_ridgeSSE_in
mape_ridgeLL_in

mape_ols_out 
mape_ridgeSSE_out
mape_ridgeLL_out

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
X_train$hat_SSE <- mape_elasticnet$betas[51, 1] + X_mat %*% mape_elasticnet$betas[51, 2:13] 
X_train$hat_LL <- mape_elasticnet_LL$betas[53, 1] + X_mat %*% mape_elasticnet_LL$betas[53, 2:13] 

X_test$hat_OLS <- predict(modelo1, newdata = out)
X_test$hat_SSE <- mape_elasticnet$betas[51, 1] + X_test2 %*% mape_elasticnet$betas[51, 2:13] 
X_test$hat_LL <- mape_elasticnet_LL$betas[53, 1] + X_test2 %*% mape_elasticnet_LL$betas[53, 2:13]
  
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
     type ="p", pch = 19, col = "blue", main = "In-Sample")
lines(edades2, coste_OLS_in, col = "red", lwd = 2)
lines(edades2, coste_LL_in, col = "grey", lwd = 2, lty = 2)
lines(edades2, coste_SSE_in, col = "lightgreen", lwd = 2, lty = 2)
legend("bottomleft", c("real", "lineal", "ridge-OLS", "ridge-LL"),
       col = c("blue", "red", "grey", "lightgreen"), 
       lwd = 2, lty = c(NA, 1, 2, 2), pch = c(19, NA, NA, NA))


#Y ahora vamos a ver los valores fuera de muestra
plot(edades2, coste_real_out, xlab="edades", ylab = "Prima", 
     type ="p", pch = 19, col = "blue", main = "Out-of-Sample")
lines(edades2, coste_OLS_out, col = "red", lwd = 2)
lines(edades2, coste_LL_out, col = "grey", lwd = 2, lty = 2)
lines(edades2, coste_SSE_out, col = "lightgreen", lwd = 2, lty = 2)
legend("bottomleft", c("real", "lineal", "ridge-OLS", "ridge-LL"),
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