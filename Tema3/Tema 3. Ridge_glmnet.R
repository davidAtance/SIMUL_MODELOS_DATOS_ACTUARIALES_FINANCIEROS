# ==============================================================================
# Tema 3: Métodos de Regularización | Práctica 3.2: Estimador Ridge
# ==============================================================================

#Carga de paquetes necesarios -- instalar si es necesario 
if (!require("glmnet")) install.packages("glmnet")
if (!require("caret")) install.packages("caret")

library(glmnet) 
library(caret)  

# ==============================================================================
# PASO 1: Generación del dataset actuarial (Coste Medio de Siniestralidad)
# ==============================================================================
set.seed(2026) # Mantenemos la semilla

load(file = "datos_Ridge.Rdata")

head(datos_Ridge)
# Potencia: Potencia del vehículo (CV)
# Cilindrada: Cilindrada (cc) -> Muy correlacionada con Potencia
# ValorVehiculo: Valor venal del vehículo (€) -> Muy correlacionada con Potencia y Cilindrada
# Edad: Edad del conductor
# AntiguedadCarnet: Antigüedad del carnet
# CosteSiniestralidad: coste de siniestralidad
# MarcaVehiculo: 8 marcas diferentes de vehiculos

#Vamos a ver las variables gráficamente. 
#Variables Numercias
hist(datos_Ridge$Potencia, breaks = 30, col = "lightblue", 
     xlab="Potencia")
mean(datos_Ridge$Potencia); sd(datos_Ridge$Potencia)

hist(datos_Ridge$Cilindrada, breaks = 30, col = "lightblue", 
     xlab="Cilindrada")
mean(datos_Ridge$Cilindrada); sd(datos_Ridge$Cilindrada)

hist(datos_Ridge$ValorVehiculo, breaks = 30, col = "lightblue", 
     xlab="Valor Vehiculo")
mean(datos_Ridge$ValorVehiculo); sd(datos_Ridge$ValorVehiculo)

hist(datos_Ridge$Edad, breaks = 30, col = "lightblue", 
     xlab="Edad Asegurado")
mean(datos_Ridge$Edad); sd(datos_Ridge$Edad)

hist(datos_Ridge$AntiguedadCarnet, breaks = 30, col = "lightblue", 
     xlab="Antiguedad Carnet")
mean(datos_Ridge$AntiguedadCarnet); sd(datos_Ridge$AntiguedadCarnet)

hist(datos_Ridge$CosteSiniestralidad, breaks = 30, col = "lightblue", 
     xlab="Coste Siniestralidad")
mean(datos_Ridge$CosteSiniestralidad); sd(datos_Ridge$CosteSiniestralidad)

#Variable numérica
#MarcaVehiculo
tabla_frec <- table(datos_Ridge$MarcaVehiculo)
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
unique(datos_Ridge$MarcaVehiculo)

#Solo generamos 7 variables dicotómicas, la 8 la elinamos porque esta incluida
datos_Ridge$Tesla <- ifelse(datos_Ridge$MarcaVehiculo == "Tesla", 1, 0)
datos_Ridge$Seat <- ifelse(datos_Ridge$MarcaVehiculo == "Seat", 1, 0)
datos_Ridge$BMW <- ifelse(datos_Ridge$MarcaVehiculo == "BMW", 1, 0)
datos_Ridge$Volkswagen <- ifelse(datos_Ridge$MarcaVehiculo == "Volkswagen", 1, 0)
datos_Ridge$Toyota <- ifelse(datos_Ridge$MarcaVehiculo == "Toyota", 1, 0)
datos_Ridge$Volvo <- ifelse(datos_Ridge$MarcaVehiculo == "Volvo", 1, 0)
datos_Ridge$Hyundai <- ifelse(datos_Ridge$MarcaVehiculo == "Hyundai", 1, 0)

# ==============================================================================
# PASO 2: División de la muestra en Entrenamiento (Train) y Test
# ==============================================================================
#Nuestra variable y 
y <- datos_Ridge$CosteSiniestralidad

# Excluimos la variable respuesta 'CosteSiniestralidad' y el factor 'MarcaVehiculo'
# (Conservamos las covariables continuas y las 7 dummies de marca)
cols_excluir <- c("CosteSiniestralidad", "MarcaVehiculo")
X <- as.matrix(datos_Ridge[, !names(datos_Ridge) %in% cols_excluir])
head(X)

set.seed(2026) #semilla para hacer partición 

train_idx <- createDataPartition(y, p = 0.80, list = FALSE)

#ahora dividimos los datos en las dos partes 
#Training
X_train <- X[train_idx, ]
y_train <- y[train_idx]

#Testing 
X_test <- X[-train_idx, ]
y_test <- y[-train_idx]

# ==============================================================================
# PASO 3: Estimación por OLS y vemos problema de multicolinealidad
# ==============================================================================
df_train <- data.frame(y = y_train, X_train)
head(df_train)

#Ajustamos el modelo por OLS
ols_fit <- lm(y ~ ., data = df_train)
summary(ols_fit)

#Proceso clasico, quedarnos solo con las variables significativas: 
ols_fit2 <- lm(y ~ +1 + Potencia + Cilindrada + Edad + 
                AntiguedadCarnet + BMW, data = df_train)
summary(ols_fit2)

#################################################3
#Evaluamos el modelo dentro de muestra 
mse_ols1_in <- mean((y_train - ols_fit$fitted.values)^2)
mse_ols2_in <- mean((y_train - ols_fit2$fitted.values)^2)

cat("\nMSE-In sample todas variables OLS:", round(mse_ols1_in, 4), "\n")
cat("\nMSE-In sample variables signifcativas OLS:", round(mse_ols2_in, 4), "\n")

#######################################################33
#Repetimos proceso pero fuera de muestra 
df_test <- data.frame(X_test)

pred_ols1 <- predict(ols_fit, newdata = df_test)
pred_ols2 <- predict(ols_fit2, newdata = df_test)

mse_ols1_out <- mean((y_test - pred_ols1)^2)
mse_ols2_out <- mean((y_test - pred_ols2)^2)

cat("\nMSE-Out-of-sample todas variables OLS:", round(mse_ols1_out, 4), "\n")
cat("\nMSE-Out-of-sample variables signifcativas OLS:", round(mse_ols2_out, 4), "\n")

# ==============================================================================
# PASO 4: Estimación del Estimador Ridge (glmnet con alpha = 0)
# ==============================================================================
#Recordamos que la idea del Ridge es: 
#min(beta) = (y - f(x))^2 + lambda·(Sum(beta_1^2 + beta_2^2 + ...)) 

#Vamos crear la rejilla de valores 
print(colnames(X))

grid_lambda <- 10^seq(5, -2, length = 100) # Rejilla de valores para lambda
round(grid_lambda, 5)

ridge_mod <- glmnet(X_train, y_train, alpha = 0, lambda = grid_lambda)

# Traza de Coeficientes (Shrinkage Path)
plot(ridge_mod, xvar = "lambda", label = TRUE)
title("Traza de Coeficientes Ridge vs. Log(Lambda)", line = 2.5)

#Ver coeficientes de cada situacion 
coef_matrix <- as.matrix(coef(ridge_mod))

head(coef_matrix)

#Voy a extraer los valores de SSE en cada valor de lambda
sse_vec <- ridge_mod$nulldev * (1 - ridge_mod$dev.ratio)
log_lambda_vec <- log(ridge_mod$lambda)

# Extraemos la evolución del coeficiente de una variable en particular (ej. "Potencia")
coef_potencia <- as.vector(coef(ridge_mod)["Potencia", ])
coef_Edad <- as.vector(coef(ridge_mod)["Edad", ])

#Vamos a verlo graficamente 
par(mfrow = c(1, 2))

# Gráfico 1: SSE vs Log(Lambda)
plot(log_lambda_vec, sse_vec, type = "l", col = "darkred", lwd = 2.5,
     xlab = expression(log(lambda)), ylab = "SSE (Suma de Cuadrados de Residuos)",
     main = "Evolución del Error (SSE) vs. Lambda")
grid()
abline(v = log_lambda_vec[which.min(sse_vec)], lty = 2, col = "gray40")

# Gráfico 2: Coeficiente de 'Potencia' vs Log(Lambda)
plot(log_lambda_vec, coef_potencia, type = "l", col = "darkblue", lwd = 2.5,
     xlab = expression(log(lambda)), ylab = "Coeficiente Estimado (Potencia)",
     main = "Shrinkage del Coeficiente 'Potencia'")
grid()
abline(h = 0, lty = 2, col = "red")

# ==============================================================================
# PASO 5: Calibración del parámetro Lambda por Validación Cruzada (10-Fold CV)
# ==============================================================================
set.seed(2026)
cv_ridge <- cv.glmnet(X_train, y_train, alpha = 0, lambda = grid_lambda, nfolds = 10)

# Gráfico del error de validación cruzada frente a log(lambda)
par(mfrow = c(1,1))
plot(cv_ridge)
title("Error CV vs. Log(Lambda) en Ridge", line = 2.5)

#lambda.min: Es el valor de lambda que logra el mínimo MSE en CV 
cat("\nLambda Óptimo (lambda.min):", round(cv_ridge$lambda.min, 4), "\n")

#lambda.1se: Aplica la One Standard Error Rule. 
#No obtiene el SSE mínimo absoluto, sino que selecciona el valor de 
#lambda más restrictivo/penalizado cuyo error no dista más de 1 error estándar del mínimo. 
#Produce modelos más parsimoniosos sin sacrificar capacidad predictiva significativa.
cat("Lambda Parsimonioso (lambda.1se):", round(cv_ridge$lambda.1se, 4), "\n")

# 1. Predicciones en la muestra de Test para cada uno de los 100 valores de lambda
pred_matrix <- predict(ridge_mod, newx = X_test, s = grid_lambda) # Matriz n_test x 100

# 2. Cálculo del SSE fuera de muestra para cada lambda
sse_test <- colSums((y_test - pred_matrix)^2)

# 3. Traza de coeficientes y valores de lambda en logaritmos
log_lambda_vec <- log(grid_lambda)
coef_potencia  <- as.vector(coef(ridge_mod)["Potencia", ]) # Excluye intercepto

par(mfrow = c(1, 2))
# Gráfico A: SSE Test vs Log(Lambda)
plot(log_lambda_vec, sse_test, type = "l", col = "darkred", lwd = 2.5,
     xlab = expression(log(lambda)), ylab = "SSE Fuera de Muestra (Test)",
     main = "SSE oos vs. Lambda")
grid()
# Líneas verticales indicando lambda.min y lambda.1se
abline(v = log(cv_ridge$lambda.min), col = "blue", lty = 2, lwd = 1.5)
abline(v = log(cv_ridge$lambda.1se), col = "darkgreen", lty = 3, lwd = 1.5)
legend("topleft", legend = c("lambda.min", "lambda.1se"), 
       col = c("blue", "darkgreen"), lty = c(2, 3), cex = 0.8)

# Gráfico B: Coeficiente de 'Potencia' vs Log(Lambda)
plot(log_lambda_vec, coef_potencia, type = "l", col = "darkblue", lwd = 2.5,
     xlab = expression(log(lambda)), ylab = "Potencia",
     main = "Shrinkage de 'Potencia' vs. Lambda")
grid()
abline(v = log(cv_ridge$lambda.min), col = "blue", lty = 2, lwd = 1.5)
abline(v = log(cv_ridge$lambda.1se), col = "darkgreen", lty = 3, lwd = 1.5)
abline(h = 0, lty = 2, col = "red")
legend("topright", legend = c("lambda.min", "lambda.1se"), 
       col = c("blue", "darkgreen"), lty = c(2, 3), cex = 0.8)

par(mfrow = c(1, 1), mar = c(4.5, 4.5, 3, 1))
plot(sse_test, coef_potencia, type = "b", pch = 19, cex = 0.6, col = "purple",
     xlab = "SSE Fuera de Muestra (Test)", ylab = "Coeficiente Estimado (Potencia)",
     main = "Relación Coeficiente 'Potencia' vs. SSE Test")
grid()


# ==============================================================================
# PASO 6: Coeficientes y Evaluación del Modelo Ridge Óptimo
# ==============================================================================
# Coeficientes estimados con lambda.min
coef_ridge_min <- predict(ridge_mod, type = "coefficients", s = cv_ridge$lambda.min)
cat("\n--- Coeficientes Estimados por Ridge (lambda.min) --- \n")
print(coef_ridge_min)

# Predicción en muestra de Test
pred_ridge <- predict(ridge_mod, s = cv_ridge$lambda.min, newx = X_test)
mse_ridge_out <- mean((y_test - pred_ridge)^2)

cat("\nMSE-Out-of-sample Ridge:", round(mse_ridge_out, 2), "\n")
cat("\nMSE-Out-of-sample todas variables OLS:", round(mse_ols1_out, 4), "\n")
cat("\nMSE-Out-of-sample variables signifcativas OLS:", round(mse_ols2_out, 4), "\n")

