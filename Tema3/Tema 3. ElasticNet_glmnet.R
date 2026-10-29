# ==============================================================================
# Tema 3: Métodos de Regularización | Práctica 3.4: Estimador Elastic Net
# ==============================================================================

# Carga de paquetes necesarios
if (!require("glmnet")) install.packages("glmnet")
if (!require("caret")) install.packages("caret")

library(glmnet) 
library(caret)  

# ==============================================================================
# PASO 1: Carga y Preparación del Dataset Actuarial
# ==============================================================================
set.seed(2026) # Mantenemos la semilla

load(file = "datos_Ridge.Rdata")

# Creación manual de las 7 variables dicotómicas de marca (Mercedes = Base level)
datos_Ridge$Tesla      <- ifelse(datos_Ridge$MarcaVehiculo == "Tesla", 1, 0)
datos_Ridge$Seat       <- ifelse(datos_Ridge$MarcaVehiculo == "Seat", 1, 0)
datos_Ridge$BMW        <- ifelse(datos_Ridge$MarcaVehiculo == "BMW", 1, 0)
datos_Ridge$Volkswagen <- ifelse(datos_Ridge$MarcaVehiculo == "Volkswagen", 1, 0)
datos_Ridge$Toyota     <- ifelse(datos_Ridge$MarcaVehiculo == "Toyota", 1, 0)
datos_Ridge$Volvo      <- ifelse(datos_Ridge$MarcaVehiculo == "Volvo", 1, 0)
datos_Ridge$Hyundai    <- ifelse(datos_Ridge$MarcaVehiculo == "Hyundai", 1, 0)

# ==============================================================================
# PASO 2: División de la muestra en Entrenamiento (Train) y Test
# ==============================================================================
y <- datos_Ridge$CosteSiniestralidad

# Matriz de predictores X
cols_excluir <- c("CosteSiniestralidad", "MarcaVehiculo")
X <- as.matrix(datos_Ridge[, !names(datos_Ridge) %in% cols_excluir])

set.seed(2026) # Misma semilla de partición
train_idx <- createDataPartition(y, p = 0.80, list = FALSE)

# Training & Testing
X_train <- X[train_idx, ]
y_train <- y[train_idx]

X_test  <- X[-train_idx, ]
y_test  <- y[-train_idx]

# ==============================================================================
# PASO 3: Estimación por OLS (Referencia)
# ==============================================================================
df_train <- data.frame(y = y_train, X_train)
ols_fit <- lm(y ~ ., data = df_train)
summary(ols_fit)

ols_fit2 <- lm(y ~ + 1 + Potencia + Cilindrada + Edad + 
                 AntiguedadCarnet + BMW, data = df_train)
summary(ols_fit2)

df_test <- data.frame(X_test)

pred_ols1 <- predict(ols_fit, newdata = df_test)
pred_ols2 <- predict(ols_fit2, newdata = df_test)

mse_ols1_out <- mean((y_test - pred_ols1)^2)
mse_ols2_out <- mean((y_test - pred_ols2)^2)

# ==============================================================================
# PASO 4: Estimación del Estimador Elastic Net (glmnet con alpha = 0.5)
# ==============================================================================
# Recordamos la idea de Elastic Net: 
# min(beta) = SSE(beta) + lambda * [ (1-alpha)/2 * Sum(beta_j^2) + alpha * Sum(|beta_j|) ]

grid_lambda <- 10^seq(5, -2, length = 100) # Rejilla de valores para lambda
alpha_enet  <- 0.5 # Parámetro de mezcla (compromiso 50% Ridge - 50% LASSO)

enet_mod <- glmnet(X_train, y_train, alpha = alpha_enet, lambda = grid_lambda)

# Traza de Coeficientes (Shrinkage Path)
plot(enet_mod, xvar = "lambda", label = TRUE)
title("Traza de Coeficientes Elastic Net (alpha = 0.5) vs. Log(Lambda)", line = 2.5)

# Extraemos valores de SSE dentro de muestra
sse_vec_enet <- enet_mod$nulldev * (1 - enet_mod$dev.ratio)
log_lambda_vec <- log(enet_mod$lambda)

# Evolución del coeficiente de una variable continua ("Potencia")
coef_potencia_enet <- as.vector(coef(enet_mod)["Potencia", ])

# Visualización en muestra de entrenamiento
par(mfrow = c(1, 2))

# Gráfico 1: SSE vs Log(Lambda)
plot(log_lambda_vec, sse_vec_enet, type = "l", col = "darkred", lwd = 2.5,
     xlab = expression(log(lambda)), ylab = "SSE",
     main = "SSE vs. Lambda (Elastic Net)")
grid()

# Gráfico 2: Coeficiente 'Potencia' vs Log(Lambda)
plot(log_lambda_vec, coef_potencia_enet, type = "l", col = "darkblue", lwd = 2.5,
     xlab = expression(log(lambda)), ylab = "Potencia",
     main = "Shrinkage / 'Potencia' (Elastic Net)")
grid()
abline(h = 0, lty = 2, col = "red")

# ==============================================================================
# PASO 5: Calibración de Lambda mediante Validación Cruzada (10-Fold CV)
# ==============================================================================
set.seed(2026)
cv_enet <- cv.glmnet(X_train, y_train, alpha = alpha_enet, 
                     lambda = grid_lambda, nfolds = 10)

par(mfrow = c(1, 1))
plot(cv_enet)
title("Error CV vs. Log(Lambda) en Elastic Net (alpha = 0.5)", line = 2.5)

cat("\nLambda Óptimo Elastic Net (lambda.min)       :", round(cv_enet$lambda.min, 4), "\n")
cat("Lambda Parsimonioso Elastic Net (lambda.1se) :", round(cv_enet$lambda.1se, 4), "\n")

# Predicciones fuera de muestra para toda la rejilla de lambda
pred_matrix_enet <- predict(enet_mod, newx = X_test, s = grid_lambda)
sse_test_enet    <- colSums((y_test - pred_matrix_enet)^2)

par(mfrow = c(1, 2))

# Gráfico A: SSE Test vs Log(Lambda)
plot(log_lambda_vec, sse_test_enet, type = "l", col = "darkred", lwd = 2.5,
     xlab = expression(log(lambda)), ylab = "SSE-out",
     main = "SSE oos vs. Lambda (Elastic Net)")
grid()
abline(v = log(cv_enet$lambda.min), col = "blue", lty = 2, lwd = 1.5)
abline(v = log(cv_enet$lambda.1se), col = "darkgreen", lty = 3, lwd = 1.5)
legend("topleft", legend = c("lambda.min", "lambda.1se"), 
       col = c("blue", "darkgreen"), lty = c(2, 3), cex = 0.8)

# Gráfico B: Coeficiente de 'Potencia' vs Log(Lambda)
plot(log_lambda_vec, coef_potencia_enet, type = "l", col = "darkblue", lwd = 2.5,
     xlab = expression(log(lambda)), ylab = "Potencia",
     main = "Shrinkage de 'Potencia' vs. Lambda")
grid()
abline(v = log(cv_enet$lambda.min), col = "blue", lty = 2, lwd = 1.5)
abline(v = log(cv_enet$lambda.1se), col = "darkgreen", lty = 3, lwd = 1.5)
abline(h = 0, lty = 2, col = "red")
legend("topright", legend = c("lambda.min", "lambda.1se"), 
       col = c("blue", "darkgreen"), lty = c(2, 3), cex = 0.8)

par(mfrow = c(1, 1), mar = c(4.5, 4.5, 3, 1))
plot(sse_test_enet, coef_potencia_enet, type = "b", pch = 19, cex = 0.6, col = "purple",
     xlab = "SSE - out", ylab = "Coeficiente Estimado (Potencia)",
     main = "Relación 'Potencia' vs. SSE- out (Elastic Net)")
grid()

# ==============================================================================
# PASO 6: Coeficientes y Comparativa Global de Métodos
# ==============================================================================
# Coeficientes estimados con lambda.min
coef_enet_min <- predict(enet_mod, type = "coefficients", s = cv_enet$lambda.min)
cat("\n--- Coeficientes Estimados por Elastic Net (alpha = 0.5, lambda.min) --- \n")
print(coef_enet_min)

# Predicción en muestra de Test
pred_enet <- predict(enet_mod, s = cv_enet$lambda.min, newx = X_test)
mse_enet_out <- mean((y_test - pred_enet)^2)

cat("\n=================== COMPARATIVA FINAL MSE (TEST) ===================\n")
cat("MSE Out (OLS Completo)         :", round(mse_ols1_out, 4), "\n")
cat("MSE Out (OLS-seleccion)         :", round(mse_ols2_out, 4), "\n")
cat("MSE Out (Elastic Net alpha=0.5):", round(mse_enet_out, 4), "\n")
cat("===================================================================\n")


# ==============================================================================
# PASO ADICIONAL: Promedio de Predicciones por Intervalos de Potencia (Test)
# ==============================================================================

# 1. Definimos tramos / intervalos para 'Potencia' en el conjunto de Test
# (por ejemplo, amplitudes de 15 CV para agrupar las observaciones)
num_cortes <- 12 
cortes_potencia <- cut(X_test[, "Potencia"], breaks = num_cortes)

# 2. Creamos un data.frame con los valores reales y las predicciones ya calculadas
df_grafico <- data.frame(
  Potencia_Real = X_test[, "Potencia"],
  Coste_Real    = y_test,
  Pred_OLS      = pred_ols1,
  Pred_ENET     = as.vector(pred_enet),
  Tramo         = cortes_potencia
)

# 3. Calculamos la media de la Potencia, la Siniestralidad Real y las Predicciones en cada tramo
if (!require("aggregate")) install.packages("aggregate")
library(aggregate) # O usando aggregate estándar de base R

medias_tramo <- aggregate(cbind(Potencia_Real, Coste_Real, Pred_OLS, Pred_ENET) ~ Tramo, 
                          data = df_grafico, 
                          FUN = mean)

# Ordenamos por la potencia media del tramo por seguridad
medias_tramo <- medias_tramo[order(medias_tramo$Potencia_Real), ]

# 4. Representación Gráfica
par(mfrow = c(1, 1), mar = c(5, 4.5, 4, 2) + 0.1)

# Diagrama de puntos reales de Test (Scatter plot de fondo)
plot(df_grafico$Potencia_Real, df_grafico$Coste_Real,
     pch = 19, col = adjustcolor("gray60", alpha.f = 0.35), cex = 0.8,
     xlab = "Potencia del Vehículo (CV)",
     ylab = "Coste Medio de Siniestralidad (€)",
     main = "Media de Siniestralidad Observada vs. Predicha por Intervalos",
     sub = "Muestra de Test agrupada por tramos de Potencia")

# Curva de medias predichas por OLS
lines(medias_tramo$Potencia_Real, medias_tramo$Pred_OLS, 
      col = "firebrick", lwd = 3, lty = 1, type = "b", pch = 19)

# Curva de medias predichas por Elastic Net (Suavizada)
lines(medias_tramo$Potencia_Real, medias_tramo$Pred_ENET, 
      col = "royalblue", lwd = 3, lty = 2, type = "b", pch = 17)

# Leyenda explicativa
legend("topleft", 
       legend = c("Pólizas Reales (Test)",
                  "Media Predicha OLS", 
                  "Media Predicha Elastic Net"),
       col = c("gray60", "firebrick", "royalblue"),
       pch = c(19, 18, 19, 17),
       lty = c(NA, 3, 1, 2),
       lwd = c(NA, 1.5, 3, 3),
       bg = "white", cex = 0.85)

grid()

