# ==============================================================================
# Tema 3: Métodos de Regularización | Práctica 3.3: Estimador LASSO
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

# Matriz de predictores X (excluyendo la variable respuesta y el factor original)
cols_excluir <- c("CosteSiniestralidad", "MarcaVehiculo")
X <- as.matrix(datos_Ridge[, !names(datos_Ridge) %in% cols_excluir])

set.seed(2026) # Misma semilla de partición
train_idx <- createDataPartition(y, p = 0.80, list = FALSE)

# Training 
X_train <- X[train_idx, ]
y_train <- y[train_idx]

#& Testing
X_test  <- X[-train_idx, ]
y_test  <- y[-train_idx]

# ==============================================================================
# PASO 3: Estimación por OLS (Recordatorio de referencia)
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
# PASO 4: Estimación del Estimador LASSO (glmnet con alpha = 1)
# ==============================================================================
# Recordamos la idea de LASSO: 
# min(beta) = SSE(beta) + lambda * Sum(|beta_j|)

grid_lambda <- 10^seq(5, -2, length = 100) # Rejilla de valores para lambda

# NOTA CLAVE: En LASSO usamos alpha = 1
lasso_mod <- glmnet(X_train, y_train, alpha = 1, lambda = grid_lambda)

# Traza de Coeficientes (Shrinkage & Selection Path)
plot(lasso_mod, xvar = "lambda", label = TRUE)
title("Traza de Coeficientes LASSO vs. Log(Lambda)", line = 2.5)

# Extraemos la matriz de coeficientes para observar la esparsidad (sparsity)
coef_matrix_lasso <- as.matrix(coef(lasso_mod))

# Extraemos valores de SSE dentro de muestra
sse_vec_lasso <- lasso_mod$nulldev * (1 - lasso_mod$dev.ratio)
log_lambda_vec <- log(lasso_mod$lambda)

# Evolución del coeficiente de una variable continua ("Potencia") y una dummy ("BMW")
coef_potencia_lasso <- as.vector(coef(lasso_mod)["Potencia", ])
coef_bmw_lasso      <- as.vector(coef(lasso_mod)["BMW", ])

# Visualización en muestra de entrenamiento
par(mfrow = c(1, 2))

# Gráfico 1: SSE vs Log(Lambda)
plot(log_lambda_vec, sse_vec_lasso, type = "l", col = "darkred", lwd = 2.5,
     xlab = expression(log(lambda)), ylab = "SSE",
     main = "SSE vs. Lambda (LASSO)")
grid()

# Gráfico 2: Coeficiente 'Potencia' vs Log(Lambda)
plot(log_lambda_vec, coef_potencia_lasso, type = "l", col = "darkblue", lwd = 2.5,
     xlab = expression(log(lambda)), ylab = "Potencia",
     main = "Seleccion lambda")
grid()
abline(h = 0, lty = 2, col = "red")

# ==============================================================================
# PASO 5: Calibración del parámetro Lambda por Validación Cruzada (10-Fold CV)
# ==============================================================================
set.seed(2026)
cv_lasso <- cv.glmnet(X_train, y_train, alpha = 1, 
                      lambda = grid_lambda, nfolds = 10)

par(mfrow = c(1, 1))
plot(cv_lasso)
title("Error CV vs. Log(Lambda) en LASSO", line = 2.5)

cat("\nLambda Óptimo LASSO (lambda.min)       :", round(cv_lasso$lambda.min, 4), "\n")
cat("Lambda Parsimonioso LASSO (lambda.1se) :", round(cv_lasso$lambda.1se, 4), "\n")

# Predicciones fuera de muestra para toda la rejilla de lambda
pred_matrix_lasso <- predict(lasso_mod, newx = X_test, s = grid_lambda)
sse_test_lasso    <- colSums((y_test - pred_matrix_lasso)^2)

par(mfrow = c(1, 2))

# Gráfico A: SSE Test vs Log(Lambda)
plot(log_lambda_vec, sse_test_lasso, type = "l", col = "darkred", lwd = 2.5,
     xlab = expression(log(lambda)), ylab = "SSE Fuera de Muestra (Test)",
     main = "SSE oos vs. Lambda (LASSO)")
grid()
abline(v = log(cv_lasso$lambda.min), col = "blue", lty = 2, lwd = 1.5)
abline(v = log(cv_lasso$lambda.1se), col = "darkgreen", lty = 3, lwd = 1.5)
legend("topleft", legend = c("lambda.min", "lambda.1se"), 
       col = c("blue", "darkgreen"), lty = c(2, 3), cex = 0.8)

# Gráfico B: Coeficiente de 'Potencia' vs Log(Lambda)
plot(log_lambda_vec, coef_potencia_lasso, type = "l", col = "darkblue", lwd = 2.5,
     xlab = expression(log(lambda)), ylab = "Potencia",
     main = "Selección de 'Potencia' vs. Lambda")
grid()
abline(v = log(cv_lasso$lambda.min), col = "blue", lty = 2, lwd = 1.5)
abline(v = log(cv_lasso$lambda.1se), col = "darkgreen", lty = 3, lwd = 1.5)
abline(h = 0, lty = 2, col = "red")
legend("topright", legend = c("lambda.min", "lambda.1se"), 
       col = c("blue", "darkgreen"), lty = c(2, 3), cex = 0.5)

par(mfrow = c(1, 1), mar = c(4.5, 4.5, 3, 1))
plot(sse_test_lasso, coef_potencia_lasso, type = "b", pch = 19, cex = 0.6, col = "purple",
     xlab = "SSE-out", ylab = "Potencia",
     main = "Relación Coeficiente 'Potencia' vs. SSE Test (LASSO)")
grid()

# ==============================================================================
# PASO 6: Coeficientes y Selección de Variables por LASSO Óptimo
# ==============================================================================
# Coeficientes estimados con lambda.min
coef_lasso_min <- predict(lasso_mod, type = "coefficients", s = cv_lasso$lambda.min)
cat("\n--- Coeficientes Estimados por LASSO (lambda.min) --- \n")
print(coef_lasso_min)

# Identificar cuáles variables fueron seleccionadas (distintas de cero)
vars_seleccionadas <- rownames(coef_lasso_min)[as.vector(coef_lasso_min != 0)]
cat("\nVariables seleccionadas por LASSO (no nulas): \n")
print(vars_seleccionadas)

# Predicción en muestra de Test
pred_lasso <- predict(lasso_mod, s = cv_lasso$lambda.min, newx = X_test)
mse_lasso_out <- mean((y_test - pred_lasso)^2)

cat("\n=================== COMPARATIVA FINAL MSE (TEST) ===================\n")
cat("MSE Out (OLS Completo)       :", round(mse_ols1_out, 4), "\n")
cat("MSE Out (OLS Seleccion)       :", round(mse_ols2_out, 4), "\n")
cat("MSE Out (LASSO lambda.min)   :", round(mse_lasso_out, 4), "\n")
cat("===================================================================\n")