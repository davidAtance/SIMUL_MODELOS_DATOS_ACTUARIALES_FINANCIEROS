###############################################################3
#CROSS-VALIDATION
###############################################################3
#HOLD-OUT
load(file = "Tema2Datos.RData")

set.seed(123)

n <- nrow(sev)

#Vamos a dividir los datos de forma aleatoria en 
#entrenamiento y validacion 
id_train <- sample(1:n, size = round(0.7*n))

train <- sev[id_train, ]
test  <- sev[-id_train, ]

#Vamos a utilizar el modelo con dos variables: 
modelo_1 <- lm(ClaimAmount ~ Exposure + BonusMalus, 
               data = train)
modelo_2 <- lm(ClaimAmount ~ DrivAge + Exposure, 
               data = train)
summary(modelo_1)
summary(modelo_2)

pred_1 <- predict(modelo_1, newdata = test)
pred_2 <- predict(modelo_2, newdata = test)

#Evaluamos la capacidad predictiva de los modelos fuera de muestra
y <- test$ClaimAmount

#MSE
mean((y-pred_1)^2)
mean((y-pred_2)^2)

#RMSE
sqrt(MSE_1)
sqrt(MSE_2)

#MAE
mean(abs(y-pred_1))
mean(abs(y-pred_2))

######################################################################
#K - FOLD CV
######################################################################
set.seed(123)

K <- 5
folds <- sample(rep(1:K, length.out = nrow(sev)))

RMSE_1 <- numeric(K)
RMSE_2 <- numeric(K)

#Hay que montar 1 bucle para cada modelo
#Modelo 1
k <- 1
for(k in 1:K){
  
  train <- sev[folds != k, ]
  test  <- sev[folds == k, ]
  
  modelo <- lm(ClaimAmount ~ Exposure + BonusMalus, 
               data = train)
  
  pred <- predict(modelo, newdata = test)
  
  RMSE_1[k] <- sqrt(mean((test$ClaimAmount - pred)^2))
  
}
#Modelo 1
k <- 1
for(k in 1:K){
  
  train2 <- sev[folds != k, ]
  test2  <- sev[folds == k, ]
  
  modelo2 <- lm(ClaimAmount ~ DrivAge + Exposure, 
               data = train2)
  
  pred2 <- predict(modelo2, newdata = test2)
  
  RMSE_2[k] <- sqrt(mean((test2$ClaimAmount - pred2)^2))
  
}

mean(RMSE_1)
mean(RMSE_2)


####################################################
# LOOCV
####################################################
n <- nrow(sev)

RMSE_1 <- numeric(n)
#Usar menos de n ya que son muchas 
for(i in 1:n){
  
  train <- sev[-i, ]
  test  <- sev[i, , drop = FALSE]
  
  modelo <- lm(ClaimAmount ~ Exposure + BonusMalus,
    data = train)
  
  pred <- predict(modelo, newdata = test)
  
  RMSE_1[i] <- sqrt((test$ClaimAmount - pred)^2)
}

RMSE_2 <- numeric(n)

for(i in 1:n){
  train2 <- sev[-i, ]
  test2  <- sev[i, , drop = FALSE]
  
  modelo2 <- lm(ClaimAmount ~ DrivAge + Exposure,
    data = train2)
  
  pred2 <- predict(modelo2, newdata = test2)
  
  RMSE_2[i] <- sqrt((test2$ClaimAmount - pred2)^2)
  
}

mean(RMSE_1)
mean(RMSE_2)