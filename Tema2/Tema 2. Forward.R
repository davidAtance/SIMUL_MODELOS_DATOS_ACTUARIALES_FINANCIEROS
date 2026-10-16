load(file = "Tema2Datos.RData")

head(sev)

######################################################################3
#Modelo forward stepwise
######################################################################3
#Partimos del modelo nulo (solo con constante)
modelo_nulo <- lm(ClaimAmount ~ 1, data = sev)

#Definimos el modelo completo: 
modelo_completo <- lm(ClaimAmount ~ ClaimNb + Exposure +
    VehPower + VehAge + DrivAge + BonusMalus + VehBrand +
    VehGas + Area + Density + Region, data = sev)

#Ahora aplicamos forward
modelo3 <- step(modelo_nulo, scope = formula(modelo_completo),
  direction = "forward")

#Variables seleccionadas: 
formula(modelo3)

######################################################################3
#Modelo backward stepwise
######################################################################3
modelo4 <- step(modelo_completo, direction = "backward")

#resultados finales
summary(modelo4)


######################################################################3
#Modelo Mixto forward and backward stepwise
######################################################################3
modelo5 <- step(modelo_completo, direction = "both")

summary(modelo5)

#Vamos a evaluar las tres propuestas, 
#para ello voy a crear una función para facilitar las cosas 
evalua_modelo <- function(modelo){
  
  pred <- predict(modelo)
  y <- modelo$model$ClaimAmount
  
  #Medidas penalizadas
  SSE <- sum((y-pred)^2)
  MSE <- mean((y-pred)^2)
  RMSE <- sqrt(MSE)
  MAE <- mean(abs(y-pred))
   
  data.frame(SSE = SSE,
             MSE = MSE,
             RMSE = RMSE,
             MAE = MAE,
             R2 = summary(modelo)$r.squared,
             R2_adj = summary(modelo)$adj.r.squared,
             AIC = AIC(modelo),
             BIC = BIC(modelo))
  
}

evalua_modelo(modelo3)
evalua_modelo(modelo4)
evalua_modelo(modelo5)

#tabla comparativa: 
rbind(
  Modelo1_Completo = evalua_modelo(modelo3),
  Modelo3_Forward = evalua_modelo(modelo4),
  Modelo4_Backward = evalua_modelo(modelo5))
