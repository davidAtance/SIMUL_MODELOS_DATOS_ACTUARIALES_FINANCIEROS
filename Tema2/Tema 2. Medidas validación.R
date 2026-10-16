load(file = "Tema2Datos.RData")

head(sev)
modelo <- lm(ClaimAmount ~ ClaimNb + Exposure + VehPower +
               VehAge + DrivAge + BonusMalus + VehBrand +
               VehGas + Area + Density + Region, data = sev)
summary(modelo)

modelo2 <- lm(ClaimAmount ~ Exposure + BonusMalus, data = sev)
summary(modelo2)

#Valores predecidos por los modelos 
modelo$fitted.values 
predict(modelo)

modelo2$fitted.values 
predict(modelo2)

#Pero para entender lo que estamos haciendo, 
#vamos a ver la prediccion de la primera observacion 
modelo2$fitted.values[1]

#De dónde sale? 
modelo2$coef[1] + modelo2$coef[2]*sev$Exposure[1] + 
  modelo2$coef[3]*sev$BonusMalus[1]

#Pero al igual que la prediccion media podemos obtener los intervalos de
#confianza al 95% para todos los coeficientes
confint(modelo)
confint(modelo2)

# Si deseas cambiar el nivel de confianza (por ejemplo, al 90% o 99%)
confint(modelo2, level = 0.90)
confint(modelo2, level = 0.99)

# Para un coeficiente específico (ej. solo para Exposure y BonusMalus)
confint(modelo2, parm = c("Exposure", "BonusMalus"), level = 0.95)

#Para visualizar como funciona lo que voy hacer es sacar la media 
#del ClaimNb por edad
media_Edad_real <- tapply(sev$ClaimAmount, sev$DrivAge, mean)
media_Edad_mod1 <- tapply(modelo$fitted.values, sev$DrivAge, mean)
media_Edad_mod2 <- tapply(modelo2$fitted.values, sev$DrivAge, mean)
edades <- sort(unique(sev$DrivAge))

plot(edades, media_Edad_real, type = "p", pch = 19, col ="blue", lwd =1)
lines(edades, media_Edad_mod1, col = "red", lwd = 2)
lines(edades, media_Edad_mod2, col = "green", lwd = 2)
legend("topright", col = c("blue", "red", "green"), 
       c("Real", "model1", "model2"), lty = c(NA, 1, 1), 
       pch = c(19, NA, NA), lwd = c(1,2,2))

#Pero ahora vamos a visualizar los intervalos de confianza del modelo 2
# Sobre los mismos datos de entrenamiento
pred_ic <- predict(modelo2, interval = "confidence", level = 0.95)
media_Edad_mod2_low <- tapply(pred_ic[,2], sev$DrivAge, mean)
media_Edad_mod2_upp <- tapply(pred_ic[,3], sev$DrivAge, mean)

plot(edades, media_Edad_real, type = "p", pch = 19, col ="blue", lwd =1)
lines(edades, media_Edad_mod2, col = "green", lwd = 2)
lines(edades, media_Edad_mod2_low, col = "green3", lwd = 1)
lines(edades, media_Edad_mod2_upp, col = "green3", lwd = 1)

##############################################################
#Ahora vamos a calculas las medidas de error: 
residuals_1 <- sev$ClaimAmount - modelo$fitted.values 
residuals_2 <- sev$ClaimAmount - modelo2$fitted.values 

#Medidas no penalizadas 
#SSE = sum((y_real - y_hat)^2)
sum(residuals_1^2)
sum(residuals_2^2)

#MSE = 1/n * sum((y_real - y_hat)^2)
mean(residuals_1^2)
mean(residuals_2^2)

#RMSE = raiz(MSE)
sqrt(mean(residuals_1^2))
sqrt(mean(residuals_2^2))

#MAE = 1/n sum(abs(y_real - y_hat))
mean(abs(residuals_1))
mean(abs(residuals_2))

#MAPE = 100* 1/n sum(abs((y_real - y_hat)/y_real ))
100* mean(abs(residuals_1/sev$ClaimAmount))
100* mean(abs(residuals_2/sev$ClaimAmount))

#R2 = 1 - sum((y_real - y_hat)^2)/sum((y_real - y_real_media)^2)
1 - sum(residuals_1^2) / sum((sev$ClaimAmount - mean(sev$ClaimAmount))^2)
1 - sum(residuals_2^2) / sum((sev$ClaimAmount - mean(sev$ClaimAmount))^2)

#R2 ajustado
summary(modelo)$adj.r.squared
summary(modelo2)$adj.r.squared

#Medidas penalizadas 
#Log-likelihood
logLik(modelo)
logLik(modelo2)

#AIC = -2*LogLik + 2*p
AIC(modelo)
-2*logLik(modelo) + 2*46 #hay que sumar 1 al número de parámetros

AIC(modelo2) 
-2*logLik(modelo2) + 2*4#hay que sumar 1 parámetro 

#BIC = -2$LogLik + p*log(n)
BIC(modelo)
-2*logLik(modelo) + 46*log(length(sev$IDpol))

BIC(modelo2)
-2*logLik(modelo2) + 4*log(length(sev$IDpol))


