load(file = "Tema2Datos.RData")
Datos
head(Datos)
summary(Datos)

hist(Datos$prima, breaks = 20, col = "red3", xlab = "prima")
hist(Datos$edad, breaks = 20, col = "lightblue", xlab = "Edad")
hist(Datos$bm, breaks = 20, col = "lightblue", xlab = "Bonus malus")
hist(Datos$potencia, breaks = 20, col = "lightblue", xlab = "Potencia")
hist(Datos$antiguedad, breaks = 20, col = "lightblue", xlab = "Antiguedad polizas")
hist(Datos$kms, breaks = 20, col = "lightblue", xlab = "Kms")
hist(Datos$capital, breaks = 20, col = "lightblue", xlab = "Capital cubierto")

modelo <- lm(prima ~ edad + bm + potencia +
               antiguedad + kms + capital, data = Datos)
summary(modelo)

#Valores ajustados
modelo$fitted.values

#Coeficientes del modelo
modelo$coef
#Primer valor de forma manual
modelo$coef[1] + modelo$coef[2]*Datos$edad[1] + modelo$coef[3]*Datos$bm[1] + 
  modelo$coef[4]*Datos$potencia[1] + modelo$coef[5]*Datos$antiguedad[1] +
  modelo$coef[6]*Datos$kms[1] + modelo$coef[7]*Datos$capital[1]

#Certificar resultado es igual 
modelo$fitted.values[1]


#Para certificar que el modelo se ajusta bien 
#a la distribucion elegida: 
#1. Residuos tienen que oscilar alrededor del cero 
plot(modelo$fitted.values, residuals(modelo),
     pch = 19, col = "blue",
     xlab = "Valores ajustados", ylab = "Residuos")
abline(h = 0, col = "red", lwd = 3)

#2. Histograma --- se ajuste a una normal 
hist(residuals(modelo), probability = TRUE, 
     breaks = 40, col = "lightblue")
curve(dnorm(x, mean(residuals(modelo)), sd(residuals(modelo))),
      add = TRUE, col = "red", lwd = 3)

#3. qq-plot Compara:
#Los cuantiles observados de los residuos.
#Los cuantiles teóricos de una distribución Normal.
qqnorm(residuals(modelo))
qqline(residuals(modelo), col = "red", lwd = 3)

#4. Homocedasticidad de los residuos -- independientes entre si
plot(residuals(modelo), type="l")
abline(h=0,col="red")

#De forma conjunta de una sola vez: 
par(mfrow=c(2,2))
plot(modelo)

#Vamos a ver como ajustamos a los datos de forma prima media:
edades <- sort(unique(Datos$edad))
prima_real <- matrix(NA, nrow = length(edades), ncol = 1)
prima_estimada <- matrix(NA, nrow = length(edades), ncol = 1)

Datos$hat_prima <- modelo$fitted.values
i <- 1
for(i in 1:length(edades)){
  prima_real[i,] <- mean(Datos[Datos$edad == edades[i], ]$prima)
  prima_estimada[i,] <- mean(Datos[Datos$edad == edades[i], ]$hat_prima)
}

max1 <- max(prima_real, prima_estimada)
min1 <- min(prima_real, prima_estimada)
plot(edades, prima_real, xlab="edades", ylab = "Prima", 
     type ="p", pch = 19, col = "blue")
lines(edades, prima_estimada, col = "red", lwd = 2)


###################################################################
#Repetimos con la siguiente base de datos, que corresponde con 
#datos de autos de Francia: 
#library(CASdatasets)
sev

head(sev)

#Variables numéricas
hist(sev$ClaimNb, breaks = 10, col = "lightblue", xlab = "")
hist(sev$Exposure, breaks = 10, col = "lightblue", xlab = "")
hist(sev$DrivAge, breaks = 10, col = "lightblue", xlab = "")

#Variables alfabéticas
table(sev$Area)
table(sev$Region)

length(sev$ClaimAmount)
names(sev)

hist(sev$ClaimAmount, breaks = 100, col = "lightblue")
mean(sev$ClaimAmount)
sd(sev$ClaimAmount)
min(sev$ClaimAmount)
max(sev$ClaimAmount)

modelo2 <- lm(ClaimAmount ~ DrivAge + VehPower + BonusMalus + Density,
              data = sev)
summary(modelo2)

#Vamos a verificar que sigan una distribución normal
#Residuos tienen que oscilar alrededor del cero 
plot(modelo2$fitted.values, residuals(modelo2),
     pch = 19, col = "blue",
     xlab = "Valores ajustados", ylab = "Residuos")
abline(h = 0, col = "red", lwd = 3)

#Histograma --- se ajuste a una normal?? 
hist(residuals(modelo2), probability = TRUE, 
     breaks = 40, col = "lightblue")

curve(dnorm(x, mean(residuals(modelo2)), sd(residuals(modelo2))),
      add = TRUE, col = "red", lwd = 3)

#qq-plot Compara:
#Los cuantiles observados de los residuos.
#Los cuantiles teóricos de una distribución Normal.
qqnorm(residuals(modelo2))
qqline(residuals(modelo2), col = "red", lwd = 3)

#Homocedasticidad de los residuos -- independientes entre si
plot(residuals(modelo2), type="l")
abline(h=0,col="red")

#De forma conjunta: 
par(mfrow=c(2,2))
plot(modelo2)
ClaimAmount

#Vamos a ver como ajustamos a los datos de forma prima media:
edades <- sort(unique(sev$DrivAge))
CA_real <- matrix(NA, nrow = length(edades), ncol = 1)
CA_estimada <- matrix(NA, nrow = length(edades), ncol = 1)

sev$hat_CA <- modelo2$fitted.values
i <- 1
for(i in 1:length(edades)){
  CA_real[i,] <- mean(sev[sev$DrivAge == edades[i], ]$ClaimAmount, na.rm = T)
  CA_estimada[i,] <- mean(sev[sev$DrivAge == edades[i], ]$hat_CA, na.rm = T)
}

max1 <- max(CA_real, CA_estimada)
min1 <- min(CA_real, CA_estimada)
plot(edades, CA_real, xlab="edades", ylab = "Prima", 
     type ="p", pch = 19, col = "blue")
lines(edades, CA_estimada, col = "red", lwd = 2)


#Conclusion ---- para predecir el coste siniestral la distribución Normal
#funciona mal.
#Por lo que es necesario utilizar otras distribuciones -- GLMs