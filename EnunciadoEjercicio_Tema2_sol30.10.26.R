#####################################################################
#TEMA 2 - REGRESIONES LINEALES
#####################################################################
#TRABAJO PRÁCTICO: Tema 2 - Modelos de Regresión Lineal 
#y Diagnóstico Predictivo
#EJERCICIO a entregar para la próxima semana 23 de Octubre de 2026

#####################################################################
#El objetivo de esta práctica es modelizar y predecir el comportamiento del 
#logaritmo de la prima/cuota de una cartera de seguros de automóvil ('LogCuota') 
#a partir de un conjunto de variables objetivas del asegurado y del vehículo, 
#aplicando técnicas de preprocesamiento, selección automática de variables y 
#evaluación predictiva mediante validación cruzada.
#####################################################################
#BASE DE DATOS:
#####################################################################
#Se proporciona el conjunto de datos: 
load(file = "EjercicioTema2.RData")

head(auto232)

names(auto232)
#contiene información sobre pólizas de automóviles con variables demográficas, 
#técnicas y de exposición.
#####################################################################
#APARTADOS A DESARROLLAR
#####################################################################

#1. PREPROCESAMIENTO DE LOS DATOS:
  
#a) Analizar la existencia de datos faltantes (NAs) en la base de datos `auto232`. 
#Indicar el número total de valores ausentes por variable y el porcentaje 
#que representan sobre la muestra.

#Vamos a ver el numero de datos NA por columna
colSums(is.na(auto232))

#Para verlo en porcentaje: 
colSums(is.na(auto232))/length(auto232$Estado)

#Contar los duplicados que hay  
sum(duplicated(auto232))
#Mostrar los duplicados 
auto232[duplicated(auto232), ]

#Ver un ejemplo de que existen 
auto232[auto232$Estado == "STATE 15" & auto232$Clase == "C7B" 
        & auto232$Genero == "F" & auto232$Edad == 50, ]

length(auto232$Estado)

#Eliminar duplicados 
auto232 <- auto232[!duplicated(auto232), ]
length(auto232$Estado)


#b) Justificar la estrategia de tratamiento aplicada para cada tipo de variable:
#  - ¿Qué decisión se debe tomar con las observaciones que presentan NA en la 
#variables tanto continúas como las variables categóricas?

#hay varias opciones, todas son correctas si la estrategia tiene sentido 
#Primero voy a ver los NA que tenemos
auto232[is.na(auto232$Cuota), ]

auto232[is.na(auto232$Ant), ]
#Parece que hay varios individuos que tienen datos faltantes 

#Por ello voy a ver cuantos individuos tienen al menos 3 datos faltantes
na_fila <- rowSums(is.na(auto232))
head(na_fila)
which(na_fila >= 3)
# o ver las filas directamente
auto232[na_fila >= 3, ]

#De estos, yo el que tiene 4 datos faltantes me lo cargaría 
#porque tenemos 10 variables y le faltan 4... (40\%)
length(auto232$Estado)
auto232 <- auto232[na_fila < 4, ]

#Se va a ver que hemos eliminado solo 1 dato
length(auto232$Estado)

##########################################3
#Vamos con los NA
#Primero "Cuota"
hist(auto232$Cuota, breaks = 20, col = "lightblue")
mean(auto232$Cuota, na.rm = T)
sd(auto232$Cuota, na.rm = T)

#Le asigno la media
auto232[is.na(auto232$Cuota), ]$Cuota <- mean(auto232$Cuota, na.rm = T)

#Ahora chequeo que no haya valores faltantes
auto232[is.na(auto232$Cuota), ]

##########################################3
#Para "LogCuota" vamos a dar el valor previo que es el logaritmo de la 
#variable Cuota 
auto232[is.na(auto232$LogCuota), ]$LogCuota <- log(auto232[is.na(auto232$LogCuota), ]$Cuota)
#Ahora chequeo que no haya valores faltantes
auto232[is.na(auto232$LogCuota), ]

hist(auto232$LogCuota, breaks = 20)

##########################################3
#Vamos con la Ant --- 
sort(unique(auto232$Ant))
hist(auto232$Ant, breaks = 20, col = "lightblue")
table(auto232$Ant)

mean(auto232$Ant, na.rm = T)
sd(auto232$Ant, na.rm = T)

#En este caso la variable tiene que ser un número entero 
#Por ello voy a optar por darle el valor de la Moda
#Para ello voy a crear una tabla con los individuos de cada antiguedad
table(auto232$Ant)

#Ahora voy a estimar lo moda
names(sort(table(auto232$Ant), decreasing = TRUE))[1]

#y ahora asigno este valor a la Antiguedad 
auto232[is.na(auto232$Ant), ]$Ant <- 3

##########################################3
#Recuerdo cuales son las variables que me quedan con NA
colSums(is.na(auto232))

hist(auto232$CV_vehi, breaks = 20, col = "lightblue")

unique(auto232$CV_vehi)

mean(auto232$CV_vehi, na.rm = T)
sd(auto232$CV_vehi, na.rm = T)
#Al igual que antes son valores enteros, 
names(sort(table(auto232$CV_vehi), decreasing = TRUE))[1]

#Doy dos opciones o moda o media pero en número entero
auto232[is.na(auto232$CV_vehi), ]$CV_vehi <- round(mean(auto232$CV_vehi, na.rm = T))

auto232[is.na(auto232$CV_vehi), ]

##########################################3
#Recuerdo cuales son las variables que me quedan con NA
colSums(is.na(auto232))
#vamos con el "Valor"

auto232$Valor
hist(auto232$Valor, breaks = 20, col = "lightblue")

mean(auto232$Valor, na.rm = T)
sd(auto232$Valor, na.rm = T)

#A estos les voy asignar la media: 
auto232[is.na(auto232$Valor), ]$Valor <- mean(auto232$Valor, na.rm = T)

#Chequear que ya no hay na en esta variable: 
auto232[is.na(auto232$Valor), ]

##########################################3
#Recuerdo cuales son las variables que me quedan con NA
colSums(is.na(auto232))

#Vamos ahora con "BM" el bonus malus: 
unique(auto232$BM)
table(auto232$BM)

mean(auto232$BM, na.rm = T)
sd(auto232$BM, na.rm = T)
#Al igual que antes son valores enteros, 
moda_bm <- as.numeric(names(sort(table(auto232$BM), decreasing = TRUE))[1])

auto232[is.na(auto232$BM), ]$BM <- moda_bm

##########################################3
#Recuerdo cuales son las variables que me quedan con NA
colSums(is.na(auto232))

#Vamos ahora con "Zona" el bonus malus: 
unique(auto232$Zona)
table(auto232$Zona)

#Ahora igual le vamos a dar el valor modal
#Al igual que antes son valores enteros, 
moda_Zona <- names(sort(table(auto232$Zona), decreasing = TRUE))[1]

auto232[is.na(auto232$Zona), ]$Zona <- moda_Zona

#2. TRANSFORMACIÓN BBDD Y DICOTOMIZACIÓN:

#b) Codificar las variables categóricas mediante variables dicotómicas (dummies), 
#indicando cuál se selecciona como categoría de referencia (baseline) para evitar 
#la multicolinealidad.

#Vale ahora que tenemos la base de datos, voy a ver los casos concretos 
#de cada columna del df.
names(auto232)
unique(auto232$Estado) #Transformar dicotomica 17
unique(auto232$Clase) #Transformar dicotomica 17
unique(auto232$Genero) #Transformar dicotomica
unique(auto232$Edad) #No tocar 
unique(auto232$Cuota) #No tocar
unique(auto232$Ant) #Por grupos 0, 1, 2, 3, 4-9, 10-15, 15-20, >20
unique(auto232$CV_vehi) #Lo dejaria como esta
unique(auto232$Valor) #Lo dejaria como esta
unique(auto232$BM) #Haría intervalos 5 grupos 
unique(auto232$Zona) #dicotomica

#Se pueden generar rápidamente las dicotómicas de la siguiente forma: 
dummies_estado <- model.matrix(~ Estado - 1, data = auto232)
dummies_clase <- model.matrix(~ Clase - 1, data = auto232)
dummies_genero <- model.matrix(~ Genero - 1, data = auto232)
dummies_zona <- model.matrix(~ Zona - 1, data = auto232)

#Ahora vamos con la antiguedad del individuo 
auto232$Ant_grupo <- cut(auto232$Ant,
  breaks = c(-Inf,0,1,2,3,9,15,20,Inf),
  labels = c("0", "1", "2", "3", "4-9",
             "10-15", "16-20", ">20"))
dummies_ant <- model.matrix(~ Ant_grupo - 1, data = auto232)

#Vamos ahora con el BM
auto232$BM_grupo <- cut(auto232$BM, breaks = quantile(auto232$BM,
                    probs = seq(0,1,0.2),  na.rm = TRUE),
                    include.lowest = TRUE)
dummies_BM <- model.matrix(~ BM_grupo - 1, data = auto232)

names(auto232)

auto232 <- cbind.data.frame(auto232, dummies_ant, 
                            dummies_BM, dummies_clase, 
                            dummies_estado, dummies_genero, 
                            dummies_zona)
names(auto232)

#Ahora voy anular las variables que me han permitido generar 
#las variables dummies
auto232$Ant <- NULL 
auto232$BM <- NULL 
auto232$Clase <- NULL 
auto232$Estado <- NULL 
auto232$Genero <- NULL 
auto232$Zona <- NULL 
auto232$BM_grupo <- NULL
auto232$Ant_grupo <- NULL

names(auto232)
modelo <- lm(LogCuota ~ -1 + ., data = auto232)
summary(modelo)
auto232$ZonaUrbana <- NULL 
auto232$GeneroM <- NULL
auto232$`EstadoSTATE 17` <- NULL 
auto232$ClaseF71 <- NULL
auto232$`BM_grupo(97,150]` <- NULL
auto232$`Ant_grupo>20` <- NULL
modelo <- lm(LogCuota ~ -1 + ., data = auto232)
summary(modelo)

#  a) Evaluar la conveniencia de incluir términos de interacción entre variables 
#explicativas.
auto232$potenciaEdad <- auto232$CV_vehi*auto232$Edad
auto232$Edad2 <- auto232$Edad^2
auto232$EdadGenero <- auto232$Edad*auto232$GeneroF

modelo <- lm(LogCuota ~ 1 + ., data = auto232)
summary(modelo)


modelo2 <- lm(LogCuota ~ + 1 + Cuota + `EstadoSTATE 03` + 
                `EstadoSTATE 04` + `EstadoSTATE 07` + GeneroF, data = auto232)

summary(modelo2)

#Vamos a chequear que los residuos de los modelos son normales o no 
#Para ver si la variable utilizada tiene sentido o no
par(mfrow = c(1,1))
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


#3. STEPWISE METHODS:
#a) Ajustar el modelo lineal MCO completo con todas las variables incluídas.
#modelo completo 
modelo

#b) Aplicar los métodos de selección automática Forward Stepwise y Backward Stepwise 
#basados en criterios de penalización (AIC / BIC).

#Partimos del modelo nulo (solo con constante)
modelo_nulo <- lm(LogCuota ~ +1, data = auto232)
summary(modelo_nulo)
#definimos modelo completo 
summary(modelo)

??stats::step
modelo_for <- step(modelo_nulo, scope = formula(modelo),
                   k = 2, #k = log(n) --- BIC
                   trace = 1, #para ver paso a paso selección variables
                   direction = "forward")
summary(modelo_for)

modelo_back <- step(modelo, 
                    k = 2, #k = log(n) --- BIC 
                    trace = 1, #para ver paso a paso selección variables
                    direction = "backward")
summary(modelo_back)


#c) ¿Producen ambos métodos (Forward y Backward) exactamente el mismo modelo final? 
#Comparar y comentar las posibles diferencias detectadas en la selección de predictores.

#modelo Forward
sum_for <- coef(summary(modelo_for))
df_for <- data.frame(Variable     = rownames(sum_for),
                     Coef_Forward = sum_for[, "Estimate"],
                     Pval_Forward = sum_for[, "Pr(>|t|)"])

#modelo Backward
sum_back <- coef(summary(modelo_back))
df_back <- data.frame(Variable      = rownames(sum_back), 
                      Coef_Backward = sum_back[, "Estimate"], 
                      Pval_Backward = sum_back[, "Pr(>|t|)"])

#modelo elegido por nosotros 
sum_mod <- coef(summary(modelo2))
df_mod <- data.frame(Variable     = rownames(sum_mod),
                     Coef_Mod = sum_mod[, "Estimate"],
                     Pval_Mod = sum_mod[, "Pr(>|t|)"])


# 3. Fusionamos ambos Data Frames incluyendo todas las variables seleccionadas (all = TRUE)
df_comparativa <- merge(df_for, df_back, 
                        by = "Variable", all = TRUE)

df_comparativa <- merge(df_comparativa, df_mod, 
                        by = "Variable", all = TRUE)

df_comparativa

#4. VISUALIZACIÓN Y REPRESENTACIÓN MARGINAL CON INTERVALOS DE CONFIANZA:
#  a) A partir del modelo elegido, realizar una visualización 
#gráfica de la predicción de la 'LogCuota' frente a la 'Edad' del asegurado/cartera
#de los datos reales frente a la estimación.

auto232$modelo_for <- fitted(modelo_for)
auto232$modelo_back <- fitted(modelo_back)
auto232$modelo_new <- fitted.values(modelo2)

edades <- sort(unique(auto232$Edad))
coste_real <- matrix(NA, nrow = length(edades), ncol = 1)
coste_forw <- matrix(NA, nrow = length(edades), ncol = 1)
coste_back <- matrix(NA, nrow = length(edades), ncol = 1)
coste_new <- matrix(NA, nrow = length(edades), ncol = 1)

i <- 1
for(i in 1:length(edades)){
  coste_real[i, ] <- mean(auto232[auto232$Edad == edades[i], ]$LogCuota)
  coste_forw[i, ] <- mean(auto232[auto232$Edad == edades[i], ]$modelo_for)
  coste_back[i, ] <- mean(auto232[auto232$Edad == edades[i], ]$modelo_back)
  coste_new[i, ] <- mean(auto232[auto232$Edad == edades[i], ]$modelo_new)
  
}

plot(edades, coste_real, type = "p", pch = 19, col = "blue", 
     xlab = "edades", ylab = "logCuota")
lines(edades, coste_forw, col = "red", lwd = 2, lty = 1)
lines(edades, coste_back, col = "green", lwd = 2, lty = 2)
lines(edades, coste_new, col = "darkgreen", lwd = 2, lty = 3)
legend("topleft", c("Real", "backward", "forward", "nuestro"), 
       col = c("blue", "red", "green", "darkgreen"), pch = c(19, NA, NA, NA), 
       lty = c(NA, 1, 2, 3), cex = 0.8, lwd = 2)

#b) Representar la curva de predicción ajustada junto con las observaciones reales 
#y añadir las bandas/intervalos de confianza del 95% proporcionados por el modelo.

#Vamos hacer las bandas de dos modelos del back y forward, pero se puede hacer con cualquiera
for_in <- predict(modelo_for, interval = "confidence", level = 0.95)
for_in

back_in <- predict(modelo_back, interval = "confidence", level = 0.95)
back_in

auto232$modelo_for_low <- for_in[, "lwr"]
auto232$modelo_for_upp <- for_in[, "upr"]
auto232$modelo_back_low <- back_in[, "lwr"]
auto232$modelo_back_upp <- back_in[, "upr"]

coste_back_low <- matrix(NA, nrow = length(edades), ncol = 1)
coste_back_upp <- matrix(NA, nrow = length(edades), ncol = 1)
coste_for_low <- matrix(NA, nrow = length(edades), ncol = 1)
coste_for_upp <- matrix(NA, nrow = length(edades), ncol = 1)

i <- 1
for(i in 1:length(edades)){
  coste_back_low[i, ] <- mean(auto232[auto232$Edad == edades[i], ]$modelo_for_low)
  coste_back_upp[i, ] <- mean(auto232[auto232$Edad == edades[i], ]$modelo_for_upp)
  coste_for_low[i, ] <- mean(auto232[auto232$Edad == edades[i], ]$modelo_back_low)
  coste_for_upp[i, ] <- mean(auto232[auto232$Edad == edades[i], ]$modelo_back_upp)
  
}

plot(edades, coste_real, type = "p", pch = 19, col = "blue", 
     xlab = "edades", ylab = "logCuota", main = "Intervalo - Backward")
lines(edades, coste_back, col = "red", lwd = 2, lty = 1)
lines(edades, coste_back_low, col = "red4", lwd = 2, lty = 1)
lines(edades, coste_back_upp, col = "red4", lwd = 2, lty = 1)

plot(edades, coste_real, type = "p", pch = 19, col = "blue", 
     xlab = "edades", ylab = "logCuota", main = "Intervalo - Backward")
lines(edades, coste_forw, col = "red", lwd = 2, lty = 1)
lines(edades, coste_for_low, col = "red4", lwd = 2, lty = 1)
lines(edades, coste_for_upp, col = "red4", lwd = 2, lty = 1)


#5. EVALUACIÓN DE LA CAPACIDAD PREDICTIVA (20-Fold Cross-Validation):
#a) Implementar un esquema de Validación Cruzada con K = 20 folds (20-Fold CV). 

#Vale ahora una vez que hemos decidido las variables predictivas, 
#vamos a ver como funciona en un 20k-fold

set.seed(2026)
K <- 20
folds <- sample(rep(1:K, length.out = length(auto232$Edad)))

RMSE_in_forw <- numeric(K)
RMSE_in_back <- numeric(K)
RMSE_in_new <- numeric(K)
RMSE_in_cte <- numeric(K)

RMSE_out_forw <- numeric(K)
RMSE_out_back <- numeric(K)
RMSE_out_new <- numeric(K)
RMSE_out_cte <- numeric(K)

k <- 1
for(k in 1:K){
  
  train <- auto232[folds != k, ]
  test  <- auto232[folds == k, ]
  
  modelo_forw <- lm(formula(modelo_for), data = train)
  modelo_back <- lm(formula(modelo_back), data = train)
  modelo_new <- lm(formula(modelo2), data = train)  
  modelo_cte <- lm(LogCuota ~ + 1, data = train)  
  
  pred_forw <- predict(modelo_forw, newdata = test)
  pred_back <- predict(modelo_back, newdata = test)
  pred_new <- predict(modelo_new, newdata = test)
  pred_cte <- predict(modelo_cte, newdata = test)
  
  RMSE_in_forw[k] <- sqrt(mean((train$LogCuota - fitted.values(modelo_forw))^2))
  RMSE_in_back[k] <- sqrt(mean((train$LogCuota - fitted.values(modelo_back))^2))
  RMSE_in_new[k] <- sqrt(mean((train$LogCuota - fitted.values(modelo_new))^2))
  RMSE_in_cte[k] <- sqrt(mean((train$LogCuota - fitted.values(modelo_cte))^2))
  
  RMSE_out_forw[k] <- sqrt(mean((test$LogCuota - pred_forw)^2))
  RMSE_out_back[k] <- sqrt(mean((test$LogCuota - pred_back)^2))
  RMSE_out_new[k] <- sqrt(mean((test$LogCuota - pred_new)^2))
  RMSE_out_cte[k] <- sqrt(mean((test$LogCuota - pred_cte)^2))
}

dentro <- data.frame(forward = mean(RMSE_in_forw), 
                     backward = mean(RMSE_in_back), 
                     new = mean(RMSE_in_new),
                     cte = mean(RMSE_in_cte))

fuera <- data.frame(forward = mean(RMSE_out_forw), 
                    backward = mean(RMSE_out_back), 
                    new = mean(RMSE_out_new),
                    cte = mean(RMSE_out_cte))

dentro
fuera

#b) Comparar la capacidad predictiva fuera de muestra entre:
#- Modelo Simple: LogCuota ~ 1 + Edad (1 -- cte)
#- Modelo Propuesto: Modelo lineal resultante del proceso de selección (Stepwise/Teórico).
#c) Calcular y comparar las siguientes medidas de bondad de ajuste y error predictivo 
#fuera de muestra en ambos modelos utilziando el 20-Fold CV:
#  * Suma de Cuadrados de Residuos (SSE)
#* Error Cuadrático Medio (MSE)
#* Error Porcentual Absoluto Medio (MAPE)
#* Coeficiente de Determinación (R^2)

