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
#contiene información sobre pólizas de automóviles con variables demográficas, 
#técnicas y de exposición.
#####################################################################
#APARTADOS A DESARROLLAR
#####################################################################

#1. PREPROCESAMIENTO DE LOS DATOS:
  
#a) Analizar la existencia de datos faltantes (NAs) en la base de datos `auto232`. 
#Indicar el número total de valores ausentes por variable y el porcentaje 
#que representan sobre la muestra.
#b) Justificar la estrategia de tratamiento aplicada para cada tipo de variable:
#  - ¿Qué decisión se debe tomar con las observaciones que presentan NA en la 
#variables tanto continúas como las variables categóricas?

#2. TRANSFORMACIÓN BBDD Y DICOTOMIZACIÓN:
#  a) Evaluar la conveniencia de incluir términos de interacción entre variables 
#explicativas.
#b) Codificar las variables categóricas mediante variables dicotómicas (dummies), 
#indicando cuál se selecciona como categoría de referencia (baseline) para evitar 
#la multicolinealidad.

#3. STEPWISE METHODS:
#a) Ajustar el modelo lineal MCO completo con todas las variables incluídas.
#b) Aplicar los métodos de selección automática Forward Stepwise y Backward Stepwise 
#basados en criterios de penalización (AIC / BIC).
#c) ¿Producen ambos métodos (Forward y Backward) exactamente el mismo modelo final? 
#Comparar y comentar las posibles diferencias detectadas en la selección de predictores.

#4. VISUALIZACIÓN Y REPRESENTACIÓN MARGINAL CON INTERVALOS DE CONFIANZA:
#  a) A partir del modelo elegido, realizar una visualización 
#gráfica de la predicción de la 'LogCuota' frente a la 'Edad' del asegurado/cartera
#de los datos reales frente a la estimación.
#b) Representar la curva de predicción ajustada junto con las observaciones reales 
#y añadir las bandas/intervalos de confianza del 95% proporcionados por el modelo.

#5. EVALUACIÓN DE LA CAPACIDAD PREDICTIVA (20-Fold Cross-Validation):
#a) Implementar un esquema de Validación Cruzada con K = 20 folds (20-Fold CV).
#b) Comparar la capacidad predictiva fuera de muestra entre:
#- Modelo Simple: LogCuota ~ 1 + Edad (1 -- cte)
#- Modelo Propuesto: Modelo lineal resultante del proceso de selección (Stepwise/Teórico).
#c) Calcular y comparar las siguientes medidas de bondad de ajuste y error predictivo 
#fuera de muestra en ambos modelos utilziando el 20-Fold CV:
#  * Suma de Cuadrados de Residuos (SSE)
#* Error Cuadrático Medio (MSE)
#* Error Porcentual Absoluto Medio (MAPE)
#* Coeficiente de Determinación (R^2)

