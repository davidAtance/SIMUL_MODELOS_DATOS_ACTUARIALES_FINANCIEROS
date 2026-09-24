#Metodo Tabla de Busqueda
#Vamos a suponer que: 
#Estado   Probabilidad
#Sin impago -- 0.75
#Retraso    -- 0.15
#Impago ----- 0.10
#Datos de una cartera de gestión de cobros de primas

#Este hecho nos generaría la siguiente regla de decisión: 
#X = 1 => 0 <= U <= 0.75 (sin impago)
#X = 2 => 0.75 <= U <= 0.90  (retraso)
#X = 3 => 0.90 <= U <= 1.00  (impago)

#establecemos semillas para los aleatorios
set.seed(123)

#Número de individuso a anlizar
n <- 10000

#Generamos los valores  Uniformes-U(0,1)
u <- runif(n)

mean(u)
var(u)
hist(u)
plot(u)

# Método de búsqueda
estado <- ifelse(u < 0.75, 1,
                 ifelse(u < 0.90, 2, 3))

table(estado) #casos para cada situacion 
prop.table(table(estado)) #porcentaje de cada situacion 
#Comparacion teórica frente a real 
teoricas <- c(0.75,0.15,0.10)

#Visualizacion grafica
barplot(prop.table(table(estado)),
        col = c("forestgreen", "gold", "firebrick"),
        main = "Estado de los préstamos simulados",
        ylab = "Probabilidad",
        names.arg = c("Sin impago", "Retraso", "Default"))

simuladas <- prop.table(table(estado))

rbind(
  Teorica = teoricas,
  Simulada = round(simuladas,4))

###################################################################
#EJERCICIO EXTRA
#Sea un generador congruente lineal simple cuyos parámetros son: 
#a=5,  c=3,m=2^{20}+5.
#Se pide: 
#1. Genere 10000 números aleatorios correspondientes a una $U(0,1)$ 
#sabiendo que $x_0$ se define como AAMMDD, 
#haciendo referencia a A, M y D al año, mes y día de su nacimiento 
#(Ejemplo: el 1 de febrero de 1994, $x_0$ será 940201). 

#2. Se sabe que la variable $X$ es de naturaleza discreta y 
#sigue la siguiente función de cuantía: 
#X | p(X=x)
#0 | 0.25
#3 | 0.15
#4 | 0.00
#5 | 0.40
#6 | 0.20
#
#A partir de los resultados del ejercicio anterior genere 
#10000 valores de la variable coherentes con esta función de cuantía. 



