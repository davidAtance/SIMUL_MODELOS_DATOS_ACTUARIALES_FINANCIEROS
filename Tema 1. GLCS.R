#Numeros aleatorios
a <- 5
c <- 3
m <- 16
semilla <- 7

xi <- matrix(NA, nrow = 10000, ncol = 1)
xi[1] <- semilla

xi[2] <- (a * xi[1] + c) - m*floor((a*xi[1] + c)/m)

xi[1:2]

for(i in 2:10000){
  xi[i] <- (a * xi[(i-1)] + c) - m*floor((a*xi[(i-1)] + c)/m)
}
xi[1:30] #ver el numero de datous que repite

min(xi)
max(xi)
mean(xi)
sd(xi)

ui <- xi/m


#histogramas de los valores uniformes generados 
hist(xi, breaks = 20, col = "lightblue",
     border = "white", main = "Histograma del GCLS", xlab = "Valor")


#histogramas de los valores uniformes generados 
hist(ui, breaks = 20, col = "lightblue",
     border = "white", main = "Histograma del GCLS", xlab = "Valor")

plot(ui[-5000],ui[-1],pch=19)

mean(ui) #approx 0.5 
var(ui) #approx 1/12

#La misma forma pero generando una función 
# Generador Congruencial Lineal Simple (GCLS)
gcls <- function(semilla, a, c, m, n) {
  
  x <- numeric(n + 1)
  u <- numeric(n)
  
  x[1] <- semilla
  
  for(i in 1:n){
    x[i + 1] <- (a * x[i] + c) %% m
    u[i] <- x[i + 1] / m
  }
  
  return(list(X = x, U = u))
}

#Numero generados por Park y Miller
#que ya cumplen las restricciones que exigimos.
a <- 16807
c <- 0
m <- 2^31 - 1
semilla <- 12345

resultado <- gcls(semilla, a, c, m, 1000)

resultado$X
resultado$U

par(mfrow = c(1,2))
hist(resultado$X, breaks = 20, col = "lightblue",
     border = "white", main = "Histograma del GCLS", xlab = "Valor")


#histogramas de los valores uniformes generados 
hist(resultado$U, breaks = 20, col = "lightblue",
     border = "white", main = "Histograma del GCLS", xlab = "Valor")

par(mfrow = c(1,1))
plot(resultado$U[-1000], resultado$U[-1], pch=19)

#################################################################3
#GENERADOR PROPIO DE R
#################################################################3
RNGkind() #repetición en 2^(19937)−1,

#Se puede cambiar haciendo RNGkind() y dentro del parentesis el metodo que 
#quieres que se empiece a utilizar 
?RNGkind


#Generadores uniformes
set.seed(123) #semilla1
u1 <- runif(10000)

set.seed(999) #semilla2
u2 <- runif(10000)

c(mean(u1), mean(u2))
c(var(u1), var(u2))

par(mfrow = c(1,2))

hist(u1, main="Semilla = 123", col="lightblue")
hist(u2, main="Semilla = 999", col="lightgreen")
  
