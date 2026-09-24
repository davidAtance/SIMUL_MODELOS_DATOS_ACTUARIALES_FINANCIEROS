#Distribucion exponencial -- 
#F(x) = 1- exp(-lambda) => U = F(x); 
# x = -ln(1-u)/lambda

set.seed(123)
n <- 10000

lambda <- 2
u <- runif(n)
x <- -log(u)/lambda

par(mfrow = c(1,1))
hist(x, probability = TRUE, breaks = 30, col = "lightblue")
curve(dexp(x, rate = lambda), add = TRUE, col = "red", lwd = 2)

mean(x) #approx 1/lambda
var(x) #approx 1/lambda^2

##############################################################3
#Distribucion de Weibull
#F(x) = 1 - exp-{x/beta}^alpha => F(x) = U
#x = [-ln(1-u)]^{1/alpha} · beta

set.seed(123)
u <- runif(10000)
alpha <- 2
beta  <- 3

x <- (-log(1-u))^(1/alpha)*beta

hist(x, probability = TRUE, breaks = 30,col = "lightblue")
curve(dweibull(x, shape = alpha, scale = beta),
      add = TRUE, col = "red", lwd = 2)

##################################################################3
#En R ciertas distribuciones se pueden hacer directamente con: 
qexp()
qweibull()
qlogis()
qcauchy()

#vamos a mostrar un ejemplo de la diferencia de la exponencial
#creada por nosotros y la hecha por R 
#Funcion exponencial
set.seed(123)
n <- 10000
lambda <- 2

# Uniformes
u <- runif(n)

# Método de la inversión
x.inv <- -log(1-u)/lambda

# Utilizando qexp()
x.qexp <- qexp(u, rate = lambda)

#Comprobamos la media 
mean(x.inv)
mean(x.qexp)

var(x.inv)
var(x.qexp)

#histogramas: 
par(mfrow = c(1,2))

hist(x.inv, probability = TRUE, breaks = 30,
     col = "lightblue", main = "Método de la inversión")
curve(dexp(x, rate = lambda), add = TRUE, col = "red",
      lwd = 2)

hist(x.qexp, probability = TRUE, breaks = 30,
     col = "lightgreen", main = "qexp()")
curve(dexp(x, rate = lambda), add = TRUE,
      col = "red", lwd = 2)
