#
#dbinom(n,p) --> f(x)
#pbinom(r, p)--> F(x)
#qbinom

#pintar curva normal
mu <- 95.3
sigma <- 5.7
curve(dnorm(x, mean =mu, sd=sigma), xlim = c(80,120))

# P(X<90)=F(90)
pnorm(90, mu, sigma)

#hacer una enquesta de 1 test, da un valor aleatorio dentro del ramgo
#set seed para que nos de a todos el mismo valor
set.seed(123)
rnorm(1, mu,sigma)

# a) muestra aleatoria de 4 cables, valor esperado de la suma de las resistencias

#hacemos enquesta de 4 - muestra aleatoria tamaño 4
rnorm(4, mu, sigma)

#E(sum(Xi)) -- E(Y)
# el function me coge los cuatro cables y me calcula la resistencia total en i cables
Y <- function(i)sum(rnorm(4, mu, sigma))
Y10000 <- sapply(1:10000, Y)
mean(Y10000)
var(Y10000)
hist(Y10000)
#estoy haciendo 10000 experimentos donde cojo 4 cables y calcula la resistencia total

#en teoria Y-> N(n*mu, n*sigma^2)
#la media de la suma muestral
4*mu
# varianza de la suma muestral
4*sigma^2

hist(Y10000, freq=FALSE)
curve(dnorm(x, mean=4*mu, sd= sqrt(4)*sigma), add=TRUE)

#b)
Y <- function(i)sum(rnorm(1000, mu, sigma))
Y10000 <- sapply(1:10000, Y)
var(Y10000)

#en teoria


#c)un cable aleatorio, prob de que NO sea menor a 103
1-pnorm(103, mu, sigma)

#otra forma
Y <- function(i)sum(rnorm(1, mu, sigma))
Y10000 <- sapply(1:10000, Y)
#el True es 1
mean(Y10000>103)


#d) 
Xbar <- function(i)mean(rnorm(4, mu, sigma))
Xbar100000 <- sapply(1:100000, Xbar)
#el True es 1
mean(Xbar100000<98)


#rapido P(Xbar<98)=pnorm (x, mu , sigma/sqrt(n))
pnorm (98, mu, sigma/sqrt(4) )

#e) hay que mirar foto del mobil 
S2 <- function(i)var(rnorm(100, mu, sigma))
S2100000 <- sapply(1:100000, S2)
#el True es 1
mean(S2100000>32)

#rapido 1 -pchisq ((n-1)*x/sigma^2, n-1)
# tiende a chisq
1-pchisq((100-1)*32/sigma^2, 100-1)
