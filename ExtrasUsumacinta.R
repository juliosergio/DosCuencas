# ExtrasUsumacinta.R
library(MASS)

source("RR/CoordinatedDrw.R", chdir = T)
dir <- getwd()
setwd("RR")
test()
setwd(dir)
load("UsumacintapreAcc.RData") # El objeto es 'dd'
pp <- dd$precProm # Precipitación promedio
m <- mean(pp)
v <- var(pp)
th <- v/m
k <- m/th
dgammaX <- function(x) dgamma(x, shape = k, scale = th)
ff <- fitdistr(pp, dgamma, start = list(shape=10, scale=0.1)) # Maximum likelyhood
dgammaXX <- function(x) dgamma(x, shape = ff$estimate[["shape"]], scale = ff$estimate[["scale"]])

# Pgamma acumulado
pgammaX <- function(x) pgamma(x, shape = k, scale = th)


# Histograma
hist(pp, freq = F, breaks = 20, 
     xlab = "PrecProm-12", ylab = "Densidad", 
     main = "Histograma Precip. Promedio y Gammas", col="gray")


curve(dgammaX, add = T, col="blue")
curve(dgammaXX, add = T, col="red", lty=2)
legend("topright", c("Método parámetros", "Método Máx V."), 
        lwd=1, col=c("blue", "red"), lty=c(1,2))

# Gráfico Acumulado
curve(pgammaX, lwd=2, xlim = c(3,9), xlab="prec (mm/día)", ylab = "prob")

# o en ggplot
f <- ggplot(data.frame(x=c(3, 9)), aes(x)) + stat_function(fun=pgammaX)
f



#------------
# library(ggplot2)

# pgammaX <- function(x) pgamma(x, shape = 64.57849, scale = 0.08854802)

compf <- qnorm %cmp% pgammaX

a <- data.frame(x=c(3,9), category="Gamma")
b <- data.frame(x=c(-4,4), category="Normal")
miXa <- c(6.3, 6.3, 9.5); miYa <- c(-0.05, pgammaX(6.3), pgammaX(6.3))
miXb <- c(-5.5, compf(6.3), compf(6.3)); miYb <- c(pgammaX(6.3), pgammaX(6.3), -0.05)
c <- data.frame(x=miXa, y=miYa, category="Gamma")
d <- data.frame(x=miXb, y=miYb, category="Normal")


f <- ggplot(a, aes(x)) + stat_function(fun=pgammaX) + stat_function(data = b, mapping = aes(x), fun = pnorm)

f <- f + geom_path(data=c, aes(x,y), arrow = arrow()) + geom_path(data=d, aes(x,y), arrow = arrow())

f + facet_wrap("category", scales = "free_x")


