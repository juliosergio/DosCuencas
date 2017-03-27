# ExtrasUsumacinta.R
library(MASS)

source("RR/MiBiblioteca.R", chdir = T)
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

spi0 <- qnorm %cmp% pgammaX

ctg <- factor(c("Gamma (mm/día)", "Normal"), levels = c("Gamma (mm/día)", "Normal"))

a <- data.frame(x=c(3,9), category=ctg[1])
b <- data.frame(x=c(-4,4), category=ctg[2])
miXa <- c(6.3, 6.3, 9); miYa <- c(0, pgammaX(6.3), pgammaX(6.3))
miXb <- c(-4, compf(6.3), compf(6.3)); miYb <- c(pgammaX(6.3), pgammaX(6.3), 0)
c <- data.frame(x=miXa, y=miYa, category=ctg[1])
d <- data.frame(x=miXb, y=miYb, category=ctg[2])


f <- ggplot(a, aes(x)) + 
    stat_function(fun=pgammaX) + 
    stat_function(data = b, mapping = aes(x), fun = pnorm) +
    ylab("Probabilidad")

f <- f + 
    geom_path(data=c, aes(x,y), arrow = arrow(), linetype="dashed", colour="red") + 
    geom_path(data=d, aes(x,y), arrow = arrow(), linetype="dashed", colour="red") +
    geom_text(data=data_frame(x=c(6.5,8.7), y=c(0,0.86), label=c("6.3", "0.797"), 
                              category=ctg[1]), 
              aes(x,y,label=label), inherit.aes=FALSE, colour="red") +
    geom_text(data=data_frame(x=c(-4,1.4), y=c(0.86,0), label=c("0.797", "0.832"), 
                              category=ctg[2]), 
              aes(x,y,label=label), inherit.aes=FALSE, colour="red")
    

f + facet_wrap("category", scales = "free_x")


# La serie de SPIs correspondiente a la serie de promedios

dd$spi <- spi0(dd$precProm)

p <- DrwSeries(dd, 4)
p



