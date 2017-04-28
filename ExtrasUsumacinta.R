# ExtrasUsumacinta.R
library(MASS)

source("RR/MiBiblioteca.R", chdir = T)
source("RR/CoordinatedDrw.R", chdir = T)
source("spi_functions.R")

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

# Con la manera de calcular de Thom:
GammaParams <- function(x) {
    # Calcula los parámetros de la distribución Gamma con el método apuntado por 
    # Thom, H.C.S. (1966), a partir de la Maximum Likelyhood
    mx <- mean(x)
    A4 <- (log(mx) - mean(log(x)))*4
    alpha <- (1+sqrt(1+A4/3))/A4 # shape
    beta <- mx/alpha             # scale
    return(c(alpha, beta))
}



# Pgamma acumulado

pgammaX <- function(x) pgamma(x, shape = k, scale = th)

creaff <- function(ff, ...) function(x) ff(x, ...) # Creador de funciones c/1er. argumento

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


spi1 <- function(precipitation){
    
    #takes a vector of precipitation values 
    #and returns a vector of spi values
    
    
    Nt <- length(precipitation)
    #include full years data only
    years <- ceiling(Nt/12)
    #-NO_SE USA>>> full.years <- trunc(Nt/12)
    #-NO_SE USA>>> Nt.f <- full.years*12
    monthsInCurrentYear <- Nt %% 12
    #monthly analysis
    spi.o <- array(NA,c(years,12))
    for (m in 1:12)
    {
        mdata <- precipitation[seq(from=m, to=Nt, by=12)]
        
        # Cumulative Gamma cdf's for each month 
        # NO>>> fit.cdf <- ecdf(mdata)
        
        mn <- mean(mdata)
        v <- var(mdata)
        th <- v/mn
        k <- mn/th
        
        # Pgamma acumulado
        
        fit.cdf <- creaff(pgamma, shape = k, scale = th)
        
        #locate each data point on cdf by applying fit.cdf function for each location
        #cdf probabilites
        # these will be unformly distributed on interval 1/years
        cdfs <- fit.cdf(mdata)
        #invert normal
        spi.t <- qnorm(cdfs)
        # NO-NECESARIO>> spi.tp <- spi.t[ spi.t != Inf] #drop Inf
        # NO-NECESARIO>> ff <- function(x) (1-sd(c(x,spi.tp)))^2 # x reemplazaría a los que resultaron Inf
        # NO-NECESARIO>> #replace Inf with the value that sets sd(spi)=1 or mean = 0 (minimises ff)
        # NO-NECESARIO>> spi.t[spi.t==Inf] <- optimize(ff,lower=0,upper=100)$minimum
        # NO-NECESARIO>> # ensure mean is zero. spi.t is normally distributed with mean zero and sd approx 1
        # NO-NECESARIO>> spi.t <- spi.t - mean(spi.t)
        #>>> ifelse( !(monthsInCurrentYear==0),ifelse (m <=monthsInCurrentYear, spi.o[,m]<-spi.t, spi.o[,m]<- c(spi.t,0) ), spi.o[,m]<-spi.t)
        # Lo anterior, en R estándar:
        spi.o[,m] <- if ((monthsInCurrentYear)&(monthsInCurrentYear < m)) c(spi.t,0) else spi.t
    }
    
    #>>> spi <- array(0,Nt)
    #>>> 
    #>>> for ( t in 1:Nt){
    #>>>     month <- (t-1)%%12 + 1
    #>>>     year <- 1 + trunc((t-1)/12)
    #>>>     spi[t] <- spi.o[year,month]
    #>>>     
    #>>> }
    #>>> return(spi)
    return(as.numeric(t(spi.o))[1:Nt]) # <- JSS: Más fácil así.
}





ctg <- factor(c("Gamma (mm/day)", "Normal"), levels = c("Gamma (mm/day)", "Normal"))

a <- data.frame(x=c(3,9), category=ctg[1])
b <- data.frame(x=c(-4,4), category=ctg[2])
miXa <- c(6.3, 6.3, 9); miYa <- c(0, pgammaX(6.3), pgammaX(6.3))
miXb <- c(-4, spi0(6.3), spi0(6.3)); miYb <- c(pgammaX(6.3), pgammaX(6.3), 0)
c <- data.frame(x=miXa, y=miYa, category=ctg[1])
d <- data.frame(x=miXb, y=miYb, category=ctg[2])


f <- ggplot(a, aes(x)) + 
    stat_function(fun=pgammaX) + 
    stat_function(data = b, mapping = aes(x), fun = pnorm) +
    ylab("Probability")

f <- f + 
    geom_path(data=c, aes(x,y), arrow = arrow(), linetype="dashed", colour="red") + 
    geom_path(data=d, aes(x,y), arrow = arrow(), linetype="dashed", colour="red") +
    geom_text(data=data_frame(x=c(6.5,8.7), y=c(0,0.86), label=c("6.3", "0.797"), 
                              category=ctg[1]), 
              aes(x,y,label=label), inherit.aes=FALSE, colour="red") +
    geom_text(data=data_frame(x=c(-4,1.4), y=c(0.86,0), label=c("0.797", "0.832"), 
                              category=ctg[2]), 
              aes(x,y,label=label), inherit.aes=FALSE, colour="red")
    

f + facet_wrap("category", scales = "free_x") + xlab("")


# La serie de SPIs correspondiente a la serie de promedios

dd$spi <- spi0(dd$precProm)

# p <- DrwSeries(dd, 4)
# p

dd$ecdf.spi <-getSPIfromPrec(dd$precProm)

# Con la otra gamma

dd$AltG.spi <- spiGamma(dd$precProm)

q <- DrwSeries(dd, 4:6, scales="fixed")
q

# La nueva implementación de SPI para dist. Gamma
dd$NG.spi <- getSPI(dd$precProm, creaCumGamma)
r <- DrwSeries(dd, c(7,5), c("Gamma.SPI","ECDF.SPI"), xlab = "Años", ylab = "Valor", scales = "fixed")
r + geom_vline(aes(xintercept = as.numeric(as.Date.character("1985-01-01"))), colour="darkred", linetype=2)
ggsave("SPI.Usumacinta2CF.png")


ffs <- creaCumFuncts(dd$precProm, creaCumGamma)
ff0 <- creaCumFuncts(dd$precProm, creaGamma)


# Dibujado de ff0

meses <- sprintf("%02d", 1:12)
p9 <- Reduce(
    function(x, y) {
        # i <- length(x$layers)+1
        x + stat_function(fun = ff0[[y]], 
                          aes_(colour = meses[y]))
    }, 
    1:length(ff0), 
    init = ggplot(data.frame(x = range(dd$precProm)), aes(x = x))
)
p9 + 
    scale_x_continuous(name = "Precipitación (mm/día)") + # ,
    #breaks = seq(0, 1, 0.2),
    # limits=c(0, 1)) +
    scale_y_continuous(name = "Densidad") +
    ggtitle("Gammas") +
    scale_colour_brewer(palette="Paired") +
    labs(colour = "Mes")  #+
    # guides(colour = guide_legend(order=3))
ggsave("FamiliaGammas.png")

# Ahora las acumuladas
p9 <- Reduce(
    function(x, y) {
        # i <- length(x$layers)+1
        x + stat_function(fun = ffs[[y]], 
                          aes_(colour = meses[y]))
    }, 
    1:length(ffs), 
    init = ggplot(data.frame(x = range(dd$precProm)), aes(x = x))
)
p9 + 
    scale_x_continuous(name = "Precipitación (mm/día)") + # ,
    #breaks = seq(0, 1, 0.2),
    # limits=c(0, 1)) +
    scale_y_continuous(name = "Probabilidad") +
    ggtitle("Gammas Acumuladas") +
    scale_colour_brewer(palette="Paired") +
    labs(colour = "Mes")  #+
# guides(colour = guide_legend(order=3))
ggsave("FamiliaGammasAcc.png")

# **************************************************************
# Otra aproximación al problema, agregando una lista de geoms al
# plot inicial:

# Aquí la función que se quiera:
f0 <- ff0 
# f0 <- ffs

plotDCurves <- function(f0, title="", ylab="") {
    ggplot(data.frame(x = range(dd$precProm)), aes(x)) + 
        lapply(1:length(f0), 
               function(ii) {stat_function(mapping = aes_(colour=meses[ii]),
                                           fun = f0[[ii]] )}) +
        scale_x_continuous(name = "Precipitación (mm/día)") + 
        scale_y_continuous(name = ylab) +
        ggtitle(title) +
        scale_colour_brewer(palette="Paired") +
        labs(colour = "Mes") 
}

# Las curvas hechas antes ahora con la nueva función

plotDCurves(ff0, "Gammas", "Densidad") 

plotDCurves(ffs, "Gammas Acumuladas", "Probabilidad")

# Hagamos ahora lo propio con las funciones ECDF

ffs <- creaCumFuncts(dd$precProm, creaCumECDF)
plotDCurves(ffs, "ECDFs (Acumuladas)", "Probabilidad")
ggsave("FamiliaECDFsAcc.png")

# Histograma
br <- c(-3, -2, -1.5, -1, 1, 1.5, 2, 3)
Mcols <- colorRampPalette(c("darkred","red","sandybrown","cornsilk2","lightblue","royalblue3","darkblue"),space="rgb")
Scols <- Mcols(length(br))

(h <- hist(dd$ecdf.spi, breaks = br, col = Scols, xlab = "SPI", ylab = "Densidad", labels = T, main = "", ylim = c(0,dnorm(0))))
curve(dnorm, lwd=2, col="navyblue", add = T)

