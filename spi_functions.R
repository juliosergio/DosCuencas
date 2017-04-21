###################################################
# Standardized Precipitation Index
# Joseph Wheatley Biospherica March 2010
################################################

if (!exists("LEIDO.MiBiblioteca")) source("RR/MiBiblioteca.R", chdir = T)


GammaParams0 <- function(x, niter=100, eps=0.001) {
    # Calcula los parámetros de la distribución Gamma con el método apuntado por 
    # Wikipedia, a partir de la Maximum Likelyhood 
    # https://en.wikipedia.org/wiki/Gamma_distribution#Cumulative_distribution_function
    mx <- mean(x)
    S <- (log(mx) - mean(log(x)))
    alpha <- (3-S+sqrt((S-3)^2+24*S))/(12*S) # Valor inicial
    # Otra aprox>>> alpha <- (1+sqrt(1+A4/3))/A4 # shape
    for (i in 1:niter) {
        # La nueva alpha (Newton-Raphson)
        alpha.n <- alpha - (log(alpha) - digamma(alpha) - S)/(1/alpha - psigamma(alpha, 1))
        delt <- abs(1-alpha.n/alpha)
        if (delt <= eps)
            break
        alpha <- alpha.n
    }
    beta <- mx/alpha.n            # scale
    return(c(shape=alpha.n, scale=beta, delt=delt, niter=i))
}


creaCumGamma <- function(x) {
    # Crea una función de distribución acumulativa Gamma a partir
    # de los datos proporcionados
    pp <- GammaParams0(x)
    function(x) pgamma(x, shape = pp[["shape"]], scale = pp[["scale"]])
}

creaGamma <- function(x) {
    # Crea una función de distribución Gamma a partir
    # de los datos proporcionados
    pp <- GammaParams0(x)
    function(x) dgamma(x, shape = pp[["shape"]], scale = pp[["scale"]])
}

creaCumECDF <- ecdf #* function(x) {
    #* Crea una función empírica acumulativa de distribución a
    #* partir de los datos dados
#*    ecdf(x)
#*}



getPrecOnTimescale <- function(precipitation, k, ini=1){
    # precipitation is a vector of monthly precipitation values
    # returns monthly precipation averaged over current month and prior k-1 months
    # JSS: Se incluye 'ini' para series que tienen datos antes del inicio; esto es ini, indica el 
    #      inicio de la serie que se desea analizar, p.ej., si ini=4, eso indica que los datos con
    #      índices entre 1 y 3 sólo servirán para calcular los promedios que comienzan a partir del
    #      índice 4
    Nt <- length(precipitation)
    # prec.k <- as.vector(sapply(seq(from=1, to=Nt),function(t) {tm <- max(t-k+1,1); mean(as.vector(precipitation[tm:t]))}))
    prec.k <- sapply(ini:Nt, function(t) {tm <- max(t-k+1,1); mean(precipitation[tm:t])})
    return(prec.k)
}


getSPIfromPrec <- function(precipitation){
    
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
        # empirical cdf's for each month 
        fit.cdf <- ecdf(mdata)
        #locate each data point on cdf by applying fit.cdf function for each location
        #cdf probabilites
        # these will be unformly distributed on interval 1/years
        cdfs <- fit.cdf(mdata)
        #invert normal
        spi.t <- qnorm(cdfs)
        spi.tp <- spi.t[ spi.t != Inf] #drop Inf
        ff <- function(x) (1-sd(c(x,spi.tp)))^2 # x reemplazaría a los que resultaron Inf
        #replace Inf with the value that sets sd(spi)=1 or mean = 0 (minimises ff)
        spi.t[spi.t==Inf] <- optimize(ff,lower=0,upper=100)$minimum
        # ensure mean is zero. spi.t is normally distributed with mean zero and sd approx 1
        spi.t <- spi.t - mean(spi.t)
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


spiGamma <- function(precipitation){
    
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


# La función que encuentra el SPI para una escala de tiempo es la 
# composición de las dos funciones anteriores:

getSPIfor_k <- getSPIfromPrec %cmp% getPrecOnTimescale  # Esto es: getSPIfor_k(prec, k), k=núm de meses
                                                        # o bien getSPIfor_k(prec, k, ini), ini=inicio de la serie

# Y si viene de un ajuste a una función de distribución Gamma:
 
getSPI.Gamma.for_k <- spiGamma %cmp% getPrecOnTimescale # Igual: getSPI.Gamma.for_k(prec, k), o bien
                                                        # getSPI.Gamma.for_k(prec, k, ini)


creaCumFuncts <- function(
    precipitation, # La precipitación
    ffcreadora     # funcion creadora de funciones una de {creaCumGamma, creaCumECDF}
                   #  o creaGamma, para función no acumulativa - para ilustraciones
) {
    # Crea la familia de funciones acumulativas de distribución
    # Una función por mes
    # a partir de la serie dada
    
    Nt <- length(precipitation)
    # Lp <- split(precipitation, rep_len(1:12,Nt)) # Precipitaciones por mes
    # lapply(Lp, ffcreadora)
    by(precipitation, rep_len(1:12,Nt), ffcreadora)
}

getGammaParams <- function (precipitation) {
    Nt <- length(precipitation)
    aggregate(precipitation, list(rep_len(1:12,Nt)), GammaParams0)$x
}

creaSPIFuncts <- function(
    precipitation, # la precipitacion
    ffcreadora,     # función creadora de funciones; una de {creaCumGamma, creaCumECDF}
    ...             # parametros adicionales (k, ini)
) {
    creaCumFuncts(getPrecOnTimescale(precipitation, ...), ffcreadora)
}

applyFuncts <- function(
    funcs,             # familia de funciones
    datos #,           # los datos: un vector
    # rule             # regla de aplicación
) {
    # Aplica cíclicamente las funciones sobre los datos
    oo <- getOption("warn")
    options(warn = -1)
    rr <- mapply(function(f, x) f(x), funcs, datos)
    options(warn = oo)
    return(rr)
}


renormaliza <- function(x) {
    # Elimina imperfecciones en una distribución normal
    x.tp <- x[ x != Inf] #drop Inf
    ff <- function(x) (1-sd(c(x, x.tp)))^2 # x reemplazaría a los que resultaron Inf
    #replace Inf with the value that sets sd(x)=1 or mean = 0 (minimises ff)
    x[x==Inf] <- optimize(ff,lower=0,upper=100)$minimum
    # ensure mean is zero. x is normally distributed with mean zero and sd approx 1
    return(x - mean(x))
}

getSPI <- function(precipitation, ffcreadora) {
    ss <- qnorm(
        applyFuncts(
            creaCumFuncts(precipitation, ffcreadora),
            precipitation
        )
    )
    if (any(is.infinite(ss))) {
        for (i in 1:12) {
            ii <- rep(F,12)
            ii[i] <- T
            ss[ii] <- renormaliza(ss[ii])
        }
    }
    return (ss)
}

# La siguiente función toma una serie de precipitaciones mensuales, hace los k-promedios y luego encuentra el SPI
getSPInew <- function(precipitation, ffcreadora, k, ...) getSPI(getPrecOnTimescale(precipitation, k, ...), ffcreadora)


test <- function() {
    #generate some sample recipitation values using a weibull distribution
    set.seed(2000)
    precipitation <- rweibull(500,2,1)
    
    #create a plot of SPI index vs timescale
    
    Nt<- length(precipitation)
    dates <- seq(from=1970,by=1/12,length.out=500)
    #detrend
    prec.fit <- lm(precipitation~dates)
    precipitation.detrend <- precipitation- fitted(prec.fit)
    
    spi <- sapply(1:12, function(i) getSPIfor_k(precipitation,i)) # 1:12 son cols a la salida
    # if you want to detrend use
    #spi <- sapply(1:12, function(i) getSPIfromPrec(getPrecOnTimescale(precipitation.detrend,i)))
    #instead
    
    spi.breaks <- c(-2.4,-2,-1.6,-1.3,-0.8,-0.5,0.5,0.8,1.3,1.6,2,2.4) # Son 11 intervalos
    spi.cols <- colorRampPalette(c("darkred","red","yellow","white","green","blue","darkblue"),space="rgb")
    filled.contour(dates,1:12,spi,col=spi.cols(11),xlab="",ylab="time-scale (months)",cex.lab=1.7,font.axis=2,font.lab=2,levels=spi.breaks,key.title="SPI")
    title(main="sample SPI",cex.main=2)
    
    # Hagamos prueba con la nueva forma de hacerlo
    
    nspi <- sapply(1:12, function(i) getSPInew(precipitation, creaCumECDF, i))
    
    filled.contour(dates,1:12,nspi,col=spi.cols(11),xlab="",ylab="time-scale (months)",cex.lab=1.7,font.axis=2,font.lab=2,levels=spi.breaks,key.title="SPI")
    title(main="NNNsample SPI",cex.main=2)
    
}