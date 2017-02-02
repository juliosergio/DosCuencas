###################################################
# Standardized Precipitation Index
# Joseph Wheatley Biospherica March 2010
################################################

if (!exists("LEIDO.MiBiblioteca")) source("RR/MiBiblioteca.R", chdir = T)


getPrecOnTimescale <- function(precipitation,k){
    # precipitation is a vector of monthly precipitation values
    # returns monthly precipation averaged over current month and prior k-1 months
    Nt <- length(precipitation)
    # prec.k <- as.vector(sapply(seq(from=1, to=Nt),function(t) {tm <- max(t-k+1,1); mean(as.vector(precipitation[tm:t]))}))
    prec.k <- sapply(1:Nt, function(t) {tm <- max(t-k+1,1); mean(precipitation[tm:t])})
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
        cdfs <- sapply(mdata,fit.cdf)
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

# La función que encuentra el SPI para una escala de tiempo es la 
# composición de las dos funciones anteriores:

getSPIfor_k <- getSPIfromPrec %cmp% getPrecOnTimescale  # Esto es: getSPIfor_k(prec, k), k=núm de meses


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
}