###################################################
# A Standardized Precipitation Index for
# NCEP precipitation data http://www.esrl.noaa.gov/psd/data/gridded/data.ncep.reanalysis.derived.surfaceflux.html
# Joseph Wheatley Biospherica March 2010
################################################

###################################################
# Standardized Precipitation Index
# Joseph Wheatley Biospherica March 2010
################################################


getPrecOnTimescale <- function(precipitation,k){
# precipitation is a vector of monthly precipitation values
# returns monthly precipation averaged over current month and prior k-1 months
Nt <- length(precipitation)
prec.k <- as.vector(sapply(seq(from=1, to=Nt),function(t) {tm <- max(t-k+1,1); mean(as.vector(precipitation[tm:t]))}))
return(prec.k)
}


getSPIfromPrec <- function(precipitation){

#takes a vector of precipitation values 
#and returns a vector of spi values


Nt <- length(precipitation)
#include full years data only
years <- ceiling(Nt/12)
full.years <- trunc(Nt/12)
Nt.f <- full.years*12
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
  cdfs <- as.vector(sapply(mdata,fit.cdf))
  #invert normal
  spi.t <- qnorm(cdfs)
  spi.tp <- spi.t[ spi.t != Inf] #drop Inf
  ff <- function(x) (1-sd(c(x,spi.tp)))^2
  #replace Inf with the value that sets sd(spi)=1 or mean = 0 (minimises ff)
  spi.t[spi.t==Inf] <- optimize(ff,lower=0,upper=100)$minimum
  # ensure mean is zero. spi.t is normally distributed with mean zero and sd approx 1
  spi.t <- spi.t - mean(spi.t)
  ifelse( !(monthsInCurrentYear==0),ifelse (m <=monthsInCurrentYear, spi.o[,m]<-spi.t, spi.o[,m]<- c(spi.t,0) ), spi.o[,m]<-spi.t)
}

spi <- array(0,c(Nt))

for ( t in 1:Nt){
  month <- (t-1)%%12 + 1
  year <- 1 + trunc((t-1)/12)
  spi[t] <- spi.o[year,month]
  
}
return(spi)
}

###

#Intall the NetCDF libraries: sudo apt-get install libnetcdf-dev 
#Install the ncdf package in R: install.packages("ncdf")

library("ncdf4")
require(graphics)
require(Hmisc)

# Set your working directory
# getwd()
# workdir <- paste("f:/r/spi/",sep="")
# setwd(workdir)

ncname <- "CLICOM_pre_1960-2008_mm"  
ncfname <- paste("../CLICOMg/",ncname,".nc", sep="")
#outname <- "ccg_usumacinta_allmes.csv"
dname <- "pre"  # note: tmp means temperature (not temporary)

# open a netCDF file
ncin <- nc_open(ncfname)
print(ncin)

lons <- ncvar_get(ncin,"longitude")
nlon <- dim(lons)
head(lons)

lats <- ncvar_get(ncin,"latitude",verbose=F)
nlat <- dim(lats)
head(lats)

t <- ncvar_get(ncin,"time")

tunits <- ncatt_get(ncin,"time","units")
nt <- dim(t)-12   # Para quitar el 2008 y sean dos períodos de 24 años.
nt

tunits

tmp_array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"units")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(tmp_array)

title <- ncatt_get(ncin,0,"title")
institution <- ncatt_get(ncin,0,"institution")
datasource <- ncatt_get(ncin,0,"source")
references <- ncatt_get(ncin,0,"references")
history <- ncatt_get(ncin,0,"history")
Conventions <- ncatt_get(ncin,0,"Conventions")

nc_close(ncin)

ls()

###

ppt <- tmp_array[,,1:nt]

#data for selected location
#################################
#longitude <- 145 #range 0, 360
#latitude <- -30 # range -90,90
longitude <- -105.8125 #range -180, 180 ##LonLat Punto TARGET
latitude <- 27.1875 # range -90,90
#longitude <- -105.8125 #range -180, 180 ##LonLat CONCHOS
#latitude <- 27.1875 # range -90,90
#longitude <- -92.0333 #range -180, 180 ##LonLat La Independencia (Chis)
#latitude <- 16.25 # range -90,90
#longitude <- -91.7666 #range -180, 180 ##LonLat Emiliano Zapata (Tab)
#latitude <- 17.7333 # range -90,90
#longitude <- -91.5333 #range -180, 180 ##LonLat Balancan (Tab)
#latitude <- 17.8 # range -90,90
#################################
#find data closest grid cell
pdata1 <- ppt[which.min((lons-longitude)^2),which.min((lats-latitude)^2),1:nt]

#create a plot of SPI index vs timescale

dates <- seq(from=1960,by=1/12,length.out=nt)
#detrend
prec.fit <- lm(pdata1~dates)
prec.detrend <- pdata1- fitted(prec.fit)

spi <- sapply(1:12, function(i) getSPIfromPrec(getPrecOnTimescale(pdata1,i)))
# if you want to detrend use
#spi <- sapply(1:12, function(i) getSPIfromPrec(getPrecOnTimescale(prec.detrend,i)))
#instead

# En el ?ltimo l?mite de spi.breaks ajustarlo para que sea consistente y no haya errores en histogramas
spi.breaks <- c(-2.5,-2,-1.5,-1.0,1.0,1.5,2,2.5)
spi.cols <- colorRampPalette(c("darkred","red","yellow","white","green","blue","darkblue"),space="rgb")
filled.contour(dates,seq(1:12),spi,col=spi.cols(7),xlab="",ylab="Escala de Tiempo (meses)",cex.lab=1.7,font.axis=2,font.lab=2,levels=spi.breaks,key.title="SPI")
title(main="SPI - Emiliano Zapata (Tab)",cex.main=2)

### Now PDFs for SPI ###

pdf_spi_6083 <- dnorm(spi[1:288,12],0,1)
pdf_spi_8407 <- dnorm(spi[289:576,12],0,1)
plot(spi[1:288,12], pdf_spi_6083, col="red",xlab="", ylab="Density", type="p",lwd=1, cex=1, main="PDF of Standard Normal", cex.axis=.8)
plot(spi[289:576,12], pdf_spi_8407, col="blue",xlab="", ylab="Density", type="p",lwd=3, cex=1, main="PDF of Standard Normal", cex.axis=.8)

hist(spi[1:288,12], breaks=spi.breaks)
hist(spi[289:576,12], breaks=spi.breaks)
#curve(dnorm(x, mean(pdf_spi_6083), sd(pdf_spi_6083)), add=TRUE, col="darkblue", lwd=2)
plot(dates,spi[,12], type = "l", xlab = "Periodo (1960-2007)", ylab = "SPI")
abline(h = 1)
abline(h = -1)
abline(h = 1.5, lty = 2)
abline(h = -1.5, lty = 2)
abline(h = 2, lty = 3)
abline(h = -2, lty = 3)

