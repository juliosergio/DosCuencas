###################################################
# TandemSPI.R
#     Aplica el calclulo del SPI a un conjunto de
#     puntos, c/u de los cuales tiene asociada una
#     serie de tiempo (mensual) de precipitaciones
################################################

###################################################
# Standardized Precipitation Index
# Joseph Wheatley Biospherica March 2010
################################################


source("spi_functions.R")
#debugSource("spi_functions.R") # Si se quiere correr esta parte con debbuger


#Intall the NetCDF libraries: sudo apt-get install libnetcdf-dev 
#Install the ncdf package in R: install.packages("ncdf")

# library("ncdf4")


ncname <- "CLICOM_pre_1960-2008_mm"  
ncfname <- paste("CLICOMg/",ncname,".nc", sep="")
#outname <- "ccg_usumacinta_allmes.csv"
dname <- "pre" 

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
nt <- dim(t)-12   # Para quitar el 2008 y sean dos per?odos de 24 a?os.
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

longitude <- as.numeric(mustGet("Longitud:", "-105.8125"))
# longitude <- -105.8125 #range -180, 180 ##LonLat Punto TARGET
latitude <- as.numeric(mustGet("Latitud:", "27.1875"))
# latitude <- 27.1875 # range -90,90
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

spi <- sapply(1:12, function(i) getSPIfor_k(pdata1,i))
# if you want to detrend use
#spi <- sapply(1:12, function(i) getSPIfromPrec(getPrecOnTimescale(prec.detrend,i)))
#instead

# En el ?ltimo l?mite de spi.breaks ajustarlo para que sea consistente y no haya errores en histogramas
spi.breaks <- c(-2.5,-2,-1.6,-1.3,-0.8,-0.5,0.5,0.8,1.3,1.6,2,2.5)
spi.cols <- colorRampPalette(c("darkred","red","yellow","white","green","blue","darkblue"),space="rgb")
filled.contour(dates,seq(1:12),spi,col=spi.cols(11),xlab="",ylab="Escala de Tiempo (meses)",cex.lab=1.7,font.axis=2,font.lab=2,levels=spi.breaks,key.title="SPI")
title(main="SPI - Conchos 3",cex.main=2)

### Now PDFs for SPI ###

pdf_spi_6083 <- dnorm(spi[1:288,12],0,1)
pdf_spi_8407 <- dnorm(spi[289:576,12],0,1)
plot(spi[1:288,12], pdf_spi_6083, col="red",xlab="", ylab="Density", type="p",lwd=1, cex=1, main="PDF of Standard Normal", cex.axis=.8)
plot(spi[289:576,12], pdf_spi_8407, col="blue",xlab="", ylab="Density", type="p",lwd=3, cex=1, main="PDF of Standard Normal", cex.axis=.8)

hist(spi[1:288,12], breaks=spi.breaks)
hist(spi[289:576,12], breaks=spi.breaks)
#curve(dnorm(x, mean(pdf_spi_6083), sd(pdf_spi_6083)), add=TRUE, col="darkblue", lwd=2)


