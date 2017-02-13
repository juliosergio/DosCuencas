###################################################
# TotalSPIs.R
#     Aplica el calclulo del SPI al promedio de un conjunto de
#     puntos, c/u de los cuales tiene asociada una
#     serie de tiempo (mensual) de precipitaciones
################################################

###################################################
# Standardized Precipitation Index
# Joseph Wheatley Biospherica March 2010
################################################

library(plotrix)

source("spi_functions.R")
#debugSource("spi_functions.R") # Si se quiere correr esta parte con debbuger

# Leemos el archivo que contiene los datos:
fn <- mustGet("Archivo de datos (csv)->") # Por ejmplo ConchosPRE_mm.csv
prefix <- strsplit(fn, ".csv", fixed = T)[[1]]


k <- as.numeric(mustGet("Período SPI (meses):"))
# La tabla de datos:
dd <- read.csv(fn, row.names = 1)


# Para partir la tabla en años exactos

nm <- nrow(dd) - 2 # Número total de meses
na <- floor(nm/12) # Número de años completos
# Tenemos que asegurarnos que na sea par:
na <- if (na%%2) na-1 else na
# El número de meses efectivos que se tratarán:
ne <- na*12

ini <- nm - ne + 1 # Inicio real de la serie

# Exclusivamente las series de datos, ajustando
# también al número de meses efectivos
# sdd <- dd[3:(ne+2),] # Se eliminan coordenadas y meses extra (<<YA NO)
sdd <- dd[-(1:2),] # Se eliminan coordenadas


# ne es par

n2 <- ne/2

# Hagamos la serie resumen de los datos:

ss <- apply(sdd,1,mean)

# Se divide la serie a la mitad y se calculan los dos SPIs
# (uno para cada sub-serie), y creamos la salida 

spiX <- getSPIfor_k(ss,k,ini)
names(spiX) <- tail(names(ss),ne)
# Particionamos en dos conjuntos:
spi0 <- spiX[1:n2]
spi1 <- spiX[(n2+1):ne]

Mbrk <- c(-3,-2,-1.5,-1,1,1.5,2,3)
hh0 <- hist(spi0, breaks=Mbrk, plot=F)
hh1 <- hist(spi1, breaks=Mbrk, plot=F)

# Saquemos la información de los histogramas:
h0 <- hh0$density
h1 <- hh1$density

# Nombremos los intervalos de h0 y h1 
rnames <- paste("(",lag(Mbrk)[-1],",",Mbrk[-1],"]", sep="")
names(h0) <- names(h1) <- rnames

# Hagamos sendas estructuras que contengan toda la información generada

# Cálculo de los índices de cambio:

indC <- (h1-h0)/(h1+h0)

mm <- max(c(h0,h1))

rg <- pretty(c(0,mm),2)


# períodos inicial y final

getYear <- function(i) strsplit(names(ss)[i],"-")[[1]][1]

n <- nm - n2 # punto medio

yr0 <- getYear(ini) %+% "-" %+% getYear(n)
yr1 <- getYear(n+1) %+% "-" %+% getYear(nm)

Mcols <- colorRampPalette(c("darkred","red","sandybrown","cornsilk2","lightblue","royalblue3","darkblue"),space="rgb")
Scols <- Mcols(length(Mbrk))

labs <- names(h0) %+% ", r= " %+% signif(indC,3)

fnam <- prefix %+% "_Pyramid.png"
if (file.exists(fnam)) file.remove(fnam)

png(filename = fnam, height = 480, width = 550)

pyramid.plot(
    h0, h1, 
    top.labels = c(yr0, " ", yr1), 
    lxcol = Scols, 
    rxcol = Scols,
    laxlab = rg, 
    raxlab = rg, 
    labels=labs, 
    space = .2, 
    unit = "", 
    show.values = T, ndig = 3,
    gap=0.35
)

dev.off()

