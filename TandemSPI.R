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
# debugSource("spi_functions.R") # Si se quiere correr esta parte con debbuger

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

# Se divide cada serie a la mitad se calcula para cada punto dos SPIs
# (uno para cada sub-serie), y creamos la salida con las coordenadas 
# y cada esos dos valores

spiX <- as.data.frame(sapply(sdd, function(ss) getSPIfor_k(ss,k,ini)))
rownames(spiX) <- tail(rownames(sdd),ne)
# Particionamos en dos conjuntos:
spi0 <- spiX[1:n2,]
spi1 <- spiX[(n2+1):ne,]

Mbrk <- c(-3,-2,-1.5,-1,1,1.5,2,3)
hh0 <- sapply(spi0, hist, breaks=Mbrk, plot=F)
hh1 <- sapply(spi1, hist, breaks=Mbrk, plot=F)

# Saquemos la información de los histogramas:
h0 <- (do.call(cbind,hh0["density",]))
h1 <- (do.call(cbind,hh1["density",]))

# h0 y h1 son sendas matrices cuyas columnas son los puntos,
# y cuyos renglones son los intervalos de los histogramas
# definidos por la partición Mbrk. Así, hagamos los nombres
# de los renglones:
rnames <- paste("(",lag(Mbrk)[-1],",",Mbrk[-1],"]", sep="")
rownames(h0) <- rownames(h1) <- rnames

# Hagamos sendas estructuras que contengan toda la información generada

E_spi0 <- rbind(
    # Las coordenadas:
    dd[1:2,],
    # Los SPI p/cada fecha
    spi0,
    # La información de los histogramas
    h0
)

E_spi1 <- rbind(
    # Las coordenadas:
    dd[1:2,],
    # Los SPI p/cada fecha
    spi1,
    # La información de los histogramas
    h1
)

# Finalmente guardemos esas estructuras en sendos archivos utilizables
# posteriormente

write.csv(E_spi0, file = paste0(prefix, "_", k, "_SPI0.csv"), row.names = T)
write.csv(E_spi1, file = paste0(prefix, "_", k, "_SPI1.csv"), row.names = T)



