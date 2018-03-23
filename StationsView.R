# JSS: StationsView.R
# PROPOSITO: Cambia la vista de Arreglo de alguna variable en formato NetCDF
#            a una vista como listado de estaciones, con la posibilidad de
#            filtrar los resultados al interior de un poligono
# FORMATO DE variable en el NetCDF:
#               var[longitude,latitude,time]
# ===================================================
library(sp)
library(ncdf4)
if (!exists("LEIDO.MiBiblioteca")) source("RR/MiBiblioteca.R", chdir = T)
# source("Fechas.R")

# Lectura NetCDF
fn <- mustGet("Nombre del archivo:") # Por ejemplo ../CLICOMg/CLICOM_pre_1960-2008_mm.nc (mensual)
fchIni <- as.Date.character(mustGet("Fecha inicio [Default:1960-01-01]", "1960-01-01"))
nn <- nc_open(fn)
# El nombre de la variable que nos interesa
nv <- nn$var[[3]]$name
# Leemos la variable:
vv <- ncvar_get(nn,nv)
# Hagamos el data.frame de longitudes y latitudes:
Lons <- ncvar_get(nn,"longitude")
Lats <- ncvar_get(nn,"latitude")
lonlat <- expand.grid(Lon=Lons, Lat=Lats)

# Poligono de recorte:
resp <- readline("Poligono de recorte?->")
 
if (resp != "") pp <- read.csv(resp)[,c("Lon","Lat")]

# número de renglones en la tabla de salida:
nr <- nrow(lonlat)

ii <- if (resp=="") rep(T,nr) else as.logical(point.in.polygon(lonlat$Lon, lonlat$Lat, pp$Lon, pp$Lat))

# ii son índices de los puntos dentro del polígono

# Si todos los ii son falso, quiere decir que posiblemente todo el polígono está
# contenido en una celda de la rejilla; en ese caso se entregarán los 4 vértices de
# la celda
if (all(!ii)) {
    # "centro" poligono:
    xc <- mean(pp$Lon)
    yc <- mean(pp$Lat)
    ic <- sum(Lons < xc) # índice menor de xc en Lons
    jc <- sum(Lats < yc) # índice menor de yc en Lats
    # La dimensión de las Lons
    n <- length(Lons)
    # y los vértices de la celda en cuestión son:
    cel <- expand.grid(i=ic:(ic+1), j=jc:(jc+1))
    cel$n <- n
    cel$base <- 1
    ind <- do.call(indexXG, cel)
    ii[ind] <- T # índices de los 4 vértices en lonlat
}

# Convertimos el arreglo vv a matriz, y lo filtramos por ii
zz <- data.frame(lonlat[ii,], matrix(vv, nrow=nr)[ii,]) # Las columnas son fechas (variable "time")
names(zz) <- c(names(lonlat), as.character(fchIni+ncvar_get(nn, "time")))
nc_close(nn)

rm(vv,lonlat)

fn <- mustGet("Nombre de archivo para guardar:")
zz <- t(zz) # trasponemos
colnames(zz) <- "P" %,% 1:ncol(zz)
write.csv(zz, file=fn)





