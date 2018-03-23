#====================
# Modificación de StationsView.R
# para leer otra base de datos
# PARA AMIGA DE DANY
#    CASO: Angangueo, Mich.
#=====================
library(sp)
library(ncdf4)
source("RR/MiBiblioteca.R", chdir = T)
# Lectura NetCDF
fn <- mustGet("Nombre del archivo:") # Por ejemplo ../MALLA.NORTEAMERICA/livneh_NAmerExt_15Oct2014.201002.nc
nn <- nc_open(fn)
nv <- nn$var[[1]]$name # La precipitación
# Leemos la variable:
vv <- ncvar_get(nn,nv)
# Hagamos el data.frame de longitudes y latitudes:
Lons <- ncvar_get(nn,"lon")
Lats <- ncvar_get(nn,"lat")
lonlat <- expand.grid(Lon=Lons, Lat=Lats)
# Poligono de recorte:
resp <- readline("Poligono de recorte?->") # p.ej. ../ANGANGUEO/Angangueo.csv
if (resp != "") pp <- read.csv(resp)[,c("Lon","Lat")]
# número de renglones en la tabla de salida:
nr <- nrow(lonlat)
ii <- if (resp=="") rep(T,nr) else as.logical(point.in.polygon(lonlat$Lon, lonlat$Lat, pp$Lon, pp$Lat))
# Convertimos el arreglo vv a matriz, y lo filtramos por ii
zz <- data.frame(lonlat[ii,], matrix(vv, nrow=nr)[ii,]) # Las columnas son fechas (variable "time")
fchIni <- as.Date.character("1900-01-01")
names(zz) <- c(names(lonlat), as.character(fchIni+ncvar_get(nn, "time")))
nc_close(nn)
rm(vv,lonlat)
fn <- mustGet("Nombre de archivo para guardar:")
zz <- t(zz) # trasponemos
colnames(zz) <- "P" %,% 1:ncol(zz)
write.csv(zz, file=fn)


