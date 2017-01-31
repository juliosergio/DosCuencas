# JSS: Collect.R
#      Colecta los datos, por variable, para un conjunto de 
#      estaciones en un directorio dado.
# =============================================
source("Fechas.R")
# source("../../RR/MiBiblioteca.R")
source("../RR/CollectFilesF.R", chdir = T)


cuenca <- mustGet("Nombre cuenca>") # P.ej. Conchos

pr <- "Tipo arch: A)rcl.txt B)rclindcal.csv C)rclindcalQC.csv :>"

Fpattrn <- switch((resp<-mustGet(pr, "A", LETTERS[1:3])),
                  A="rcl\\.txt$",         # archivos terminados en rcl.txt
                  B="rclindcal\\.csv$",   # archivos terminados en rclindcal.csv
                  C="rclindcalQC\\.csv$"  # archivos terminados en rclindcalQC.csv
)

fls <- CollectFiles(Fpattrn, cuenca) # archivos c/informaci?n

# Completamos el directorio de la cuenca
cuenca <- ExtractDir(fls[1], cuenca)

# Para los nombres de las estaciones:
xx <- strsplit(fls, ".*/|" %+% Fpattrn)

NomEsts <- sapply(xx,tail,1)


if (resp == "A") {
    readTb <- read.table
    hdr <- F
} else {
    readTb <- read.csv
    hdr <- T
}

cols <- c("year", "month", "day", "prcp", "tmax", "tmin")

tablas <- lapply(fls, readTb, header=hdr, col.names=cols)

fechIni <- mustGet("Fecha Inicio [Default:1903-01-01]>", "1903-01-01", E_Fecha)
fechFin <- mustGet("Fecha T?rmino [Default:2014-12-31]>", "2014-12-31", E_Fecha)

fechIni <- as.integer(strsplit(fechIni, "-", fixed = T)[[1]])
fechFin <- as.integer(strsplit(fechFin, "-", fixed = T)[[1]])

# Completar las tablas para tener uniformidad de fechas:
tablas <- lapply(tablas, CompleteTable, fechIni, fechFin, -99.)

# A partir de aqu? har? un arreglo con tres dimensiones (cubo), como sigue:
#   DIMENSI?N 1: Fecha, constru?da a partir de las 3 primeras columnas
#   DIMENSI?N 2: Nombres de las estaciones
#   DIMENSI?N 3: Nombres de las variables
# De modo que: Cubote[i,j,k], donde i,j,k pueden ser enteros o strings
# me regresa el valor de la variable k, de la estaci?n j en la fecha i

# Las fechas son todas uniformes para todas las tablas, construyamos el
# arreglo de fechas a partir de cualquiera de las tablas
Fechas <- do.call(function(...) paste(...,sep="-"), tablas[[1]][,1:3])
Fechas <- as.Date.character(Fechas)
nFechas <- length(Fechas)
nVars <- 3
nEsts <- length(NomEsts)

# Ahora podemos recortar las columnas de fecha de cada una de las tablas
ttablas <- lapply(tablas, function(e) as.matrix(e[,-(1:3)]))
Cubote <- array(do.call(c, ttablas), 
                c(nFechas,nVars,nEsts),
                list(as.character(Fechas), cols[-(1:3)], NomEsts))
                # list(Fechas, cols[-(1:3)], NomEsts))

# Ahora arreglamos las dimensiones (trasponemos ?ndices)
Cubote <- aperm(Cubote, c(1,3,2))

# Salvamos el objeto:
save(Cubote, file = cuenca %+% "/Cubote.RData")





