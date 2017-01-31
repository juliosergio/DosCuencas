# JSS: PondInfoEsts.R
# PROPÓSITO: Pondera el "porcentaje" de información que contiene un
#            conjunto de estaciones, cuya información está contenida
#            como un cubo de información en el archivo "Cubote.RData",
#            creado con collect.R, y hace un recorte del Cubote a 
#            un cubo recortado con un número de las "mejores" 
#            estaciones.
#==============
source("../RR/CollectFilesF.R", chdir = T)

# Primeramente ubicamos dónde está el archivo en cuestión y lo leemos
cuenca <- mustGet("Nombre cuenca>") # P.ej. Conchos

f0 <- CollectFiles("Cubote\\.RData", cuenca)
# Completamos el directorio de la cuenca
cuenca <- ExtractDir(f0[1], cuenca)

# Ahora lo leemos:
load(f0) # El nombre del objeto es Cubote

# FORMATO Cubote[i,j,k] i=Fecha, j=Estación, k=Variable
# dimensiones:
nn <- dim(Cubote)

# Función que cuenta los que no son NAs
ff <- function(e) {sum(!is.na(e))}

# Función que encuentra los índices del primer y último valor
# NO NA, en un arreglo
RangeNONAs <- function(e) range(which(!is.na(e)))

# Longitud de un rango como el anterior
LRan <- function(x) x[2] - x[1] + 1

LRanNONAs <- LRan %cmp% RangeNONAs

# Mínimos y mínimos con información
MinNONAs <- function(e) min(which(!is.na(e)))
MaxNONAs <- function(e) max(which(!is.na(e)))


# Vamos a desarrollar varias funciones que servirán más adelante
# (1) Contabilidad de los No_NAs (div entre el número de fechas: nn[1])
Nnas <- apply(Cubote, c(2,3), ff)/nn[1] # Nnas[i,j] i=Estacion, j=Var
# El complemento:
Snas <-  1 - Nnas

# (2) Mínimas y máximas fechas con info y Rango
MinF <- apply(Cubote, c(2,3), MinNONAs)
MaxF <- apply(Cubote, c(2,3), MaxNONAs)
RngF <- MaxF - MinF + 1
# Rango normalizado:
RngF <- RngF/nn[1]

# Factor combinado
Cmbf <- Nnas*RngF

ln <- list(c("conDato","sinDato","minFech","maxFech","rngFech","Combf"))
dn <- c(tail(dimnames(Cubote),-1), ln)
# Armamos un arreglo con estas matrices: 
mm <- array(c(Nnas,Snas,MinF,MaxF,RngF,Cmbf), 
            dim = c(nn[-1],length(ln[[1]])), 
            dimnames = dn)
# mm[i,j,k], donde i:Estaciones, j:Vars, k:índice

# Ahora haré índices por estación promediando
#>>  globMedioNNA <- apply(mm[,,"conDato"],1,mean) # 1: las estaciones
#>>  globMedioRgF <- apply(mm[,,"rngFech"],1,mean)

globMedioCmb <- apply(mm[,,"Combf"],1,mean)

# Ordenamos decreciente: Estaciones con más información primero
#>>  iiNNA <- order(globMedioNNA, decreasing = T)
# Orden por rangos de fechas
#>>  iiRgF <- order(globMedioRgF, decreasing = T)

ii <- order(globMedioCmb, decreasing = T)

ns <- mustGet("Indique cuántas mejores estaciones quiere>","",E_excInt)
#>>  jjNNA <- iiNNA[1:as.integer(ns)] # Los ns mejores en ambos criterios
#>>  jjRgF <- iiRgF[1:as.integer(ns)]
jj <- ii[1:as.integer(ns)]


# Los ordenamos para respetar el orden original
jj <- sort(jj)
# Con estos índices, primero recortamos el arreglo con informaciones
# en el índice de Estaciones:
mm <- mm[jj,,]
# Encontremos el rango de fechas en las estaciones seleccionadas
mmin <- min(apply(mm[,,"minFech"], 1, min)) 
mmax <- max(apply(mm[,,"maxFech"], 1, max)) 

# Y recortamos el Cubote en:
#   (1) las fechas: 1a dimensión
#   (2) las estaciones: 2a dimensión
CuboRecortado <- Cubote[mmin:mmax,jj,]

# .. y se guarda el resultado
save(CuboRecortado, file = cuenca %+% "/CuboRecortado.RData")

