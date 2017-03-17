#====================================
# Gather.R
#
#   Arregla climatologías en dos arhcivos,
#   uno mensual y uno anual
#====================================
source("../RR/MiBiblioteca.R", chdir = T)
# library(dplyr)
# Directorio base
bdir <- "CUENCAS"

cuencas <- list.files(bdir)

# Variables a examinar:
vars <- c("PRE", "TMAX", "TMIN")

for (cc in cuencas) {
    # Directorio c/cuenca:
    cdir <- paste0(bdir, "/", cc)
    
    for (vv in vars) {
        # El nombre del archivo sin el apéndice ".._mm.csv"
        bare <- cdir %,% "/" %,% cc %,% vv
        # Nombre del archivo con la información

        postfijo1 <- "_ClimaMens.RData"
        newname <- bare %,% postfijo
        # Se guarda el arreglo, en el mismo directorio, con el nuevo nombre:
        load(newname) # Contiene rClima, 
        
        ff <- bare %,% "_mm.csv"
        # Se lee el archivo
        t0 <- read.csv(ff, row.names = 1)
        # quitamos coordenadas
        tt <- t0[-(1:2),]
        # La operación de resumen, agrupada por (mes)
        f0 <- as.integer(sapply(strsplit(rownames(tt),"-", fixed = T), '[', 2)) # meses de la serie
        rMean <- as.matrix(aggregate(tt, list(mes=f0), mean))[,-1] # Sin col del mes
        rSDev <- as.matrix(aggregate(tt, list(mes=f0), sd))[,-1]
        n <- nrow(rMean); m <- ncol(rMean)
        rClima <- array(c(rMean, rSDev), c(n, m, 2), dimnames = list(1:n, colnames(rMean), c("mean", "sd")))
        # rClima es un arreglo con 3 dim: mes X punto(estación) X {"mean", "sd"}
        # Nuevo nombre del archivo:
    }
}
