#====================================
# AnomaliaMensual.R
#
#   Calcula anomalias mensuales (mes)
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
        
        # Lectura del clima para la variable en cuestión:
        postfijo <- "_ClimaMens.RData"
        newname <- bare %,% postfijo
        # Se guarda el arreglo, en el mismo directorio, con el nuevo nombre:
        load(newname) # trae la tabla rClima
        # y rClima es un arreglo con 3 dim: mes X punto(estación) X {"mean", "sd"}
        
        # Nombre del archivo con la información
        ff <- bare %,% "_mm.csv"
        # Se lee el archivo
        t0 <- read.csv(ff, row.names = 1)
        # quitamos coordenadas
        tt <- t0[-(1:2),]
        
        # Los meses de la serie
        # f0 <- as.integer(sapply(strsplit(rownames(tt),"-", fixed = T), '[', 2)) # meses de la serie
        
        rAnomMens <- NULL
        n <- dim(rClima)[1]
        for (i in 1:(nrow(tt)/n)) {
            j <- n*(i-1)
            rAnomMens <- bind_rows(rAnomMens, 10*(tt[(j+1):(j+n),]-rClima[,,"mean"])/rClima[,,"sd"])
        }
        rownames(rAnomMens) <- rownames(tt)
        
        # Guardado de la información
        postfijo <- "_AnomMens.RData"
        newname <- bare %,% postfijo
        # Se guarda el arreglo, en el mismo directorio, con el nuevo nombre:
        save(rAnomMens, file=newname) # trae la tabla rClima
    }
}
