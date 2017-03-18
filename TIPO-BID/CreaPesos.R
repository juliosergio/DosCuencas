#====================================
# CreaPesos.R
#
#   TRIVIAL: Crea pesos de estaciones 
#   (Como es una malla uniforme no se usará Voronoi)
#====================================
source("../RR/MiBiblioteca.R", chdir = T)
# library(dplyr)
# Directorio base
bdir <- "CUENCAS"

cuencas <- list.files(bdir)

for (cc in cuencas) {
    # Directorio c/cuenca:
    cdir <- paste0(bdir, "/", cc)
    
    # El nombre del archivo sin el apéndice ".._mm.csv"
    bare <- cdir %,% "/" %,% cc 
    # Nombre del archivo con la información
    
    postfijo1 <- "PRE.csv"
    newname <- bare %,% postfijo1
    t0 <- read.csv(newname, row.names = 1)
    n <- length(t0) # número de estaciones
    peso <- 1/n
    write.table(data.frame(x=rep(peso,n)),file = cdir %,% "/PesoEstaciones.txt", row.names = F)
}

