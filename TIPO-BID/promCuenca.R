# ===========================
# JSS:
#    promCuenca.R
# DESCRIPCIÓN:
#    Funcioncita para hacer los promedios ponderados 
#    en una cuenca
# ===========================
source("PonderaVoronoi.R")
source("PromediaPond.R")

promCuenca <- function(cc, ee, vals, dirCC="CUENCAS/") {
    # cc: cuenca
    # ee: estaciones asociadas a los valores
    # directorio específico de la cuenca:
    dCC <- paste0(dirCC, cc[1])
    if (length(vals)==10) { # caso trivial 10 estaciones
        # archivo de pesos:
        fname <- paste0(dCC, "/PesoEstaciones.txt")
        vpesos <<- leePesos(fname)
        return (PromediaPond(vals))      
    }
    # Cuáles son las estaciones faltantes?
    # Primero, abrimos el archivo que contiene la información de estaciones
    fname <- paste0(dCC, "/Estaciones.txt")
    # tabla de estaciones:
    tte <- read.table(fname, header=T)
    # la ordenamos:
    ii <- order(tte$id)
    tte <- tte[ii, ]
    # String identificador de faltantes, será del tipo "1110110111", donde
    # los ceros indican estaciones faltantes,en orden y de izq. a der.:
    # -- primero una máscara:
    mask <- tte$id %in% ee
    sff <- paste0(as.integer(mask), collapse="")
    # El nombre del posible archivo que contendría los pesos de las estaciones
    # en cuestión:
    fname <- paste0(dCC, "/Peso-", sff, ".txt")
    # Averiguamos si existe el archivo:
    if (file.exists(fname)) {
        vpesos <<- leePesos(fname)
        # DBG>
        # DBG> l1 <- length(vals); l2 <- length(vpesos)
        # DBG> if (l1 != l2) {
        # DBG>     print(paste0("2:", fname, ":a)", l1, " b)", l2))
        # DBG> }
        return (PromediaPond(vals))
    }
    fgg <- paste0(dCC, "/Geometria.txt")
    gcc <- read.table(fgg)
    vpesos <<- PonderaVoronoi(tte[mask,2:3],gcc) # Note que tte ya está ordenado
    # Guardamos nuestro cálculo para posibles posteriores usos:
    write.table(vpesos, fname, row.names = F)
    # DBG>
    l1 <- length(vals); l2 <- length(vpesos)
    if (l1 != l2) {
        print(paste0("1:", fname, ":a)", l1, " b)", l2))
        print(ee)
        print(vals)
    }
    return (PromediaPond(vals))
} 
