#====================================
# GatherMonth.R
#
#   Arregla climatologías en un arhcivo mensual
#====================================
source("../RR/MiBiblioteca.R", chdir = T)
# library(dplyr)
# Directorio base
bdir <- "CUENCAS"

cuencas <- list.files(bdir)

# Variables a examinar:
vars <- c("PRE", "TMAX", "TMIN")
varA <- c("App", "Tmax", "Tmin")
varPr <- c("m", "mm", "mm")

tarea1 <- function(tte, xx, yy="est") {
    # Convierte todas las columnas, salvo la primera,
    # en valores, identificados por la columna con
    # nombre 'xx'
    # tte: tabla de entrada
    # xx: Nombre de la columna 
    # -----------------------
    # xx <- vvPr %,% vvA
    n <- ncol(tte)
    tte <- tte %>% gather(est, xx, 2:n) # Tidyr no interpreta bien xx
    names(tte)[2:3] <- c(yy,xx)
    return(tte)
}

MegaT <- NULL

for (cc in cuencas) {
    # Directorio c/cuenca:
    cdir <- paste0(bdir, "/", cc)
    
    m <- length(vars)
    
    for (i in 1:m) {
        vv <- vars[i]
        vvA <- varA[i]
        vvPr <- varPr[i]
        
        # El nombre del archivo sin el apéndice ".._mm.csv"
        bare <- cdir %,% "/" %,% cc %,% vv
        # Nombre del archivo con la información

        postfijo1 <- "_ClimaMens.RData"
        newname <- bare %,% postfijo1
        # Se guarda el arreglo, en el mismo directorio, con el nuevo nombre:
        load(newname) # Contiene rClima, 
        a <- rClima[,,"mean"]
        b <- rClima[,,"sd"]
        tta <- tbl_df(cbind(mes=as.integer(rownames(a)),a))
        ttb <- tbl_df(cbind(mes=as.integer(rownames(b)),b))
        
        tta <- tarea1(tta, vvPr %,% vvA)
        ttb <- tarea1(ttb, "sd" %,% vvA)

        if (i==1) tt <- tta[,1:2] %>% mutate(cuenca=cc)
        tt <- bind_cols(tt, tta[,3], ttb[,3])
    }
    
    MegaT <- bind_rows(MegaT, tt)
}

MegaT$cuenca <- as.factor(MegaT$cuenca)
# Se salva la MegaT en el sitio que le corresponde:
save(MegaT, file="GLOBAL/MegaTClima.RData")
