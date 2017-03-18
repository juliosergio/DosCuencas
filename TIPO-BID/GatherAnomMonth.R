#====================================
# GatherAnomMonth.R
#
#   Arregla anomalías mensuale en un arhcivo
#====================================
source("../RR/MiBiblioteca.R", chdir = T)
# library(dplyr)
# Directorio base
bdir <- "CUENCAS"

cuencas <- list.files(bdir)

# Variables a examinar:
vars <- c("PRE", "TMAX", "TMIN")
varA <- c("ppAcc", "mTmax", "mTmin")
# varPr <- c("m", "mm", "mm")

tarea1 <- function(tte, xx, yy="est") {
    # Convierte todas las columnas, salvo la primera,
    # en valores, identificados por la columna con
    # nombre 'xx'
    # tte: tabla de entrada
    # xx: Nombre de la columna 
    # -----------------------
    # xx <- vvPr %,% vvA
    n <- ncol(tte)
    tte <- tte %>% gather(est, xx, 3:n) # Tidyr no interpreta bien xx
    names(tte)[3:4] <- c(yy,xx)
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
        # vvPr <- varPr[i]
        
        # El nombre del archivo sin el apéndice 
        bare <- cdir %,% "/" %,% cc %,% vv
        # Nombre del archivo con la información

        postfijo1 <- "_AnomMens.RData"
        newname <- bare %,% postfijo1
        # Se guarda el arreglo, en el mismo directorio, con el nuevo nombre:
        load(newname) # Contiene rAnomMens, 
        mm <- do.call(rbind, lapply(strsplit(rownames(rAnomMens), "-"), as.integer))
        colnames(mm) <- c("anio", "mes", "dia")
        tta <- tbl_df(cbind(mm[,1:2],rAnomMens))
        
        tta <- tarea1(tta, vvA)

        if (i==1) tt <- tta[,1:3] %>% mutate(cuenca=cc)
        tt <- bind_cols(tt, tta[,4])
    }
    
    MegaT <- bind_rows(MegaT, tt)
}

MegaT$cuenca <- as.factor(MegaT$cuenca)

# Se salva la MegaT en el sitio que le corresponde:
save(MegaT, file="GLOBAL/AltMegaTabla.RData")
