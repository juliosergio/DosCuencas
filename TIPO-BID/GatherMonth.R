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

Tfinal <- NULL

for (cc in cuencas) {
    # Directorio c/cuenca:
    cdir <- paste0(bdir, "/", cc)
    
    for (i in 1:length(vars)) {
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
        n <- ncol(tta)
        xx <- vvPr %,% vvA
        tta <- tta %>% gather(est, xx, 2:n) # Tidyr no interpreta bien xx
        names(tta)[3] <- xx
        
        xx <- "sd" %,% vvA
        ttb <- ttb %>% gather(est, xx, 2:n)
        names(ttb)[3] <- xx
        
        tt <- bind_cols(tta, ttb[,3])
        tt$cuenca <- cc
    }
    
    Tfinal <- bind_rows(Tfinal, tt)
}
