#====================================
# ResuClimaAnual.R
#
#   Calcula resumen y climatología anuales
#====================================
source("../RR/MiBiblioteca.R", chdir = T)
# library(dplyr)
# Directorio base
bdir <- "CUENCAS"

cuencas <- list.files(bdir)

# Variables a examinar:
vars <- c("PRE", "TMAX", "TMIN")
# operaciones de resumen p/c variable
ops <- list(sum, mean, mean)

for (cc in cuencas) {
    # Directorio c/cuenca:
    cdir <- paste0(bdir, "/", cc)
    
    for (i in 1:length(vars)) {
        vv <- vars[i]
        op <- ops[[i]] # operación correspondiente a vv
        # El nombre del archivo sin el apéndice "...csv"
        bare <- cdir %,% "/" %,% cc %,% vv
        # Nombre del archivo con la información
        ff <- bare %,% ".csv"
        # Se lee el archivo
        t0 <- read.csv(ff, row.names = 1)
        # quitamos coordenadas
        tt <- t0[-(1:2),]
        # La operación de resumen, agrupada por (mes)
        f0 <- as.integer(sapply(strsplit(rownames(tt),"-", fixed = T), '[', 1)) # años de la serie
        rr <- as.matrix(aggregate(tt, list(anio=f0), op))
        rResu <- rr[,-1] # Sin col del año
        # Nombres de renglones: los años
        rownames(rResu) <- rr[,1]
        
        # Ahora el clima 
        rClima <- rbind(apply(rResu, 2, mean), apply(rResu, 2, sd))
        rownames(rClima) <- c("mean", "sd")
        
        # De una vez se harán las anomalías anuales
        rAnomAnual <- NULL
        for (i in 1:nrow(rResu)) {
            rAnomAnual <- rbind(rAnomAnual, 10*(rResu[i,]-rClima["mean",])/rClima["sd",])
        }
        rownames(rAnomAnual) <- rownames(rResu)
        rAnomAnual <- as.data.frame(rAnomAnual)
        
        # El resumen estadístico:
        rSummary <- apply(rResu, 2, summary)
        # Nuevo nombre del archivo:
        postfijo <- "_Resu_Clima_Anom_Anual.RData" # contiene rResu rClima y rAnom
        newname <- bare %,% postfijo

        save(rResu, rClima, rSummary, rAnomAnual, file=newname) # Se guardan los cuatro objetos
    }
}
