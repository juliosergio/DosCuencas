#====================================
# AplicarMannKendallAnual.R
#
#   Calcula los índices de Mann-Kendall
#   en las anomalias anuales, 
# Este archivo será "sourced" 
# en PendientesTendAnualGrf-POND.R
#====================================
#REMOVED-BECAUSE-sourced> library(dplyr)
library(Kendall)

# Directorio de información global
#REMOVED-BECAUSE-sourced> glob <- "GLOBAL" # En este se guardará la MegaTabla
#REMOVED-BECAUSE-sourced> fname <- paste0(glob, "/AltMegaTabla.RData")
#REMOVED-BECAUSE-sourced> dirGraf <- paste0(glob, "/GRAFICOS/PendientesMensual/") # Directorio de gráficos

# Directorio de cuencas:
#REMOVED-BECAUSE-sourced> dirCC <- "CUENCAS/"
#REMOVED-BECAUSE-sourced> source("promCuenca.R")


#YANO>> Meses <- c("Ene","Feb","Mar","Abr","May","Jun",
#YANO>>            "Jul","Ago","Sep","Oct","Nov","Dic")
#REMOVED-BECAUSE-sourced> Meses <- month.abb



# La gran tabla que incluye "todo":
#    ya no se leerá de un archivo de texto con read.table
#REMOVED>> MegaT <-  tbl_df(read.table(fname, header=T))
#REMOVED-BECAUSE-sourced> load(fname) # Contiene MegaT generada con   AltHacerMegaTabla.R
# Averiguamos las cuencas
#REMOVED-BECAUSE-sourced> cuencas <- levels(MegaT$cuenca)
#REMOVED-BECAUSE-sourced> nc <- length(cuencas)


# =================================================
# MÉDULA DE LOS CÁLCULOS
# Agrupemos por (mes, cuenca, anio) y hagamos el 
# resumen de los datos
#REMOVED-BECAUSE-sourced> MegaT <- MegaT %>% 
#REMOVED-BECAUSE-sourced>     group_by(mes, cuenca, anio) %>% 
#REMOVED-BECAUSE-sourced>     summarise(
#REMOVED-BECAUSE-sourced>         ppAcc = promCuenca(cuenca, est, ppAcc), 
#REMOVED-BECAUSE-sourced>         mTmax = promCuenca(cuenca, est, mTmax), 
#REMOVED-BECAUSE-sourced>         mTmin = promCuenca(cuenca, est, mTmin)
#REMOVED-BECAUSE-sourced>     )

ff <- function(ss) {
    # Aplica Mann-Kendall a una serie de tiempo y 
    # extrae sus valores
    w <- MannKendall(ss)
    c(S=w$S, tau=w$tau, var=w$varS, pvalue=w$sl)
}

ll <- list(Prec=NULL, Tmax=NULL, Tmin=NULL)
for (ii in 1:nc) {
    cc <- cuencas[ii]
    tt <- MegaT %>% filter(cuenca==cc)
    for (vv in 3:5) { # variables
        mk <- c(cuenca=ii, ff(tt[[vv]]))
        ll[[vv-2]] <- rbind(ll[[vv-2]], mk)
    }     
}

pv <- c(0.3, 0.1, 0.1) # Prec, Tmax, Tmin
names(pv) <- names(ll)

for (nn in names(ll)) {
    rr <- ll[[nn]]
    rownames(rr) <- NULL
    write.table(rr, paste0(glob, "/", "MK-", nn, "-Anual.txt"), row.names=F)
    # Además filtraremos la tabla por pvalue < umbral (pv)
    rr <- rr[rr[,"pvalue"] < pv[nn],,drop=F]
    write.table(rr, paste0(glob, "/", "MK-", nn, "-Anual-FILTERED.txt"), row.names=F)
    # Modificamos la lista de tablas, para poder utilizarla en quien llame
    # a este bloque de código:
    ll[[nn]] <- rr
}




