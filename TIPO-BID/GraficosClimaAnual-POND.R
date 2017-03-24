#====================================
# GraficosClimaAnual-POND.R
#
#   Hace el concentrado de gráficos
#   de clima (medias ponderadas)
#====================================
# library(dplyr)
source("../RR/MiBiblioteca.R", chdir = T)

# Directorio de información global
glob <- "GLOBAL" # En este se guardará la MegaTabla
fname <- paste0(glob, "/MegaTClimaAnual.RData")
dirGraf <- paste0(glob, "/GRAFICOS/ClimaAnual/") # Directorio de gráficos

# Directorio de cuencas:
dirCC <- "CUENCAS/"
#  source("promCuenca.R") # <- PARA ESTE CASO NO, se usa la func. de abajo

# YANO> promCuenca <- function(cc, vals) {
# YANO>     # archivo de pesos:
# YANO>     fname <- paste0(dirCC, cc[1], "/PesoEstaciones.txt")
# YANO>     vpesos <<- leePesos(fname)
# YANO>     PromediaPond(vals)
# YANO> } 


# La gran tabla que incluye "todo":
load(fname) # Contiene MegaT generada con   HacerMegaTClima.R
# Averiguamos las cuencas
cuencas <- levels(MegaT$cuenca)
nc <- length(cuencas)

# =================================================
# MÉDULA DE LOS CÁLCULOS
# Agrupemos por (cuenca) y hagamos el 
# resumen de los datos
MegaT <- MegaT %>% 
    group_by( cuenca ) %>% 
    summarise(aApp = mean(mApp), aTmax = mean(mmTmax), aTmin =mean(mmTmin))
    # YANO> summarise(
    # YANO>     aApp = promCuenca(cuenca, mApp), 
    # YANO>     aTmax = promCuenca(cuenca, mmTmax), 
    # YANO>     aTmin = promCuenca(cuenca, mmTmin)
    # YANO> )


titles <- c(
    "", 
    "Maximum Temperature Climatology",
    "Minimum Temperature Climatology",
    ""
)

# Unidades de la escala:
usc <- list("mm" , "°C", "°C", "°C")

# Se inicializan los plots 
graphics.off()
# Se abrirán tres dispositivos gráficos (archivos pdf), cuyos
# nombres estarán compuestos por c/variable
gnamePP <- paste0(dirGraf, "ClimaAnual_PP-POND.pdf") 
gnameTmax <- paste0(dirGraf, "ClimaAnual_Tmax-POND.pdf") 
gnameTmin <- paste0(dirGraf, "ClimaAnual_Tmin-POND.pdf")
# Alternativamente: -- cllima anual combinado --
gnameTemp <- paste0(dirGraf, "AltClimaAnual_Tmp-POND.pdf")
# Los abriré en tal orden que quede el que me interesa como activo:
pdf(gnamePP) 
pdf(gnameTmax)
pdf(gnameTmin) # Este es el que quda activo al principio del ciclo (ultimo)
pdf(gnameTemp)


# Antes de cerrar los dispositivos gráficos se añaden las 
# leyendas
for (jj in 1:3) { # Un archivo gráfico por variable
    dev.set(dev.next()) # Un dispositivo 
    barplot(MegaT[[1+jj]], main=titles[jj] , 
            names=cuencas, xlab="Watersheds", ylab=usc[jj])
    
}
# Hacemos el gráfico alterno de temperaturas
dev.set(dev.next()) # Último dispositivo 
barplot(t(as.matrix(select(MegaT, aTmin:aTmax) %>% mutate(aTmax=aTmax-aTmin))), 
        beside=F,
        main=titles[4], 
        names=cuencas, xlab="Watersheds", ylab=usc[4])

# se cierran todos los dispositivos gráficos:
graphics.off()
