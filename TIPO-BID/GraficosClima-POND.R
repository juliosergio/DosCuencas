#====================================
# GraficosClima-POND.R
#
#   Hace el concentrado de gráficos
#   de clima
#   Usa promedios ponderados por Voronoi (en este caso NO)
#====================================
# library(dplyr)
# library(tidyr)
source("../RR/MiBiblioteca.R", chdir = T)
source("PromediaPond.R")

# Directorio de información global
glob <- "GLOBAL" # En este se guardará la MegaTabla
fname <- paste0(glob, "/MegaTClima.RData")
dirGraf <- paste0(glob, "/GRAFICOS/ClimaMensual/") # Directorio de gráficos
# Directorio de cuencas:
dirCC <- "CUENCAS/"

promCuenca <- function(cc, vals) {
    # archivo de pesos:
    fname <- paste0(dirCC, cc[1], "/PesoEstaciones.txt")
    vpesos <<- leePesos(fname)
    PromediaPond(vals)
} 


#YANO>> Meses <- c("Ene","Feb","Mar","Abr","May","Jun",
#YANO>>            "Jul","Ago","Sep","Oct","Nov","Dic")
Meses <- month.abb

# La gran tabla que incluye "todo":
load(fname) # Contiene MegaT generada con   HacerMegaTClima.R
# Averiguamos las cuencas
cuencas <- levels(MegaT$cuenca)
nc <- length(cuencas)

# =================================================
# MÉDULA DE LOS CÁLCULOS
# Agrupemos por (mes, cuenca, anio) y hagamos el 
# resumen de los datos
MegaT <- MegaT %>% 
    group_by( cuenca, mes) %>% 
    summarise(
        aApp = promCuenca(cuenca, mApp), 
        aTmax = promCuenca(cuenca, mmTmax), 
        aTmin =promCuenca(cuenca, mmTmin)
    )

mmm <- factor(Meses, levels = Meses)

MegaT$mes <- mmm[MegaT$mes]

ext <- ".png" # extensión archivos gráficos

# Precipitaciones:
p <- ggplot(MegaT, aes(x=mes))
p + geom_col(aes(y=aApp)) + facet_grid(cuenca ~ .) + ylab("precipitation (mm)") + xlab("month") + theme_bw()
ggsave(dirGraf %,% "Clima_PP-POND" %,% ext, width = 8.5, height = 4.8)

# Temperatura máxima
p + geom_col(aes(y=aTmax)) + facet_grid(cuenca ~ .) + ylab("Max Temp (°C)") + xlab("month") + theme_bw()
ggsave(dirGraf %,% "Clima_Tmax-POND" %,% ext, width = 8.5, height = 4.8)

# Temperatura mínima
p + geom_col(aes(y=aTmin)) + facet_grid(cuenca ~ .) + ylab("Min Temp (°C)") + xlab("month") + theme_bw()
ggsave(dirGraf %,% "Clima_Tmin-POND" %,% ext, width = 8.5, height = 4.8)

# Temperaturas combinadas
xx <- MegaT %>% gather(Variable, Temp, aTmax:aTmin)
q <- ggplot(xx, aes(x = mes, y = Temp, fill = Variable)) 

q + geom_bar(stat = "identity", position = "identity") + facet_grid(cuenca ~ .) + ylab("Temp (°C)") + xlab("month") + 
    scale_fill_grey(start = 0, end = .9) + theme_bw()

ggsave(dirGraf %,% "Clima_Tmp-POND" %,% ext, width = 8.5, height = 4.8)
