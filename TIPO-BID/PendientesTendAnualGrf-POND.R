#====================================
# PendientesTendAnualGrf-POND.R
#
#   Calcula las tendencias
#   en las anomalias anuales, y
#   grafica por variable (pendiente) para cada
#   una de las cuencas en cuestion
#====================================
# library(dplyr)
source("../RR/MiBiblioteca.R", chdir = T)

# Directorio de información global
glob <- "GLOBAL" # En este se guardará la MegaTabla
fname <- paste0(glob, "/MegaTAnual.RData")
dirGraf <- paste0(glob, "/GRAFICOS/PendientesAnual/") # Directorio de gráficos

# Directorio de cuencas:
dirCC <- "CUENCAS/"
# source("promCuenca.R")


# +--- ***FUNCIÓN DE MODELO*** ---+
# |        usando dplyr           |
fff <- function (x, y) {
    # función sólo en términos 
    # de las columnas para usar
    # con "dplyr"
    mm <- lm(y ~ x)
    coef(mm)[2]
}
# |                               |
# +-FIN ***FUNCIÓN DE MODELO***  -+


# La gran tabla que incluye "todo":
#    ya no se leerá de un archivo de texto con read.table
#REMOVED>> MegaT <-  tbl_df(read.table(fname, header=T))
load(fname) # Contiene MegaT generada con   HacerMegaTAnual.R
# Averiguamos las cuencas
cuencas <- levels(MegaT$cuenca)
nc <- length(cuencas)

# =================================================
# MÉDULA DE LOS CÁLCULOS
# Agrupemos por (mes, cuenca, anio) y hagamos el 
# resumen de los datos
MegaT <- MegaT %>% 
    group_by(cuenca, anio) %>% 
    summarise(ppAcc=mean(ppAcc), mTmax=mean(mTmax), mTmin=mean(mTmin))
    # YANO> summarise(
    # YANO>     ppAcc = promCuenca(cuenca, est, ppAcc), 
    # YANO>     mTmax = promCuenca(cuenca, est, mTmax), 
    # YANO>     mTmin = promCuenca(cuenca, est, mTmin)
    # YANO> )

# ============ Calculos de Mann-Kendall ===========

source("AplicarMannKendallAnual.R")




# =================================================
# +----- ***USARE ESTA ALTERNATIVA*** ------+
# |               usando dplyr              |    
# |             aquí usaré MegaT            |
ttrr <- MegaT %>%
    group_by(cuenca) %>%
    summarise(
        aApp=fff(anio, ppAcc),
        aTmax=fff(anio,mTmax),
        aTmin=fff(anio,mTmin))
# |  la tabla resultante solo hay que       |
# |  separarla con filter o split           |
# |  por cuenca                             |
# +-FIN- ****USARE ESTA ALTERNATIVA*** -----+

# +========== CALCULO DE CORRELACIONES ===========+
# |               Tmax vs Precip                  |
# +===============================================+
#YANO>> ttt <- ttrr %>% ungroup %>%
#YANO>>     group_by(cuenca) %>%
#YANO>>     summarise(corr=cor(aTmax, aApp))

# Escribimos a un archivo
#YANO>> write.csv(ttt, file=paste0(dirGraf, "AltCorr_Tmax_vs_Precip.csv"))

titles <- c(
    "", 
    "Maximun Temperature Tendency",
    "Minimum Temperature Tendency",
    ""
)

# Unidades de la escala:
usc <- list( "", "", "", ""
    #expression(paste("% ",  Year^-1)), 
    #expression("°C per decade"),
    #expression("°C per decade"),
    #expression("°C per decade")   
)

# Se inicializan los plots 
graphics.off()

# Se abrirán tres dispositivos gráficos (archivos pdf), cuyos
# nombres estarán compuestos por c/variable
gnamePP <- paste0(dirGraf, "Pend_Tnd_PP-POND.pdf") 
gnameTmax <- paste0(dirGraf, "Pend_Tnd_Tmax-POND.pdf") 
gnameTmin <- paste0(dirGraf, "Pend_Tnd_Tmin-POND.pdf")
# Alternativamente: -- tendencias de temperatura anual combinado --
gnameTemp <- paste0(dirGraf, "AltPend_Tnd_Tmp-POND.pdf")
# Los abriré en tal orden que quede el que me interesa como activo:
pdf(gnamePP) 
pdf(gnameTmax)
pdf(gnameTmin)
pdf(gnameTemp)


# Antes de cerrar los dispositivos gráficos se añaden las 
# leyendas
for (jj in 1:3) { # Un archivo gráfico por variable
    dev.set(dev.next()) # Un dispositivo 

    # Mann-Kendall significance:
    MK <- ll[[jj]] [,"cuenca"]
    # T: significante, F: no significante
    aa <- 1:nc %in% MK # Para c/u de las 10 cuencas
    # Se manifestará mediante ashurado
    ashura <- c(-1,25)[1 + aa]
    barplot(ttrr[[1+jj]], main=titles[jj], density=ashura, 
            # names=letters[1:nc], 
            names=cuencas, # las=2,
            xlab="Watersheds", ylab=usc[jj])
}
# Hacemos el gráfico alterno de temperaturas
dev.set(dev.next()) # Último dispositivo 

# Mann-Kendall significance:
MK <- ll[[2]] [,"cuenca"]
# T: significante, F: no significante
aa <- 1:nc %in% MK # Para c/u de las 10 cuencas
# Se manifestará mediante ashurado
ashura <- c(-1,25)[1 + aa]

barplot(ttrr$aTmax, density = ashura, 
        ylim=range(ttrr$aTmax, ttrr$aTmin),
        main=titles[4], 
        # names=letters[1:nc], 
        names=cuencas, # las=2,
        xlab="Watersheds", ylab=usc[4])

# Mann-Kendall significance:
MK <- ll[[3]] [,"cuenca"]
# T: significante, F: no significante
aa <- 1:nc %in% MK # Para c/u de las 10 cuencas
# Se manifestará mediante ashurado
ashura <- c(-1,25)[1 + aa]

barplot(ttrr$aTmin, col=gray(0.10),  density = ashura, add=T)
# se cierran todos los dispositivos gráficos:
graphics.off()
