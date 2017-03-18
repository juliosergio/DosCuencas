#====================================
# GraficosClima-POND.R
#
#   Hace el concentrado de gráficos
#   de clima
#   Usa promedios ponderados por Voronoi
#====================================
library(dplyr)
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

# +-------- Rango de las Y, por variable ----------+
# +--------+----------------+------------------+------------------+
# |        |   precip       |      Tmax        |       Tmin       |
yr <- list(range(MegaT$aApp),range(MegaT$aTmax),range(MegaT$aTmin))
# y para las gráficas combinadas de Tmax y Tmin:
yr[[4]] <- range(yr[2:3])
# rangos exclusivamente para las preccipitaciones:
yppr <- list(c(0,160), c(0,250))

titles <- c(
    "", 
    "Maximun Temperature Climatology",
    "Minimum Temperature Climatology",
    ""
)

# Unidades de la escala:
usc <- list("mm" , "°C", "°C", "°C")

# Se inicializan los plots 
graphics.off()
# Para la creación de un conjunto de ventanas de graficación,
# mediante los parámetros primitivos de graficación par(fig, new)
# con el siguiente arreglo:
#       +-----------------------------------+ (1,1)
#       |                11  h=0.5/11.1     |
#       +--+-----------------------------+--+
#       |  |              1  h=1/11.1    |  |
#       |  +-----------------------------+  |
#       |  |              2  h=1/11.1    |  |
#       |  +-----------------------------+  |
#       |  |              3  h=1/11.1    |  |
#       |  +-----------------------------+  |
#       |12:              :              :13|
#       |  |                             |  |
#       |  +-----------------------------+  |
#       |  |             10  h=1.6/11.1  |  |
#       +--+-----------------------------+--+
#    (0,0)
#
# La ventana 10, correspondiente el primer índice del siguiente vector,
# lleva un espacio extra para etiquetas (su altura es del doble)
# Vector de alturas:
Yinc <- c(1.6, rep(1,9))/11.1 
# Las acumulo
Yinc <- Reduce('+', Yinc, accumulate=T)
# orden inverso
Yinc <- Yinc[10:1] # Ys de las 1as. 10 ventanas
# Matriz de definición (10 ventanas):
Mm <- cbind(left=1/15, right=14/15, bottom=c(Yinc[2:10],0), top=Yinc)
# Ahora agregamos las ventanas, superior y laterales, 11, 12 y 13:
Mm <- rbind(Mm, 
            # +--------+--------+--------+--------+
            # | left   | right  | bottom |   top  |
            # +--------+--------+--------+--------+
            c(   0    ,   1    , Yinc[1],    1   ),
            c(   0    ,  1/15  ,   0    , Yinc[1]),
            c(  14/15 ,   1    ,   0    , Yinc[1])
)
# Mm contiene la definición de las 13 ventanas necesarias. Cada dispositivo
# (archivo) se divide de acuerdo con esa definición.

#>> gpar <- list(mar=c(0.3, 4.1, 0.1, 2.1))
#>> gpar1 <- list(mar=c(3, 4.1, 0.1, 2.1))
gpar <- list(mar=c(0.3, 4.1, 0.17, 4.1))
gpar1 <- list(mar=c(3, 4.1, 0.17, 4.1))

# Se abrirán tres dispositivos gráficos (archivos pdf), cuyos
# nombres estarán compuestos por c/variable
gnamePP <- paste0(dirGraf, "Clima_PP-POND.pdf") 
gnameTmax <- paste0(dirGraf, "Clima_Tmax-POND.pdf") 
gnameTmin <- paste0(dirGraf, "Clima_Tmin-POND.pdf")
gnameTmp <- paste0(dirGraf, "Clima_Tmp-POND.pdf") # Combinado de temperaturas
# Los abriré en tal orden que quede el que me interesa como activo:
pdf(gnamePP, width=7, height=9.11) 
#YANO>> par(gpar) # los parámetros van por dispositivo
pdf(gnameTmax, width=7, height=9.11)
#YANO>> par(gpar)
pdf(gnameTmin, width=7, height=9.11) 
#YANO>> par(gpar)
pdf(gnameTmp, width=7, height=9.11) 

for (jj in 1:3) { # Un archivo gráfico por variable
    dev.set(dev.next()) # Un dispositivo 
    # Ahora creamos las ventanas:
    #YANO>> split.screen(Mm) # Divide todo el espacio del dispositivo
    # En la ventana superior (11) va el título:
    #YANO>> screen(11); 
    # Ventana 11
    par(fig=Mm[11,])
    par(gpar)
    plot(c(0,1), c(0,1), ylab="", axes=F, type="n")
    # points(0.5, 0)
    text(0.5, 0.5, titles[jj], cex=1.5)
    
    # Para cada cuenca:
    for (ii in 1:nc) { # varía sobre 1..número de cuencas 
        # La gráfica se dibuja en la ventana correspondiente a la cuenca
        # que son las numeradas de 1:10 (nc)
        #YANO>> screen(ii); 
        # Ventanas 1 a 10 (ii):
        par(fig=Mm[ii,], new=T)
        par(if (ii==nc) gpar1 else gpar)
        cc <- cuencas[ii]
        
        # Nos interesa la información correspondiente a la cuenca
        tt <- MegaT %>% 
            filter(cuenca==cc) %>%
            ungroup %>% 
            select(mes, 2+jj) # El mes y la variable correspondiente
        
        # rangos de las Ys
        if (jj > 1) { # No es la precipitación
            rr <- yr[[jj]]
        } else { # precipitaciones: rangos por cuencas
            if (ii <= 3) # cuencas de la 1 a la 3
                rr <- yppr[[1]]
            else if (ii <= 9) # cuencas de la 4 a la 9
                rr <- yppr[[2]] 
            else
                rr <- yr[[jj]] # La cuenca 10 se queda como estaba
        }
        
        plot(tt, #>> main=tit,
             ylab=letters[ii], xlab="", type="b", ylim=rr, 
             axes=F, frame=T)
        # abline (h=0, lty="dotdash", lwd=2)
        grid(lwd=1)
        # tics <- c(yr[[jj]][1], 0, yr[[jj]][2])
        axis(4, las=2, cex.axis=0.7) # at=tics, lab=tics, 
        
        if (ii==nc)
            axis(1, at=1:12, lab=Meses, las=2)
    }
    # Ventana lateral izquierda
    #YANO>> screen(12); 
    # Ventana 12
    #(Por Articulo)--YANO>>par(fig=Mm[12,], new=T)
    #(Por Articulo)--YANO>>par(mar=c(0.1,0.1,0.1,0.1))
    #(Por Articulo)--YANO>>plot(c(0,1), c(0,1), axes=F, type="n")
    #(Por Articulo)--YANO>>text(0.5, 0.5, "WATERSHEDS", srt=90, cex=1.2)
    # Ventana lateral derecha
    #YANO>> screen(13); 
    # Ventana 13
    par(fig=Mm[13,], new=T)
    par(mar=c(0.1,0.1,0.1,0.1))
    plot(c(0,1), c(0,1), axes=F, type="n")
    text(0.5, 0.5, usc[[jj]], srt=90, cex=1.2)
    #YANO>> abline(h=0.7)
    #YANO>> legend(-0.05, 0.65, 
    #YANO>>        legend=paste0(letters[1:3], ": ", cuencas[1:3]), bty="n")
    #YANO>> legend(0.2, 0.65,
    #YANO>>        legend=paste0(letters[4:5], ": ", cuencas[4:5]), bty="n")
    #YANO>> legend(0.50, 0.65,
    #YANO>>        legend=paste0(letters[6:7], ": ", cuencas[6:7]), bty="n")
    #YANO>> legend(0.8, 0.65,
    #YANO>>        legend=paste0(letters[8:10], ": ", cuencas[8:10]), bty="n")
    #YANO>> text(0.5, 0.1, "CUENCAS", cex=1.5)  
}

# El gráfico combinado de temperaturas:
dev.set(dev.next()) # El cuarto dispositivo 
# Ventana 11
par(fig=Mm[11,])
par(gpar)
plot(c(0,1), c(0,1), ylab="", axes=F, type="n")
# points(0.5, 0)
text(0.5, 0.5, titles[4], cex=1.5)

# Para cada cuenca:
for (ii in 1:nc) { # varía sobre 1..número de cuencas 
    # La gráfica se dibuja en la ventana correspondiente a la cuenca
    # que son las numeradas de 1:10 (nc)
    # Ventanas 1 a 10 (ii):
    par(fig=Mm[ii,], new=T)
    par(if (ii==nc) gpar1 else gpar)
    cc <- cuencas[ii]
    # Nos interesa la información correspondiente a la cuenca
    tt <- MegaT %>% 
        filter(cuenca==cc) %>%
        ungroup %>% 
        select(mes, aTmax, aTmin) # El mes y las variables de temperatura
    
    plot(tt$aTmax, 
         ylab=letters[ii], xlab="", type="b", ylim=yr[[4]], 
         axes=F, frame=T)
    lines(tt$aTmin, type="b", pch=20)
    # abline (h=0, lty="dotdash", lwd=2)
    grid(lwd=1)
    # tics <- c(yr[[jj]][1], 0, yr[[jj]][2])
    axis(4, las=2, cex.axis=0.7) # at=tics, lab=tics, 
    
    if (ii==nc)
        axis(1, at=1:12, lab=Meses, las=2)
}
# Ventana lateral izquierda
#YANO>> screen(12); 
# Ventana 12
#(Por Articulo)--YANO>>par(fig=Mm[12,], new=T)
#(Por Articulo)--YANO>>par(mar=c(0.1,0.1,0.1,0.1))
#(Por Articulo)--YANO>>plot(c(0,1), c(0,1), axes=F, type="n")
#(Por Articulo)--YANO>>text(0.5, 0.5, "WATERSHEDS", srt=90, cex=1.2)
# Ventana lateral derecha
#YANO>> screen(13); 
# Ventana 13
par(fig=Mm[13,], new=T)
par(mar=c(0.1,0.1,0.1,0.1))
plot(c(0,1), c(0,1), axes=F, type="n")
text(0.5, 0.5, usc[[4]], srt=90, cex=1.2)

# se cierran todos los dispositivos gráficos:
graphics.off()
