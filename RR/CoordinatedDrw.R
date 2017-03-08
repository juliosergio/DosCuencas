###################################################
# CoordinatedDrw.R
#     Dibuja varias columnas de una tabla
#     como gráficos coordinados por el eje
#     de las X
################################################

# library(plotrix)
library(ggplot2)

# source("spi_functions.R")
#debugSource("spi_functions.R") # Si se quiere correr esta parte con debbuger

# getYearS <- function(aa) do.call(rbind,strsplit(as.character(aa), "-"))[,1]


DrwSeries <- function(mdd, sbst=NULL, sbstLabs=NULL) { #, prefix="Arch_Coo") {
    # mdd: es un data.frame con la primer columna como fechas y
    #      de nombre Fecha
    # sbst: subconjunto de columnas que se graficarán
    # sbstLabs: Etiquetas, en orden, que se usarán en vez de los nombres
    #           de las columnas a graficar (puede llevar NAs)
    # YANO-> prefix: prefijo que se añade a los archivos generados.
    # ========================================================
    
    # mdd <- tbl_df(data.frame(Fecha=names(spiX), pre=tail(ss,ne), spi=spiX))
    mdd <- tbl_df(mdd) # Se convierte para su manejo
    nc <- ncol(mdd)
    sbst <- if (is.null(sbst)) 2:nc else sbst
    na <- names(mdd)[sbst]
    nc <- length(na) # actualiza nc
    # Transformemmos
    mxx <- mdd %>% gather(variable, value, sbst)
    if (!is.null(sbstLabs)) {
        for (i in 1:nc)
            if (!is.na(sbstLabs[i])) {
                # en los espacios con NA, no se hace substitución
                mxx$variable <- sub(na[i], sbstLabs[i], mxx$variable)
                na[i] <- sbstLabs[i] # para registrar el cambio
            }
    }
    # Para que los datos queden ordenados en el gráfico, de acuerdo a lo 
    # etablecido en na y/o sbstLabs
    mxx$variable <- factor(mxx$variable, levels = na) # factor ordenado de acuerdo a na
    
    # fnam <- prefix %,% ".png"
    # if (file.exists(fnam)) file.remove(fnam)

    p <- ggplot(mxx, aes(x=as.Date.character(Fecha), y=value)) + xlab("Fecha")
    return (
        p + geom_col() + 
        facet_grid(variable ~ ., scale="free_y") + 
        ylab("Valor") 
    )
    # ggsave(fnam, width = 8.5, height = 2.4*nc)
}


