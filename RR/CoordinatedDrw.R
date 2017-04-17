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


DrwSeries <- function(mdd, sbst=NULL, sbstLabs=NULL, scales="free_y", xlab="x", ylab="Values") { #, prefix="Arch_Coo") {
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
            facet_grid(variable ~ ., 
                       scales=scales) + 
            xlab(xlab) +
            ylab(ylab) 
    )
    # ggsave(fnam, width = 8.5, height = 2.4*nc)
}

test <- function(fn="UsumacintaPRE_mm.csv", k=12) {
    
    setwd("..")
    
    source("spi_functions.R")
    #debugSource("spi_functions.R") # Si se quiere correr esta parte con debbuger
    
    prefix <- strsplit(fn, "PRE_mm.csv", fixed = T)[[1]]
    dd <- read.csv(fn, row.names = 1)
    
    # Exclusivamente las series de datos, 
    # sdd <- dd[3:(ne+2),] # Se eliminan coordenadas y meses extra (<<YA NO)
    sdd <- dd[-(1:2),] # Se eliminan coordenadas
    
    # Hagamos la serie resumen de los datos:
    
    ss <- apply(sdd,1,mean)
    
    nm <- length(ss) # Número total de meses
    na <- floor(nm/12) # Número de años completos
    # Tenemos que asegurarnos que na sea par:
    na <- if (na%%2) na-1 else na
    # El número de meses efectivos que se tratarán:
    ne <- na*12
    
    ini <- nm - ne + 1 # Inicio real de la serie
    
    ssProm <- getPrecOnTimescale(ss, k, ini)
    ss <- tail(ss,ne)
    dd <- data.frame(Fecha=names(ss),  prec=ss, precProm=ssProm)
    # Salvamos para uso posterior
    save(dd, file=prefix %,% "preAcc.RData")
    
    p <- DrwSeries(dd, sbstLabs = c(NA, "precProm-" %,% k))
    
    fnam <- prefix %,% "PreYPreAcc.png"
    if (file.exists(fnam)) file.remove(fnam)
    
    p
    ggsave(fnam, width = 8.5, height = 4.8)
}
