###################################################
# DrawIndC.R
#     Dibuja el índice calculado en RelateSPIs.R
################################################

if (!exists("LEIDO.MiBiblioteca")) source("RR/MiBiblioteca.R", chdir = T)

# Leemos el archivo que contiene los datos:
prefix <- mustGet("Nombre genérico de los archivos (csv)->") # Por ejemplo ConchosPRE_mm_12
fnam <- paste0(prefix, "_indC.csv")
# El nombre de la cuenca
components <- strsplit(prefix, "PRE_mm")[[1]]
cuenca <- components[1]
cuencaF <- cuenca %+% "_puntos.csv"

# La tabla de datos:
EIndC <- read.csv(fnam, row.names = 1)

# Averigüemos los incrementos y los mínimos en x y y
xx <- unique(sort(as.numeric(EIndC["Lon",])))
yy <- unique(sort(as.numeric(EIndC["Lat",])))

id.mode <- function(tt) as.numeric(names(tt)[which.max(tt)])  # identifica la moda en una tabla de fecuencias
# La moda de una serie de datos es la composición de la función table(), que calcula las frecuencias
# en la serie, con id.mode()
stat.mode <- id.mode %cmp% table # stat.mode(x) donde x es la serie de datos
get.dif <- function(x) stat.mode(x-lag(x))

dx <- get.dif(xx)
dy <- get.dif(yy)

rx <- range(xx)
ry <- range(yy)

    
# Número de intervalos en los histogramas
nn <- nrow(EIndC) - 2

# En la siguiente captura se puede dar un sólo entero, o un conjunto de ellos, p. ej., 1, 1:7, o c(1,3,5)
inds <- evalstr(mustGet("Índices de intervalos a graficar" %+% " [1.." %+% nn %+% "]:>", inclSet = "[[:digit:]]*:?[[:digit:]]+"))+2

pp <- read.csv(cuencaF) # frontera de la cuenca

# rangos de la cuenca
pp_rx <- range(pp$Lon)
pp_ry <- range(pp$Lat)

Mbreaks <- seq(-1, 1, length.out = 11)

resp <- mustGet("Elija tipo gráfico: 1) Contornos, 2) Colores =>","1", c("1", "2"))

for (i in inds) {
    datos <- EIndC[i,] # Sigue con la estructura de data.frame
    
    # Características de la cuadricula:
    lasx <- seq(rx[1],rx[2],by=dx)
    lasy <- seq(ry[1],ry[2],by=dy)
    
    m <- length(lasx)
    n <- length(lasy)
    
    # En la matriz los renglones serán X y las columnas Y
    
    dd <- as.numeric(rep(NA,m*n))
    
    # Ahora llenamos los puntos de la matriz con la información relacionada
    # y contenida en datos
    
    # Matriz de índices
    Mii <- 1+(EIndC[1:2,]-c(rx[1],ry[1]))/c(dx,dy)
    
    # Apareamiento
    dd[as.integer(Mii["Lon",]+(Mii["Lat",]-1)*m)] <- as.numeric(datos)
    
    # se convierte a matriz
    dd <- matrix(dd, nrow = m)
    
    # YA-NO>>> Mbreaks <- pretty(range(datos),10)
    
    t0 <- "Relación cambio SPI para intervalo " %+% rownames(datos)
    # puntos de la cuenca:
    
    fnam <- cuenca %+% components[2] %+% "_REL_" %+% (i-2) %+% ".png"
    
    if (file.exists(fnam)) file.remove(fnam)
    
    png(filename = fnam)
    
    if (resp == "1") 
    {
        # opar <- par(pty = "s")
        plot(x = 0, y = 0,type = "n", xlim = pp_rx, ylim = pp_ry,
             xlab = "Lon", ylab = "Lat")
        u <- par("usr")
        rect(u[1], u[3], u[2], u[4], border = "black")
        contour(lasx, lasy, dd,lty = "solid", add = TRUE,levels=Mbreaks,
                vfont = c("sans serif", "plain"))
        grid()
        polygon(pp$Lon, pp$Lat)
        title(t0, font = 4)
        # abline(h = 200*0:4, v = 200*0:4,lty = 2, lwd = 0.1)
        # par(opar)
    } else {
        # dev.off()
        # Mbreaks <- seq(100, 200, by=10) # Son 10 intervalos
        # Mcols <- colorRampPalette(c("darkred","red","yellow","cornsilk2","green","blue","darkblue"),space="rgb")
        Mcols <- colorRampPalette(c("darkred","red","sandybrown","cornsilk2","lightblue","royalblue3","darkblue"),space="rgb")
        filled.contour(lasx,lasy,dd,col=Mcols(length(Mbreaks)),
                       xlab="Lon", ylab="Lat",
                       xlim = pp_rx, ylim = pp_ry,
                       # cex.lab=1.7,font.axis=2,font.lab=2,
                       levels=Mbreaks,key.title="m", 
                       main = t0,
                       plot.axes = {axis(1); axis(2); grid();polygon(pp$Lon, pp$Lat)})
        # title(t0,cex.main=2)
    }
    dev.off()
}

