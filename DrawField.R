###################################################
# DrawField.R
#     Funciones para el dibujado de un campo
################################################

if (!exists("LEIDO.MiBiblioteca")) source("RR/MiBiblioteca.R", chdir = T)

ArreglaMtx <- function(x, y, datos) {
    ## Tres vectores paralelos x, y, y los datos (campo) a graficar
    ## el propósito es arreglarlos como una matriz
    ## NOTA: x, y, constituyen una malla regular
    
    # Averigüemos los incrementos y los mínimos en x y y
    xx <- unique(sort(as.numeric(x)))
    yy <- unique(sort(as.numeric(y)))
    
    dx <- get.dif(xx) # La moda de las diferencias de un vector
    dy <- get.dif(yy)
    
    rx <- range(xx)
    ry <- range(yy)
    
    # datos <- Field[i,] # Sigue con la estructura de data.frame
    
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
    Mii <- 1+(rbind(x,y)-c(rx[1],ry[1]))/c(dx,dy)
    
    # Apareamiento
    dd[as.integer(Mii[1,]+(Mii[2,]-1)*m)] <- as.numeric(datos)
    
    # se convierte a matriz y se regresan la matriz y dos vectores lasx, lasy
    return(list(lasx=lasx, lasy=lasy, mm=matrix(dd, nrow = m)))
}
    

DrawContColors <- function(what, Mbreaks, rampaCol, pp, xlab="Lon", ylab="Lat", tit="") {
    ## Dibuja a colores, de acuerdo a la rampa de colores, rampaCol, los datos
    ## contenidos en what (lista con lasx, lasy, y matriz de valoresm,mm)
    ## y el contorno dado en pp
    ## Mbreaks: Donde se rompen los valores para graficar
    
    # rangos del contorno
    pp_rx <- range(pp[[1]])
    pp_ry <- range(pp[[2]])
    
    filled.contour(what$lasx,what$lasy,what$mm,col=rampaCol(length(Mbreaks)),
                   xlab=xlab, ylab=ylab,
                   xlim = pp_rx, ylim = pp_ry,
                   # cex.lab=1.7,font.axis=2,font.lab=2,
                   levels=Mbreaks,key.title="m", 
                   main = tit,
                   plot.axes = {axis(1); axis(2); grid();polygon(pp[[1]], pp[[2]])})
}

DrawContCurvs <- function(what, Mbreaks, pp, xlab="Lon", ylab="Lat", tit="") {
    ## Dibuja con curvas, los datos
    ## contenidos en what (lista con lasx, lasy, y matriz de valoresm,mm)
    ## y el contorno dado en pp
    ## Mbreaks: Donde se rompen los valores para graficar
    
    # rangos del contorno
    pp_rx <- range(pp[[1]])
    pp_ry <- range(pp[[2]])

    plot(x = 0, y = 0, type = "n", xlim = pp_rx, ylim = pp_ry,
         xlab = "Lon", ylab = "Lat")
    u <- par("usr")
    rect(u[1], u[3], u[2], u[4], border = "black")
    contour(what$lasx, what$lasy, what$mm, lty = "solid", add = TRUE, levels=Mbreaks,
            vfont = c("sans serif", "plain"))
    grid()
    polygon(pp[[1]], pp[[2]])
    title(tit, font = 4)
}
