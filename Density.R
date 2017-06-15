# ========================
# JSS: Density.R
#      Funciones para el manejo de densidades estadísticas.
# ========================

library(data.table)
if (!exists("LEIDO.MiBiblioteca")) source("RR/MiBiblioteca.R", chdir = T)


# La función de densidad es la composición de dos funciones:

# Función armada con splines, pero adaptada a funciones de densidad y distribución de probabilidades,
# que no pueden tener valores negativos, ni arriba de la unidad
dsplinefun <- function (...) {
    f0 <- splinefun(...) # Se arma una función con splinefun
    return (
        function (x, deriv=0L) {
            dd <- data.table(x=x, y=f0(x, deriv))
            if (deriv==0L) {
                dd[ y < 0 , y := 0][
                    y > 1 , y := 1]
            }
            return (dd[,y]) # función que entrega sólo las Ys
        }
    )
}

# También definimos la función de aproximación lineal con 2 variantes:
dapproxfun <- function (...) approxfun(..., yleft = 0, yright = 0)
papproxfun <- function (...) approxfun(..., yleft = 0, yright = 1)


ffunCreate <- function(x.data, aprxf=dapproxfun, accumulate = FALSE, ...) {
    p <- density(x.data, bw = "SJ", ...)
    p <- data.table(x=p$x, yy=p$y)
    if (accumulate) {
        d <- mean(p$x-data.table::shift(p$x), na.rm = T)
        p$y <- cumsum(c(0, ((p$yy+data.table::shift(p$yy))/2)[-1]))*d
        p[y > 1, y := 1] # teoricamente la función no puede ser mayor que 1
    } else 
        names(p)[2] <- "y"
    
    aprxf(p$x, p$y)
}


# Creador de funciones de densidad:
# Con interp lineal
dfunCreate0 <- (function(...) approxfun(..., yleft = 0, yright = 0)) %cmp% (function(x, ...) density(x, bw = "SJ", ...))
dfunCreate <- function(x.data, ...) ffunCreate(x.data, dapproxfun, ...)
# Con splines
dSfunCreate0 <- dsplinefun %cmp% (function(x, ...) density(x, bw = "SJ", ...))
dSfunCreate <- function(x.data, ...) ffunCreate(x.data, dsplinefun, ...)


# USO: 
#    dfun <- dfunCreate(X) # donde X es el vector de datos
#      y luego se puede usar esta función
#      p.ej.
#
#    X <- log(rgamma(150,5)) # Creación aleatoria de datos
#    dfun <- dfunCreate(X)
#    dfun(c(0.45, 1.84, 2.3))
#      o para graficarla:
#    plot(dfun, xlim=c(-0.1, 3.0))

pfunCreate0 <- function(x.data, ...) {
    p <- density(x.data, bw = "SJ", ...)
    d <- mean(p$x-data.table::shift(p$x), na.rm = T)
    # p$ny <- cumsum(p$y)*d 
    # Integración trapecial
    p$ny <- cumsum(c(0, ((p$y+data.table::shift(p$y))/2)[-1]))*d
    p$ny[p$ny > 1] <- 1 # teoricamente la función no puede ser mayor que 1
    approxfun(p$x, p$ny, yleft = 0, yright = 1)
}
pfunCreate <- function(x.data, ...) ffunCreate(x.data, papproxfun, accumulate = T, ...)


# Lo mismo con Splines
pSfunCreate0 <- function(x.data, ...) {
    p <- density(x.data, bw = "SJ", ...)
    d <- mean(p$x-data.table::shift(p$x), na.rm = T)
    # p$ny <- cumsum(p$y)*d 
    # Integración trapecial
    p$ny <- cumsum(c(0, ((p$y+data.table::shift(p$y))/2)[-1]))*d
    p$ny[p$ny > 1] <- 1 # teoricamente la función no puede ser mayor que 1
    dsplinefun(p$x, p$ny)
}
pSfunCreate <- function(x.data, ...) ffunCreate(x.data, dsplinefun, accumulate = T, ...)


test <- function() {
    # Datos
    x <- c(-2.1,-1.3,-.4,1.9,5.1,6.2)
    
    # Densidad
    # Solo para averiguar los límites
    p <- density(x, bw = "SJ")
    ff <- dfunCreate(x)
    ff0 <- dSfunCreate(x) # Splines

    hist(x, freq = F, xlim = range(p$x))
    curve(ff, col="red", add = T)
    curve(ff0, col="blue", add=T)
    
    # Distribución
    fff <- pfunCreate(x)
    fff0 <- pSfunCreate(x)
    curve(fff, col="blue", xlim = range(p$x))
    curve(fff0, col="red", add = T)
}
