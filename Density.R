# ========================
# JSS: Density.R
#      Funciones para el manejo de densidades estadísticas.
# ========================

if (!exists("LEIDO.MiBiblioteca")) source("RR/MiBiblioteca.R", chdir = T)

# La función de densidad es la composición de dos funciones:

# Creador de funciones de densidad:
dfunCreate <- approxfun %cmp% function(x) density(x, bw = "SJ")
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

pfunCreate <- function(x.data) {
    p <- density(x.data, bw = "SJ")
    d <- mean(p$x-lag(p$x), na.rm = T)
    # p$ny <- cumsum(p$y)*d 
    # Integración trapecial
    p$ny <- cumsum(c(0, ((p$y+lag(p$y))/2)[-1]))*d
    p$ny[p$ny > 1] <- 1 # teoricamente la función no puede ser mayor que 1
    approxfun(p$x, p$ny)
}

test <- function() {
    # Datos
    x <- c(-2.1,-1.3,-.4,1.9,5.1,6.2)
    
    # Densidad
    # Solo para averiguar los límites
    p <- density(x, bw = "SJ")
    ff <- dfunCreate(x)
    hist(x, freq = F, xlim = range(p$x))
    curve(ff, col="red", add = T)
    
    # Distribución
    fff <- pfunCreate(x)
    curve(fff, col="blue", xlim = range(p$x))
}
