# Kernels.R
#    kernel density estimation example
#    Reproduciendo ejemplo de 
#    https://en.wikipedia.org/wiki/Kernel_density_estimation

if (!exists("LEIDO.MiBiblioteca")) source("RR/MiBiblioteca.R", chdir = T)



# Datos
x <- c(-2.1,-1.3,-.4,1.9,5.1,6.2)

# Serán 6 kernels con distribución normal

# producción de las funciones 
kf.producer <- function(x.data, bw) {
    # Dado un conjunto de datos produce las funciones kernel
    # (de distrib. normal), para el ancho de banda dado
    # SALIDA: Una lista de tantas funciones kernel como 
    #         datos se hayan dado. 
    # Las funciones van divididas entre 'n' para poder 
    # sumarlas después y obtener la función kernel total
    n <- length(x.data)
    lapply(x.data, function(elt) function(x) dnorm(x, elt, bw)/(n))
}



# varianza del kernel
vv <- 2.25
# desviación estándar
ss <- sqrt(vv)

ffs <- kf.producer(x, ss)

kf.sum <- function(lasFs) {
    # Dada una lista de funciones produce una función "suma" de las funciones;
    # puesto que el argumento x de la función saliente puede ser un vector, 
    # la función debe aplicarse consecuentemente, esto es, p.ej., si el vector
    # es de 3 elementos la función debe producir a la salida 3 sumas. Por ello
    # se construye como una matriz en la que las columnas corresponden a las
    # funciones en la lista, y los renglones al número de elementos en el 
    # vector 'x', así, el resultado que se quiere serán las sumas por renglones.
    function(x) apply(matrix(sapply(lasFs, function(ff) ff(x)),nrow=length(x)), 1, sum)
}


# La composición de las dos funciones tendrá a la salida una funcion "suma" armada a partir de
# los datos y un ancho de banda (bw)
kf.sum.producer <- kf.sum %cmp% kf.producer  # llamar con ff <- kf.sum.producer (x.datos, bw)

# Plots
hist(x, freq = F, xlim = c(-7,11))
void <- lapply(kf.producer(x,ss), function(ff) curve(ff, add=T))
SumF <- kf.sum.producer(x, ss)
curve(SumF, add = T, col="red")


