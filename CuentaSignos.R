#################################
# JSS: CuentaSignos.R
# PROPOSITO: Contar los grupos de elementos
#            Con el mismo signo en un vector
########################################

# xx[,b:=shift(a)]
# xx[1,b:=(a+2)%%3-1]
# a <- xx$a
library(data.table)

creaID <- function(a) {
    # Dada un vector, identifica grupos con el mismo signo en la 
    # secuencia.
    tst <- (sign(a[1])+2)%%3-1 # Hace un signo diferente del primero
    n <- length(a)
    nn <- 0 # Para identificadores
    cc <- NULL
    for (i in 1:n) {
        if (sign(a[i]) != tst) {
            nn <- nn+1
            tst <- sign(a[i])
        }
        cc <- c(cc, nn)
    }
    cc
}

CuentaSignos <- function(xx, columna) {
    xx[, mi.ID:=creaID(xx[[columna]])]
    list(xx, xx[, .N, by=mi.ID])
}

# Ejemplo
test <- function () {
    xx <- data.table(a=c(rep(-5,3),0,0,rep(8,3),rep(-2,2),0,rep(-3,2),9,rep(-3,2),rep(1,2)))
    # xx[,id:=creaID(a)]
    # xx[,.N,by=id] # Cuántos elementos tiene cada grupo
    CuentaSignos(xx, "a")
}