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
    nn <- 0 # Para identificadores de grupos
    cc <- NULL
    for (i in 1:length(a)) {
        if (sign(a[i]) != tst) {
            nn <- nn+1
            tst <- sign(a[i])
        }
        cc <- c(cc, nn)
    }
    cc
}

CuentaSignos <- function(xx, columna) {
    xx[, c("sgn", "ID.sgn") := .(sign(xx[[columna]]), creaID(xx[[columna]]))]
    list(xx, xx[, .(sgn=sgn[1], .N), by=ID.sgn])
}

CuentaSignosV <- function(v) {
    # Igual que la anterior, pero aquí la entrada es un vector
    xx <- data.table(v=v)
    CuentaSignos(xx, "v")
}

# Supone la salida de CuentaSignos no de CuentaSignosV
AmarraFechas <- 

# Ejemplo
test <- function () {
    xx <- data.table(a=c(rep(-5,3),0,0,rep(8,3),rep(-2,2),0,rep(-3,2),9,rep(-3,2),rep(1,2)))
    CuentaSignos(xx, "a")
    print ("===========")
    print(w <- CuentaSignosV(xx$a))
    print ("===========")
    w[[2]][, .(sum=sum(N)), by=sgn]
    load("/media/checo/7B2C787106E895ED/PROY.CONACYT/ConchosPRE_mm_12Struct.RData")
    View(mdd)
    n2 <- nrow(mdd)/2
    rr0 <- CuentaSignosV(head(mdd$spi,n2))
    rr1 <- CuentaSignosV(tail(mdd$spi,n2))
    # rr0[[2]][,mean(N)]
    # rr1[[2]][,mean(N)]
    # hist(rr0[[2]]$N)
    # hist(rr1[[2]]$N)
    boxplot(list(rr0[[2]]$N, rr1[[2]]$N), names=c("1er Subperíodo", "2o Subperíodo"))
    rr0 <- CuentaSignos(setDT(head(mdd,n2)),"spi")
    rr1 <- CuentaSignos(setDT(tail(mdd,n2)),"spi")
    # Uniformizar a una escala de meses
    rr0[[2]][,c("meses", "sN") := .(cumsum(N), sgn*N)]
    rr1[[2]][,c("meses", "sN") := .(cumsum(N), sgn*N)]
    plot(rr1[[2]][,c("meses","sN")], type="l", col="red", ylab="ancho y signo", xlim=c(0,n2), ylim=range(rr1[[2]]$sN)+c(-10,0))
    lines(rr0[[2]][,c("meses","sN")], lty=2)
    abline(h=0)
    grid()
    legend("bottomright", c("Subperíodo 1", "Subperíodo 2"), col=c("black", "red"), lty=c(2,1))
    
    boxplot(list(rr0[[2]]$N, rr1[[2]]$N), names=c("Subperíodo 1", "Subperíodo 2"), 
            border = c("black", "red"), main="Signo Indistinto")
    
    # Los negativos y los positivos
    N.pos.0 <- rr0[[2]][sgn > 0, N]
    N.pos.1 <- rr1[[2]][sgn > 0, N]
  
    N.neg.0 <- rr0[[2]][sgn < 0, N]
    N.neg.1 <- rr1[[2]][sgn < 0, N]
    
    boxplot(list(N.pos.0, N.pos.1), names=c("Subperíodo 1", "Subperíodo 2"), 
            border = c("black", "red"), main="Signo Positivo")
    
    boxplot(list(N.neg.0, N.neg.1), names=c("Subperíodo 1", "Subperíodo 2"), 
            border = c("black", "red"), main="Signo Negativo")
    
    df.pos <- rbind(data.frame(N=N.pos.0, Subper=1), data.frame(N=N.pos.1, Subper=2))
    df.neg <- rbind(data.frame(N=N.neg.0, Subper=1), data.frame(N=N.neg.1, Subper=2))
    
    ii.pos <- order(df.pos$N, decreasing = T)
    ii.neg <- order(df.neg$N, decreasing = T)
    df.pos.ord <- df.pos[ii.pos,]
    df.neg.ord <- df.neg[ii.neg,]
    df.pos.ord
    df.neg.ord
    write.csv(df.pos.ord, "CuentaSignosPOS.csv")
    write.csv(df.neg.ord, "CuentaSignosNEG.csv")
}