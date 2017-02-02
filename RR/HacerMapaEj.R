# HacerMapaEj.R
source("MiBiblioteca.R")
data("volcano")
rx <- range(x <- 10*1:nrow(volcano))
ry <- range(y <- 10*1:ncol(volcano))
ry <- ry + c(-1,1) * (diff(rx) - diff(ry))/2
resp <- mustGet("Elija: 1) Contornos, 2) Colores =>","1", c("1", "2"))
if (resp == "1") 
{
    opar <- par(pty = "s")
    plot(x = 0, y = 0,type = "n", xlim = rx, ylim = ry,
         xlab = "", ylab = "")
    u <- par("usr")
    rect(u[1], u[3], u[2], u[4], border = "black")
    contour(x, y, volcano,lty = "solid", add = TRUE,
            vfont = c("sans serif", "plain"))
    title("Mapa topográfico de Maunga Whau", font = 4)
    abline(h = 200*0:4, v = 200*0:4,lty = 2, lwd = 0.1)
    par(opar)
} else {
    dev.off()
    Mbreaks <- seq(100, 200, by=10) # Son 10 intervalos
    Mcols <- colorRampPalette(c("darkred","red","yellow","white","green","blue","darkblue"),space="rgb")
    filled.contour(x,y,volcano,col=Mcols(10),cex.lab=1.7,font.axis=2,font.lab=2,levels=Mbreaks,key.title="m")
    title(main="Mapa topográfico de Maunga Whau",cex.main=2)
}