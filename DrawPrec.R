###################################################
# DrawPrec.R
#     Dibuja la precipitación media anual
##################################################

source("DrawField.R")
# debugSource("DrawField.R")

# Leemos el archivo que contiene los datos:
fnam <- mustGet("Archivo de datos (csv)->") # Por ejemplo ConchosPRE_mmAcc.csv

# El nombre de la cuenca
cuenca <- strsplit(fnam, "PRE_mm")[[1]][1]

cuencaF <- cuenca %,% "_puntos.csv"

# La tabla de datos:
precT <- read.csv(fnam, row.names = 1)

# Número de datos (descontando los dos de las coordenadas)
nn <- nrow(precT) - 2

# Se hace el promedio por año de la tabla de datos (que empiezan a partir del renglón 3)
panual <- apply(precT, 2, group.mean, ini=3)

aa <- ArreglaMtx(precT[1,], precT[2,], panual)

rr <- range(panual)

pp <- read.csv(cuencaF) # frontera de la cuenca

Mbreaks <- pretty(range(panual),10)

# Rampa de colores en caso de usar esquema de colores
Mcols <- colorRampPalette(c("cornsilk2","lightblue","royalblue3","darkblue"),space="rgb")

resp <- mustGet("Elija tipo gráfico: 1) Contornos, 2) Colores =>","1", c("1", "2"))

t0 <- "Precip. anuales medias cuenca: " %,% cuenca

fnam <- cuenca %,% "_PreAnual.png"

if (file.exists(fnam)) file.remove(fnam)

png(filename = fnam)

p0 <- pp[,c("Lon","Lat")]
# col <- 1
i <- 1
otra <- " ALGUNA"
repeat
{
    if (mustGet("Incluir" %,% otra %,% " Subcuenca? [S/N]:>", "N", c("s","S","n","N")) %in% c("s","S")) {
        i <- i+1
        fn <- mustGet("Archivo de SubCuenca (csv)->")
        px <- read.csv(fn) # frontera de la cuenca
        p0 <- rbind(p0[,c("Lon","Lat")], c(NA,NA), px[,c("Lon","Lat")])
        otra <- " OTRA"
    } else 
        break
}
col <- 1:i

if (resp == "1") {
    DrawContCurvs(aa, Mbreaks, p0, xlab = "Lon", ylab = "Lat", col=col) # tit=t0)
} else {
    DrawContColors(aa, Mbreaks, Mcols, p0, xlab = "Lon", ylab = "Lat", col=col) #tit = t0)
}
dev.off()

