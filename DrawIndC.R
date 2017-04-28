###################################################
# DrawIndC.R
#     Dibuja el índice calculado en RelateSPIs.R
##################################################

source("DrawField.R")
# debugSource("DrawField.R")

# Leemos el archivo que contiene los datos:
prefix <- mustGet("Nombre genérico de los archivos (csv)->") # Por ejemplo ConchosPRE_mm_12
fnam <- paste0(prefix, "_indC.csv")
# El nombre de la cuenca
components <- strsplit(prefix, "PRE_mm")[[1]]
cuenca <- components[1]
cuencaF <- cuenca %,% "_puntos.csv"

# La tabla de datos:
EIndC <- read.csv(fnam, row.names = 1)

# Número de intervalos en los histogramas
nn <- nrow(EIndC) - 2

# En la siguiente captura se puede dar un sólo entero, o un conjunto de ellos, p. ej., 1, 1:7, o c(1,3,5)
inds <- evalstr(mustGet("Índices de intervalos a graficar" %,% " [1.." %,% nn %,% "]:>", inclSet = "[[:digit:]]*:?[[:digit:]]+"))+2

pp <- read.csv(cuencaF) # frontera de la cuenca

Mbreaks <- seq(-100, 100, length.out = 11)

# Rampa de colores en caso de usar esquema de colores
Mcols <- colorRampPalette(c("darkred","red","sandybrown","cornsilk2","lightblue","royalblue3","darkblue"),space="rgb")

resp <- mustGet("Elija tipo gráfico: 1) Contornos, 2) Colores =>","1", c("1", "2"))

rrDatos <- NULL

for (i in inds) {
    tag <- letters[i-2]
    datos <- EIndC[i,]*100 # Sigue con la estructura de data.frame y va multiplicado por 100 (en %)
    
    aa <- ArreglaMtx(EIndC[1,], EIndC[2,], datos)
    
    # YA-NO>>> Mbreaks <- pretty(range(datos),10)
    
    # t0 <- tag %,% ":   (%)r for interval " %,% rownames(datos)
    t0 <- tag %,% ":   (%)r para intervalo " %,% rownames(datos)
    # t0 <- "Relación cambio SPI para intervalo " %,% rownames(datos)
    # puntos de la cuenca:
    
    fnam <- cuenca %,% components[2] %,% "_REL_" %,% (i-2) %,% ".png"
    
    if (file.exists(fnam)) file.remove(fnam)
    
    png(filename = fnam)
    
    if (resp == "1") 
        DrawContCurvs(aa, Mbreaks, pp[,c("Lon","Lat")], tit=t0, xlab = "Longitud", ylab = "Latitud")
    else 
        DrawContColors(aa, Mbreaks, Mcols, pp[,c("Lon","Lat")], tit = t0, xlab = "Longitud", ylab = "Latitud")
    
    dev.off()
    rrDatos <- rbind(rrDatos, datos)
}

# Guardaremos la información usada

fnamRD <- cuenca %,% components[2] %,% "_PrcentDATOS.RData"
save(rrDatos, file = fnamRD)


