# ===============
# JSS: TranfAcc.R
#     Transforma una tabla de promedios mensuales de precipitación
#     diaria a precipitación acumulada en el mes
# =============

# Leemos el archivo que contiene los datos:
fnam <- mustGet("Archivo de datos (csv)->") # Por ejemplo ConchosPRE_mm.csv

# El nombre de la cuenca
prefix <- strsplit(fnam, ".csv")[[1]][1]

# La tabla de datos:
precT <- read.csv(fnam, row.names = 1)

ii <- strsplit(rownames(precT)[-(1:2)],split = "-")
jj <- as.integer(sapply(ii, "[", 3 )) # Número de días por mes, registrado en la fecha (rownames)
jj <- c(1,1, jj) # agregamos muiltiplicador para las coordenadas (quedan igual)
precA <- jj*precT # La nueva tabla ya multiplicada.

fnam <- prefix %,% "Acc.csv"
# guardamos
write.csv(precA, file=fnam)




