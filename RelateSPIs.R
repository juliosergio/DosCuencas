###################################################
# RelateSPIs.R
#     Relaciona punto a punto los SPIs contenidos en 
#     dos archivos producidos por TandemSPI.R
################################################

if (!exists("LEIDO.MiBiblioteca")) source("RR/MiBiblioteca.R", chdir = T)

# Leemos el archivo que contiene los datos:
prefix <- mustGet("Nombre genérico de los archivos (csv)->") # Por ejemplo ConchosPRE_mm
fnams <- paste0(prefix, c("_SPI0.csv", "_SPI1.csv"))

# La tablas de datos:
E_spi0 <- read.csv(fnams[1], row.names = 1)
E_spi1 <- read.csv(fnams[2], row.names = 1)


# Extracción de la información que nos interesa en las tablas
ii0 <- grepl("\\(.*\\]", rownames(E_spi0))
ii1 <- grepl("\\(.*\\]", rownames(E_spi1))

h0 <- E_spi0[ii0,]
h1 <- E_spi1[ii1,]

# Cálculo de los índices de cambio:

indC <- (h1-h0)/(h1+h0)

# Guardamos los resultados

write.csv(rbind(E_spi0[1:2,], indC), file = prefix %+% "_indC.csv", row.names = T)



