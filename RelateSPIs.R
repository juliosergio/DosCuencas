###################################################
# RelateSPIs.R
#     
################################################

###################################################
# Standardized Precipitation Index
# Joseph Wheatley Biospherica March 2010
################################################

if (!exists("LEIDO.MiBiblioteca")) source("RR/MiBiblioteca.R", chdir = T)

# Leemos el archivo que contiene los datos:
prefix <- mustGet("Nombre genérico de los archivos (csv)->") # Por ejemplo ConchosPRE_mm
fnams <- paste0(prefix, c("_SPI0.csv", "_SPI1.csv"))

# La tablas de datos:
E_spi0 <- read.csv(fnams[1], row.names = 1)
E_spi1 <- read.csv(fnams[2], row.names = 1)

# Averigüemos los incrementos y los mínimos en x y y
xx <- unique(sort(as.numeric(E_spi0["Lon",])))
yy <- unique(sort(as.numeric(E_spi0["Lat",])))

id.mode <- function(tt) as.numeric(names(tt)[which.max(tt)])  # identifica la moda en una tabla de fecuencias
# La moda de una serie de datos es la composición de la función table(), que calcula las frecuencias
# en la serie, con id.mode()
stat.mode <- id.mode %cmp% table # stat.mode(x) donde x es la serie de datos
get.dif <- function(x) stat.mode(x-lag(x))

dx <- get.dif(xx)
dy <- get.dif(yy)

rx <- range(xx)
ry <- range(yy)

# Extracción de la información que nos interesa en las tablas
ii0 <- grepl("\\(.*\\]", rownames(E_spi0))
ii1 <- grepl("\\(.*\\]", rownames(E_spi1))

h0 <- E_spi0[ii0,]
h1 <- E_spi1[ii1,]

# Cálculo de los índices de cambio:

indC <- (h1-h0)/(h1+h0)

