# Tests

library(ggplot2)
source("spi_functions.R")
#debugSource("spi_functions.R") # Si se quiere correr esta parte con debbuger

source("spi_functions.R")
#debugSource("spi_functions.R") # Si se quiere correr esta parte con debbuger

# Leemos el archivo que contiene los datos:
fn <- mustGet("Archivo de datos (csv)->") # Por ejmplo:   ConchosPRE_mm.csv
prefix <- strsplit(fn, ".csv", fixed = T)[[1]]


k <- as.numeric(mustGet("Período SPI (meses):"))
# La tabla de datos:
dd <- read.csv(fn, row.names = 1)


# Para partir la tabla en años exactos

nm <- nrow(dd) - 2 # Número total de meses
na <- floor(nm/12) # Número de años completos
# Tenemos que asegurarnos que na sea par:
na <- if (na%%2) na-1 else na
# El número de meses efectivos que se tratarán:
ne <- na*12

ini <- nm - ne + 1 # Inicio real de la serie

# Exclusivamente las series de datos, ajustando
# también al número de meses efectivos
# sdd <- dd[3:(ne+2),] # Se eliminan coordenadas y meses extra (<<YA NO)
sdd <- dd[-(1:2),] # Se eliminan coordenadas


# ne es par

n2 <- ne/2

# Hagamos la serie resumen de los datos:

ss <- apply(sdd,1,mean)

# Se divide la serie a la mitad y se calculan los dos SPIs
# (uno para cada sub-serie), y creamos la salida 

spiX <- getSPIfor_k(ss,k,ini)
names(spiX) <- tail(names(ss),ne)

mdd <- tbl_df(data.frame(Fecha=names(spiX), pre=tail(ss,ne), spi=spiX))

# Transformemmos
mxx <- mdd %>% gather(variable, value, pre:spi)

p <- ggplot(mxx, aes(x=as.Date.character(Fecha), y=value)) + xlab("Fecha")
p + geom_col() + facet_grid(variable ~ ., scale="free_y") + ylab("Valor")
