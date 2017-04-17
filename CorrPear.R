# CorrPear.R
#    Calcula la correlación de Pearson entre
#    las series SPI-6 y SPI-12 para Conchos

load("ConchosPRE_mm_6Struct.RData")
mdd6 <- mdd
load("ConchosPRE_mm_12Struct.RData")
mdd12 <- mdd
mdd <- mdd6
names(mdd)[3] <- "spi-6"
mdd$"spi-12" <- mdd12$spi
corr <- cor(mdd$`spi-6`, mdd$`spi-12`)
print(corr)
