# CorrPear.R
#    Calcula la correlación de Pearson entre
#    las series SPI-6 y SPI-12 para Conchos
#    Y tambien para los r espaciales de
#    Conchos

load("ConchosPRE_mm_6Struct.RData")
mdd6 <- mdd
load("ConchosPRE_mm_12Struct.RData")
mdd12 <- mdd
mdd <- mdd6
names(mdd)[3] <- "spi-6"
mdd$"spi-12" <- mdd12$spi
corr <- cor(mdd$`spi-6`, mdd$`spi-12`)
print(corr)

load("/media/checo/7B2C787106E895ED/PROY.CONACYT/Conchos_6_PrcentDATOS.RData")
rrDatos6 <- as.data.frame(t(rrDatos))
load("/media/checo/7B2C787106E895ED/PROY.CONACYT/Conchos_12_PrcentDATOS.RData")
rrDatos12 <- as.data.frame(t(rrDatos))
Corrs <- mapply(cor, rrDatos6, rrDatos12)
write.csv(as.data.frame(Corrs), file = "CorrelacionesEspaciales.csv")
