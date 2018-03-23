###############
# ParaArtAtm.R
###############
library(tidyverse)
library(lubridate)
library(MASS)
library(grid)
library(gridExtra)
# library(ggpubr)


source("RR/MiBiblioteca.R", chdir = T)
source("Density.R")
source("spi_functions.R")
source("RR/CoordinatedDrw.R", chdir = T)

f0 <- read.csv("SequiasPresasConchosENG.csv")
# f0 <- as.tibble(f0)

f0$Ini.Date <- as.Date(f0$anio.inicial %,% "-01-01")
f0$Fin.Date <- f0$Ini.Date
year(f0$Fin.Date) <- year(f0$Fin.Date) + f0$duracion


# xx <- ggplot(f0, mapping = aes(x=anio.inicial, y=dam)) + labs(x = "Years", y="Dams")
# xx <- xx + geom_segment(
#     aes(x=anio.inicial, y=dam, 
#         xend=anio.final+1, yend=dam, col=drought.severity), size=8) +
#     scale_x_continuous(limits = c(1960,2012))
# xx

xx <- ggplot(f0, mapping = aes(x=Ini.Date, y=dam)) + labs(x = "Years", y="Dams")
xx <- xx + geom_segment(
    aes(x=Ini.Date, y=dam, 
        xend=Fin.Date, yend=dam, col=drought.severity), size=8) +
    scale_x_date(limits = as.Date(c("1960-01-01","2009-01-01"))) #"2013-01-01")))
xx

# ggsave("SequiasPresas.png")
ggsave("SequiasPresas.svg")

# ======= PARA ARTÍCULO ==============================
# Se necesita la serie de precipitaciones mensuales:
fn="ConchosPRE_mm.csv"
dd <- read.csv(fn, row.names = 1)

# Exclusivamente las series de datos, 
# sdd <- dd[3:(ne+2),] # Se eliminan coordenadas y meses extra (<<YA NO)
sdd <- dd[-(1:2),] # Se eliminan coordenadas

# Hagamos la serie resumen de los datos:

ss <- apply(sdd,1,mean) # <- Esta es la serie de precipitaciones necesaria
nm <- length(ss) # Número total de meses
na <- floor(nm/12) # Número de años completos
# Tenemos que asegurarnos que na sea par:
na <- if (na%%2) na-1 else na
# El número de meses efectivos que se tratarán:
ne <- na*12

ini <- nm - ne + 1 # Inicio real de la serie

mdd <- data.frame(
    # Fecha = as.Date(names(tail(ss,ne))),
    Ini.Date = as.Date(names(tail(ss,ne))),
    SPI.12 = getSPInew_for_k(ss, 12, ffcreadora=creaCumGamma, ini=ini),
    SPI.24 = getSPInew_for_k(ss, 24, ffcreadora=creaCumGamma, ini=ini)
)

# r <- DrwSeries(mdd, 3, xlab = "Years")
r <- ggplot(mdd, aes(x=Ini.Date, y=SPI.24)) + geom_col() + xlab("Years")
r <- r + geom_vline(xintercept = as.Date.character("1985-01-01"), colour="darkred", linetype=2)
r <- r + geom_hline(yintercept = c(-1,1), colour="darkblue", linetype=2) +
    scale_x_date(limits = as.Date(c("1960-01-01","2009-01-01"))) #"2013-01-01")))
r

# ggsave("ConchosSPI_12_24.png", width = 8.5, height = 4.8)
ggsave("ConchosSPI_24.svg", width = 7.39*8/11.36, height = 4.34*0.7, units = "in")

ggarrange(xx, r, heights = c(2, 1), ncol = 1, nrow = 2, align = "v")


#================================
# png("Alineadas.png")
# grid.newpage()
# vp1 <- viewport(x=0.5, y=0.75, width = 1, height = 0.5)
# # vp2 <- viewport(x=0.5, y=0.25, width = 1, height = 0.5)
# vp2 <- viewport(x=0.102057, y=0.25, just = c("left", "center"), width = 0.70364, height = 0.5)
# # pushViewport(vp1)
# # upViewport()
# # pushViewport(vp2)
# # pushViewport(viewport(layout = grid.layout(2, 1)))
# # vplayout <- function(x) viewport(layout.pos.row = x, layout.pos.col = 1)
# g1 <- grob(xx, vp = vp1)
# g2 <- grob(r, vp = vp2)
# grid.draw(g1)
# grid.draw(g2)
# dev.off()
