##############
# GraficosGrid.R
##############
library(grid)
# Ejemplos de gráficos con grid
grid.rect(gp = gpar(lty = "dashed"))
vp1 <- viewport(x = 0, y = 0.5, w = 0.5, h = 0.5,
                just = c("left", "bottom"), name = "vp1")
vp2 <- viewport(x = 0.5, y = 0, w = 0.5, h = 0.5,
                just = c("left", "bottom"))
pushViewport(vp1)
grid.rect(gp = gpar(col = "grey"))
grid.text("Some drawing in graphics region 1", y = 0.8)
upViewport()
pushViewport(vp2)
grid.rect(gp = gpar(col = "grey"))
grid.text("Some drawing in graphics region 2", y = 0.8)
upViewport()
downViewport("vp1")
grid.text("MORE drawing in graphics region 1", y = 0.2)
popViewport()

grid.newpage()
grid.rect(gp = gpar(lty = "dashed"))
vp <- viewport(width = 0.5, height = 0.5)
pushViewport(vp)
grid.rect(gp = gpar(col = "grey"))
grid.text("quarter of the page", y = 0.85)
pushViewport(vp)
grid.rect()
grid.text("quarter of the\nprevious viewport")
popViewport(2)

grid.newpage()
pushViewport(viewport(gp = gpar(fill = "grey", fontface = "italic")))
grid.rect()
grid.rect(width = 0.8, height = 0.6, gp = gpar(fill = "white"))
grid.text(paste("This text and the inner rectangle",
                   "have specified their own gpar settings", sep = "\n"),
             y = 0.75, gp = gpar(fontface = "plain"))
grid.text(paste("This text and the outer rectangle",
                   "accept the gpar settings of the viewport", sep = "\n"),
            y = 0.25)
popViewport()
