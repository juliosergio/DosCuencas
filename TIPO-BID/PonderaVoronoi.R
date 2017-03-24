# ==============================
# PonderaVoronoi.R
#    Conjunto de funciones para hacer un
#    promedio ponderado a partir de los
#    poligonos de Voronoi
# ==============================
library(sp)
library(deldir) # Delauney & Voronoi
library(rgeos)

expandbb <- function (bb, fe) {
    # expande y traspone un bounding box
    bb <- t(bb) # traspone
    dlt <- (bb["max",] - bb["min",])*fe
    bb + rbind(-dlt, dlt)
}

HacerSPoly <- function (mtx, id) {
    mtx <- if (Reduce('&', mtx[1,]==mtx[nrow(mtx),])) {
        mtx
    } else {
        rbind(mtx, mtx[1,]) # copia el primer vértice al final
    }
    SpatialPolygons(
        list(
            Polygons(
                list(Polygon(mtx)),
                ID=id
            )
        )    
    )
}

voronoipolygons <- function(v, bb, fe=0.25) {
    # Encuentra los polígonos de Voronoi recortados contra un bounding box
    # v:  los vértices, dados como un conjunto de puntos (de acuerdo con la
    #     descripción de 'sp') o simplemente como una matriz cuyas columnas
    #     son las coordenadas y cuyos renglones c/u de los vértices.
    # bb: el bounding box, tal como lo da 'sp', esto es coordenadas por
    #     renglones (así que es necesario transponer)
    # fe: factor de expansión del bounding box, esto es lo que aumenta
    #     en cada extremo.
    # ---------------------------------
    # coordenadas de los vértices: 
    crds <- if (.hasSlot(v, 'coords')) v@coords else v
    rw = expandbb(bb, fe)
    # entra como vector en deldir, si no, no entiende
    dim(rw) <- NULL
    z <- deldir(crds[,1], crds[,2],rw=rw)
    w <- tile.list(z) # poligoos de Voronoi
    polys <- vector(mode='list', length=length(w))
    for (i in seq(along=polys)) {
        pcrds <- cbind(w[[i]]$x, w[[i]]$y)
        pcrds <- rbind(pcrds, pcrds[1,]) # cierra el polígono
        polys[[i]] <- Polygons(list(Polygon(pcrds)), ID=as.character(i))
    }
    SP <- SpatialPolygons(polys)
    
    voronoi <- SpatialPolygonsDataFrame(
        SP, 
        data=data.frame(
            x=crds[,1], 
            y=crds[,2], 
            row.names=sapply(slot(SP, 'polygons'), function(x) slot(x, 'ID'))
        )
    )
    
    return(voronoi)
}


PonderaVoronoi <- function(VrtxSet, Enclosing) {
    # Dado un conjunto de Vertices (VrtxSet), en un área que los
    # contiene (Enclosing), encuentra los pesos asignados a cada vértice
    # de acuerdo a una partición de Voronoi.
    # El resultado se entrega simplemente como un vector numérico (cuya suma
    # sera 1), con los pesos, en orden, correspondientes a cada vértice.
    
    # Caso trivial: un solo vértice
    if (length(VrtxSet)==1) return (1)
    Enclosing <- HacerSPoly(Enclosing, "cc-")
    A <- gArea(Enclosing)
    voronoiPolis <- voronoipolygons(VrtxSet, bbox(Enclosing))
    
    # Se hace la intersección:
    ii <- gIntersection(Enclosing, voronoiPolis, byid = T)
    cca <<- Enclosing
    vvp <<- ii  
    
    #ERRONEO> sapply(ii@polygons, function(elt) elt@Polygons[[1]]@area)/A
    #ERRONEO> ya que al recortar contra la cuenca un poligono de voronoi
    #         pudiera haber quedado dividido en varios (sub)polígonos, y
    #         el código anterior solamente tomaría el primero.
    #  Lo correcto es:
    sapply(ii@polygons, function(elt) elt@area)/A
}


cca <- NULL
vvp <- NULL

test <- function() {
    # Los datos de entrada son matrices, cuyas columnas
    # corresponden a cada una de las coordenadas en
    # cuestion: x, y, ...
    
    cc <- cbind(x=c(0, 2, 3, 4,  4, 1, 0), 
                y=c(0, 0, 1, 1,3.5, 4, 2))
    vv <- cbind(x=c(1.5,  2, 2.5),
                y=c(2.5,  1,   3))
    PonderaVoronoi(vv, cc)
}

