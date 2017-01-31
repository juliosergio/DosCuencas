# JSSS: Fechas.R
# PROPOSITO: Algunos manejos de fechas.
# --------------

# Algoritmo p/determinar si un año es bisiesto:
bisiesto <- function(a) {!(a%%4) & ((a%%100) | !(a%%400))} 
bisiesto1 <- function(a) {(!(a%%4) & (a%%100)) | !(a%%400)} 

# Tabla común de los días por mes en un año no bisiesto:
tablaMes <- c(31, 28, rep(c(rep(c(31,30),2),31),2))

rdim <- function(year,month) {
    # dimension del mes para el año dado (con if)
    if (bisiesto(year) & month==2) 29 else tablaMes[month]
}

rdimT <- function(SubTabla) { # [[1]]==year, [[2]]==month
    # Misma que la anterior pero con argumentos en SubTabla
    ifelse(bisiesto(SubTabla[[1]]) & SubTabla[[2]]==2, 29, tablaMes[SubTabla[[2]]])
}

ydim <- function(year) {if (bisiesto(year)) 366 else 365}

ydimT <- function(year) ifelse(bisiesto(year), 366, 365)



calendario <- function(year) {
    # Produce un calendario para el conjunto de
    # años proporcionado, p.ej., calendario(2000:2002)
    ny <- length(year)
    nd <- rdimT(data.frame(year=rep(year,each=12), month=1:12))
    ss <- ydimT(year)
    return(data.frame(
        year = do.call(c, lapply(1:ny, function(i) rep(year[i],ss[i]))),
        month = do.call(c,lapply((1:(12*ny)-1), function(i) rep(i%%12+1,nd[i+1]))),
        day = do.call(c,lapply(nd, function(n) 1:n))
    ))
}

ThereIsDate <- function(year, month, day) {
    # Existe la fecha dada como argumento?
    day <= rdim(year, month)
}

ThereIsDateT <- function(SubTabla) { # [[1]]==year, [[2]]==month, [[3]]==day
    # Misma que la anterior pero con argumentos en SubTabla
    SubTabla[[3]] <= rdimT(SubTabla[,-3])
}

CompleteTable <- function(Tbl, Strt, End, na.val=NULL) {
    # Lo siguiente es para completar tablas (data.frames), cuyas 3 
    # primeras columnas, etiquetadas como "year", "month", "day",
    # LA CONDICION DE LAS ETIQUETAS (nombres de columnas) de las 
    # primeras 3 columas es prescindible, ya que se uniformiza, ... y
    # representan una fecha, y las demás columnas representan 
    # cualquier cantidad de variables. Los huecos, serán llenados
    # con NAs.
    # ARGUMENTOS--
    #   Tbl: La tabla de datos en cuestión.
    #   Strt: Fecha de inicio, codificada como vetor c(Anio,Mes,Dia)
    #   End:  Fecha final, codificada como vetor c(Anio,Mes,Dia)
    #         En las fechas,p.ej, c(1972,3,22), es 22 de marzo de 1972
    #   na.val: contendrá algún valor numérico que será considerado como
    #           etiqueta del NA en la tabla, Tbl, proporcionada.
    
    # Cambio de las "etiquetas" na.val, a NAs. Esto sólo se hace
    # en caso de que se haya proporcionado la etiqueta; si es NULL
    # se omite este paso
    if (! is.null(na.val)) {
        # función de comparación
        ff <- if(na.val < 0) '<=' else '>='
        # Se convierten a matriz, exclusivamente los datos
        mm <- Tbl[,-(1:3)] # Tabla sin las columnas de fecha
        # Se cambian en la matriz por NAs los valores etiquetados
        # para ello se usa la función de comparación
        mm[get(ff)(mm,na.val)] <- NA # Como vector, sin perder su estructura de matriz
        # Se sustituyen los datos originales con los nuevos:
        Tbl[,-(1:3)] <- mm
    }
    
    # Primero, elimino de la serie fechas inexistentes
    ii <- ThereIsDateT(Tbl)
    Tbl <- Tbl[ii,]
    # año inicial y final
    years <- Strt[1] # Año de inicio
    yeare <- End[1]  # Año de término
    # mezclamos con el calendario completo
    cc <- calendario(years:yeare)
    names(Tbl)[1:3] <- names(cc) # uniformiza los nombres de cols. comunes
    Tbl <- merge(cc, Tbl, all=T) 
    # ordenamos los datos - porque no sabemos si vienen en orden
    ii <- order(Tbl[[1]],Tbl[[2]],Tbl[[3]])
    Tbl <- Tbl[ii,] # Tabla Ordenada
    # Ahora se recortará a las fechas exactas de inicio y término
    # - índice de inicio:
    ii <- which(Tbl[[1]]==Strt[1] & Tbl[[2]]==Strt[2] & Tbl[[3]]==Strt[3])
    # - índice de término:
    jj <- which(Tbl[[1]]==End[1] & Tbl[[2]]==End[2] & Tbl[[3]]==End[3])
    Tbl[ii:jj,] # Tabla recortada
}

# días transcurridos en el año para una fecha dada

DaysElapsed <- function(year, month, day) {
    mm <- month - 1
    dd <- if (mm) {
        sum(rdimT(data.frame(rep(year,mm),1:mm)))
    } else 0
    day + dd
}

DaysRemaining <- function(year, month, day) ydim(year) - DaysElapsed(year, month, day)

# Para convertir formato "YYYY-MM-DD" a c(y,m,d)
Fecha2Vt <- function(sFecha) as.integer(strsplit(sFecha, "-", fixed = T)[[1]])

# Para crear funciones que usan la conversión de fechas anterior
creaF <- function(ff) function(sFecha) do.call(ff,as.list(Fecha2Vt(sFecha)))

DaysElapsedS <- creaF(DaysElapsed)
DaysRemainingS <- creaF(DaysRemaining)


