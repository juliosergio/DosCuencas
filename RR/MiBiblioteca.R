# ==========================
# JSS: MiBiblioteca.R
# ==========================

# Variable para detectar que se ha leido Biblioteca:
LEIDO.MiBiblioteca <- TRUE

library(dplyr)
library(tidyr)
library(stringdist)
composite <- function(f,g) function(...) f(g(...))
# como operador:
`%cmp%` <- composite # Operador de composición de funciones

`%,%` <- function(x, y) paste0(x, y) # Operador de concatenación de cadenas

`%//%` <- function(x,y) as.integer(x/y) # División entera


bdir <- "" # YA-NO>> "E:/RR/"
source(bdir %,% "ManejaErrores.R")

source(bdir %,% "Sustituyes.R")
source(bdir %,% "Geodetic.distance.R")
source(bdir %,% "Intercala.R")

mustGet <- function (prompt, default="", inclSet=NULL) {
    # Obtiene un dato en línea, posiblemente obligando a que
    # se encuentre en un conjunto de strings dado:
    # USO: rr <- mustGet("De una respuesta [S/N] >", "n", c("s","S","n","N"))
    # o cheque contra una expresión regular:
    # USO: rr <- mustGet("De una fecha [YYYY-DD-MM] >", "", E_Fecha)
    repeat {
        resp <- if ((resp<-readline(prompt))=="") default else resp
        if (resp != "") {
            if (is.null(inclSet)) return(resp)
            # Lo que viene en inclSet es o un conjunto de strings
            # -- sólo explicable si length(inclSet) >= 2
            # o una expresión regular que conformaría el tipo de
            # respuesta aceptable:
            # CASO 1: Conjunto de strings:
            if (length(inclSet) > 1) {
                if (resp %in% inclSet) return(resp)
            } else if (grepl(inclSet, resp)) #<- CASO 2: Expresión regular:
                return(resp) 
            # Todos los otros casos siguen en el ciclo
        }
    }
}

group.mean <- function(x, ini=1, size=12) {
    # Hacer la media por grupos donde el tamaño de cada grupo
    # es 'size'. La media se inicia a partir índice inicia 'íni'
    # Se puede usar para calcular promedios de acumulados anuales
    # ------------
    
    # número total de elementos a considerar
    n <- length(x) - ini + 1
    m <- floor(n/size) # Número de grupos completos
    fin <- ini + m*size -1
    sum(x[ini:fin])/m
}

# Funciones para la moda

id.mode <- function(tt) as.numeric(names(tt)[which.max(tt)])  # identifica la moda en una tabla de fecuencias
# La moda de una serie de datos es la composición de la función table(), que calcula las frecuencias
# en la serie, con id.mode()
stat.mode <- id.mode %cmp% table # stat.mode(x) donde x es la serie de datos
get.dif <- function(x) stat.mode(x-lag(x))


dist2 <- function(pts, p0, pw=1) {
    # Calcula la distancia del punto p0 a todos los puntos en el conjunto pts
    r <- (pts[,1]-p0[,1])^2 + (pts[,2]-p0[,2])^2
    if (pw==2) r else r^(pw/2)
}

mdist2 <- function(pts, mp0, pw=1) {
    # multiple de lo anterior: es decir entrega una matriz en que
    # cada una de las columnas contiene las distancias de cada uno de
    # los puntos de pts al punto correspondiente de mp0
    sapply(as.data.frame(t(mp0)), function(p) dist2(pts, t(p), pw))
}

# Está vacío un vector?
is.empty.v <- function(v) !as.logical(length(v))

composite <- function(f,g) function(...) f(g(...))
# como operador:
`%cmp%` <- composite # Operador de composición de funciones

`%,%` <- function(x, y) paste0(x, y) # Operador de concatenación de cadenas

# Comparación que incluye NAs
compareNA <- function(v1,v2) {
    same <- (v1 == v2) | (is.na(v1) & is.na(v2))
    same[is.na(same)] <- FALSE
    return(same)
}
# como operador
`%=%`<- compareNA

# Extrae primer elemento de una lista
bareId <- function (L) L[[1]]

## bareIId <- function(f, L, ...) f(bareId(L), ...)


# Productores de funciones
# = con primer argumento =
ffun <- function(f, ...) function(x) f(x, ...)
# p.ej. para hacer una función 'convert' (ver definición abajo) pero que multiplique por
# 500 en vez de 1/2.54 sería con:
#  conv50 <- ffun(convert, 50)

# = con argumento nombrado =
ffun0 <- function(f, name, ...) function(x) f(assign(name,x), ...)
# p.ej. ffun0(gsub, "x", pattern=",", replacement=""), crea una función
# que elimina todas las comas de su único argumento.

readCsv <- function(...) read.csv(..., stringsAsFactors = F)
writeCsv <- function(...) write.csv(..., row.names = F, na="")

readTable <- function(...) read.table(..., stringsAsFactors = F, header = T)


chkSintx <- as.logical %cmp% length %cmp% grep
forceNum <- function (x, na.as.0=F) tryCatch.W.E({
    x <- as.numeric(x)
    if (na.as.0) x[is.na(x)] <-0
    x
})$value

tstchr <- function(x, set="") is.na(x) | x %in% set

extrae <- function (Expr, ss) regmatches(ss, regexec(Expr, ss))

# Hace match de un conjunto de "expresiones regulares" contra una tabla de
# strings (un catálogo), el resultado es (a) un arreglo de lógicos indicando
# los elementos que se encontraron en la tabla, o (b) los índices a los elementos
# de la tabla correspondientes a cada una de las expresiones dadas en expSet
rexpsMatchL <- function(expSet,Tbl) apply(sapply(expSet, grepl, Tbl),1,any)
rexpsMatch <- function(expSet,Tbl) {
    # aqui la tabla de salida es del tamaño de expSet
    x <- sapply(expSet, grep, Tbl)
    x[sapply(x, function(e) !length(e))] <- NA
    unlist(x)
}

cambiaMultiple <- function(patrns, reemplazos, x) {
    n <- length(x)
    mask <- rep(TRUE, n)
    for (i in 1:length(patrns)) {
        inds <- mask
        inds[mask] <- grepl(patrns[i], x[mask])
        x[inds] <- reemplazos[i]
        mask <- mask & !inds
    }
    x
}

# corta registros de una tabla de acuerdo
# a una condición dada:
cuTable <- function (Tb, cond) Tb[cond,]

ConvierteNumeric <- forceNum %cmp% bareId

# Para convertir strings que representan números pero que en esa expresión
# contienen comas, se usan las siguientes funciones
ConvNumConComas00 <- forceNum %cmp% ffun0(gsub, "x", pattern=",", replacement="")

ConvNumConComas <- ConvNumConComas00 %cmp% bareId

# Para cambiar un caracter por otro
cambia <- function(x, patrn=",", repl=".", ...) gsub(patrn, repl, x, ...)
numericConComaDec <- forceNum %cmp% cambia %cmp% bareId


# Expresiones gramaticales
E_SI <- "^[[:blank:]]*[Ss][IiÍí][[:blank:]]*$" # Sí
E_SIoX <- E_SI %,% "|^[[:blank:]]*[Xx][[:blank:]]*$"
E_OTRO <- " ?OTRO ?" # OTRO
E_OBJ <- "[[:alpha:]]+[[:digit:]]*" # Objeto: letras + num
E_DefOBJ <- paste0(E_OBJ, ":") # Definición de Objeto
P_Gpo <- "^(.+)\\[\\[([[:digit:]]+)\\]\\],([[:alpha:]]+)"
E_Gpo <- paste0(P_Gpo, "\\.?$")
E_GpoFin <- paste0(P_Gpo, "\\.$")
E_Fin <- "\\.$"
E_Par <- "^\\((.+)\\)$"
Es_Continuacion <- "^\\.\\."
Es_Inline <- "^::"
Es_Indexado <- ".+\\[(.+)\\]"
E_1stAplfa <- "[^[:alpha:]]*([[:alpha:]]+)[^[:alpha:]]*"
#>> E_EdoInRPD <- "[^[:alpha:]]+([[:alpha:]]+)[^[:alpha:]]*"
E_1stMultAlfa <- "[^[:alpha:]]*([[:alpha:]][[:alpha:]]+)[^[:alpha:]]*"

E_1stToken <- "(^[[:alpha:]]?[^[:alpha:]]+)[[:alpha:]]"
# E_comaYOblancos <- "[[:blank:]]+,?[[:blank:]]*|[[:blank:]]*,?[[:blank:]]+|,"
E_comaYblancos <- "[[:blank:]]*,[[:blank:]]*"
E_comaGYblancos <- "[[:blank:]]*[-,][[:blank:]]*" # Coma o guión con blancos
E_comaYOblancos <- E_comaYblancos  %,% "|[[:blank:]]+" 
E_comaYbl_O_Y <- E_comaYblancos %,% "|[[:blank:]]+[YyEe][[:blank:]]+"
E_comaYbl_O_Yg <- E_comaYblancos %,% "|[[:blank:]]+[-YyEe][[:blank:]]+" # con guión
# E_grds <- "[[:blank:]]*[°º'\\\"][[:blank:]]*|[[:blank:]]+"
E_grds <- "[^.[:digit:]]+"


# Número de anexo denro de un string
E_anxIn <- "[^[:digit:]]*[[:digit:]]+[,.]([[:digit:]]+)[^[:digit:]]*"
# Exclusivamente un entero:
E_excInt <- "^[[:digit:]]+$"
E_Fecha <- "^([[:digit:]]+-){2}[[:digit:]]+$"

sepElts <- ffun(strsplit, E_comaYbl_O_Yg)

# ------------------------------------
# Método de Horner para polinomios:
horn <- function(coefs, x=1/60) { # se usará p/convertir a grad. decimales
    ss <- 0 # La suma inicial
    for (i in length(coefs):1) 
        ss <- coefs[i] + ss*x
    ss
}

# Si de antemano se dan los coeficientes en orden inverso; esto es,
# el coeficiente para la x con mayor potencia primero y así para llegar 
# al coeficiente "independiente" al final, se puede usar esta versión,
# que resulta la más eficiente
rhorn <- function(coefs, x=1/60) { # se usará p/convertir a grad. decimales
    ss <- 0 # La suma inicial
    for (e in coefs) 
        ss <- e + ss*x
    ss
}


# Otra forma menos eficiente
fff <- function(a,b,x) x*a+b
horn1 <- function(coefs, x=1/60) {
    Rc <- coefs[length(coefs):1]
    Reduce(function(a,b) fff(a,b,x), Rc, 0)
}


# Cadenas representando grados con ° ' "
sepGrds <- ffun(strsplit, E_grds)
# Como numérico:
ToDecGrds00 <- function(s) {
    ss <- sepGrds(s)
    ss[sapply(ss, is.empty.v)] <- NA
    apply(
        do.call(rbind, ss),
        1,
        horn %cmp% as.numeric
    )
}

# Para usar con subtabla:
ToDecGrds <- ToDecGrds00 %cmp% bareId
# Con el negativo:
ToMinusDecGrds <- function(x) -ToDecGrds(x)


Extrae1stAlpha <- function (X) sapply(extrae(E_1stAplfa,X), '[', 2)
ExtraeEdo <- function (X) sapply(extrae(E_1stMultAlfa,X), '[', 2)
ExtraeRegAdm <- function (X) sapply(extrae(E_1stToken,X), '[', 2)
ExtraeRegHid <- function (X) sapply(extrae("/(..)",X), '[', 2)

ExtraeNAnx <- function (X) sapply(extrae(E_anxIn,X), '[', 2)



# ==============================
# FUNCIONES AUXILIARES:


# Evaluación de strings:
evalstr <- function(s, ...) eval(parse(text=s), ...)

# Operaciones entre miembros de la subTabla:
fform <- function (subTabla, expr, nvar="X", nfun="forceNum") {
    # Las expresiones expr serán del tipo:
    #  "X[[2]] * X[[1]] + X[[3]]" donde los X[[i]] son los elementos de subTabla
    e1 <- sys.frame(sys.nframe()) # El ambiente de la función
    nexp <- nvar %,% "(.+?]])"
    nval <- nfun %,% "(subTabla\\1)"
    # eval(parse(text=gsub(nexp, nval, expr)))
    evalstr(gsub(nexp, nval, expr), e1)
}

sintetizaCampos <- function(subTabla, Catalogo, ord =NULL, expr=E_SI, tstfun=tstchr, ...) {
    # El Catalogo puede exceder trayendo al final el campo correspondiente
    # a "OTRO".
    ff <- paste0("as.",class(Catalogo[[1]]))
    res <- get(ff)(rep(NA,nrow(subTabla)))
    tst1arg <- function(a) !tstfun(a, ...)
    A_Asignar <- apply(
        sapply(subTabla, tst1arg),
        1,
        function(r) Reduce('|',r)
    )
    
    arrNoms <- if (is.null(ord)) Catalogo[[1]] else Catalogo[[1]][ord]
    lst <- length(arrNoms)
    #>>> mm <- subTabla == "SI" # matriz
    # Manejo de la función que prueba la "veracidad" en cada columna de la subtabla;
    # si se compara con un texto, p.ej., "VERDADERO" o "X", se usa grepl contra tal
    # columna, si se ha provisto una función, se usa ésta, y si no se ha provisto
    # nada, la prueba es contra el vacío, que está representado por el negativo de
    # la función 'tstfun'.
    fun <- if (is.character(expr)) {
        function(ccol) grepl(expr, ccol)
    } else if (is.function(expr)) {
        expr
    } else tst1arg
    
    mm <- sapply(subTabla[A_Asignar,], fun)
    mmod <- apply(mm, 1, function(e) paste0(arrNoms[e], collapse = " "))
    res[A_Asignar] <- ifelse(lst > length(subTabla) & mmod == "", arrNoms[lst], mmod)
    return (res)
}


pegaCampos <- function(subTabla, sep=", ") {
    apply(subTabla, 1, paste0, collapse = sep)
}

embedCampos <- function(subTabla, sep="/", prefix="", postfix="") {
    # si hay NAs en un registro, se descarta:
    ii <- !(apply(subTabla, 1, function(v) any(is.na(v))))
    rr <- NULL
    rr[ii] <- prefix %,% apply(subTabla[ii,], 1, paste0, collapse = sep) %,% postfix
    rr
}


TSubtabla <- function(subTabla) {
    lapply(subTabla, names)
}


mergeStr <- function(desc, dato) {
    ifelse(is.na(dato) | dato=="", "", desc %,% "=(" %,% dato %,% ")")
}

pegaNombrado <- function(subTabla, sep="; ") {
    nn <- as.list(names(subTabla))
    apply(
        do.call(cbind,
            # lapply(1:length(nn), function(i) mergeStr(nn[i], subTabla[,i]))),
            mapply(mergeStr, nn, subTabla, SIMPLIFY = F)),
        1,
        paste0,
        collapse=sep
    )
}

filtraNombres <- function(SubTabla, Nombs=names(SubTabla), tstVal='VERDADERO') {
    # Cada tstVal en una columna "prende" el valor del nombre asociado a la columna
    # y se concatena (por renglones) en el resultado final.
    
    LsubT <- (SubTabla == tstVal) # Matriz de lógicos
    # Eliminamos los NAs de la matriz:
    LsubT[is.na(LsubT)] <- FALSE
    apply(LsubT, 1, function(rr) paste0(Nombs[rr], collapse = " "))
}

calcSegmentado <- function(SubTabla,
                           # vPrueba, será 1a. col de SubTabla
                           valsTest, 
                           default=vector(mode(SubTabla[[1]]),1)) 
{
    vPrueba <- SubTabla[[1]] # Es la 1a. col de SubTabla
    
    # De acuerdo a índices calculados contra la tabla 'valsTest'
    # para cada elemento en vPrueba elige el elemento en la columna
    # correspondiente al índice en SubTabla:
    ii <- match(vPrueba, valsTest) + 1 # se incrementa índice por "cbind" abajo
    r <- apply(cbind(ii,SubTabla[,-1]), 1, function(rr) rr[rr[1]])
    r[is.na(r)] <- default
    return(r)
}

ini <- function(size, type, na.ini = F) {
    if (na.ini) return(get("as." %,% type)(rep(NA, size)))
    vector(mode = type, size)
}

# === INTERPRETE ===
# Evaluación de una expresión dada de 
# acuerdo a la gramática:
#
# Descr = Bloque, {Bloque}.
# Bloque = Def | Agr.
# Def = Enca, Elt, {Elt}.
# Agr = Enca1, Elt, {Elt}.
# Enca = Dest, [Prefix].
# Dest = Obj, ":", Nomb.
# Prefix = Nomb.
# Enca1 = Dest1, [Prefix].
# Dest1 = Obj.
# -----------------------
StrEval <- function(s) {
    return(eval(parse(text=s)))
}

descomenta <- function (arrS) {
    # Los elementos que empiezan con '#' son 
    # comentarios, los quitaré de arrS
    arrS[grep("^#", arrS, invert = T)]
}

# Lectura y conversion a dplyr

toDplyr <- tbl_df %cmp% read.csv
toDplyrNf <- tbl_df %cmp% readCsv # sin considerar como Factores los strings

# dimensión de tablas
nnn <- nrow

# ===================================================
# Lo que sigue sustituye a source("Calc_Mpio_RHA.R") # Municipios
# Ver ejemplo en deCatalogo(), abajo

SubCatalogo <- function(
    NomCat, 
    ExtCat, 
    indSel= # Si no se da, se captura:
        as.integer(
            {
                # IDENTIFICACIÓN DE LA OPERACIÓN:
                write("--------------------------------", "")
                write(
                    paste0(
                        " Intentando identificar para: ",
                        NomCat
                    ),
                    ""
                )
                write("--------------------------------", "")
                print(read.csv(ExtCat))
                unlist( strsplit(
                    readline("Claves num.de lista anterior,posibles(orden prioridad):"),
                    "[ |,]+"
                ))
            }
        )
) 
{
    # Catálogo completo
    mm <- read.csv(NomCat, 
                   stringsAsFactors = F, 
                   header = T)
    lst <- length(mm)
    # tendré dos sub-tablas, una con sólo la clave principal
    # En el catalogo la clave clasificadora será la última 
    # columna
    t1 <- mm[mm[[lst]] == indSel[1],]
    
    if (length(indSel) == 1) return (list(t1))
    
    # Subtabla-municipio de todos los estados elegidos
    smm <- do.call(
        rbind, 
        lapply(indSel, function(elt) mm[mm[[lst]] == elt,])
    )
    # Note que smm es una "supertabla" de t1, esto es,
    # t1 va al principio de smm, por lo tanto, los 
    # índices de t1 son los mismos en smm
    
    list(t1, smm)
}

leeCat0 <- function (NomCat) {
    # Catálogo completo
    read.csv(NomCat, 
             stringsAsFactors = F, 
             header = T)
}

# LeeCat sólo mete en una lista el 
# resultado de la anterior función:
leeCat <- list %cmp% leeCat0

#>> leeCat <- function (NomCat) {
#>>     # Catálogo completo
#>>     mm <- read.csv(NomCat, 
#>>                    stringsAsFactors = F, 
#>>                    header = T)
#>>     list(mm)
#>> }

deCatalogo <- function( # Ver descripcion de argumentos abajo 
    SubTabla,
    # yy,
    SubCat # Si es un sólo catalogo dar list(Catalogo)
) 
{
    # SubTabla: es la tabla básica cuya primer columna es el string
    #     correspondiente al campo principal y las demás son accesorias;
    #     es decir, que pueden ayudar a identificar el dicho campo, en
    #     orden de prioridad.
    # SubCat: lista: contiene un catalogo, o un catalogo subdividido
    #
    # P.ej. para catalogo de municipios llamar:
    #   deCatalogo(yy[, c("CGAMUNIC", "CGALOCAL")], 
    #              SubCatalogo("CatalogoMunicipios.csv", "CatalogoEstadosW.csv"))
    
    # El número de columnas de yy:
    #  
    numCols <- ncol(SubTabla)
    
    if (length(SubCat)==1) {
        t1 <- smm <- SubCat[[1]]
    } else {
        t1 <- SubCat[[1]]
        smm <- SubCat[[2]]
    }
    
    # IDENTIFICACIÓN DE LA OPERACIÓN:
    write("--------------------------------", "")
    write(
        paste0(
            " Intentando identificar para: ",
            names(SubTabla)[1], "/", names(smm)[2]
        ),
        ""
    )
    write("--------------------------------", "")
    
    lst <- length(smm)
    
    # Se elimina basura al principio del campo primero
    # que es el que corresponde a la descripción en SubTabla
    #>> yy$CGAMUNIC <- sub("[[:digit:]|[:blank:]]+(.*)", "\\1", yy$CGAMUNIC)
    SubTabla[[1]] <- sub("^[[:digit:]|[:blank:]]+", "", SubTabla[[1]])
    
    # Se convierte a mayúsculas el campo
    # y se quitan acentos
    SubTabla[[1]] <- StandardText(SubTabla[[1]])
    
    # En el SubCatalogo (t1 o smm), la clave viene en el campo 1 y
    # la descripción en el campo 2
    ii <- amatch(SubTabla[[1]], t1[[2]], maxDist = 2)
    # Los strings vacíos no serán tomados en cuenta (clave==0):
    empties <- tstchr(SubTabla[[1]]) # Vacíos y NAs
    #>>> claves[empties] <- 0
    ii[empties] <- 0
    cambiar <- NULL # Manejo de reemplazos
    repeat { # repeat1
        if (is.null(cambiar)) {
            # Los no asignados:
            ii.no.asignados <- which(is.na(ii))
            if (length(ii.no.asignados)==0) break
            # Se prueba el primero:
            ss <- SubTabla[ii.no.asignados[1], ] # con cols. accesorias
            write("=============================", "")
            write("NO SE ENCONTRO un MATCH para:", "")
            print(ss)
            write("------", "")
        }
        iss <- stringsim(ss[[1]], smm[[2]], method='jw', p=0.1)
        # Los que son mayores de 0.60
        kk <- which(iss >= 0.60)
        oo <- kk[order(iss[kk], decreasing = T)]
        ## smm[[2]][oo] # Nombres ordenados
        ## iss[oo]        # Pesos c/nombre
        lnoo <- length(oo)
        
        repeat { # repeat2
            if (lnoo > 0) {
                writeLines(
                    paste0("   (", 
                           paste0(names(smm)[-1], collapse = ","), 
                           ")"
                    )
                )
                writeLines(
                    head(paste0(
                        1:lnoo, ") ", 
                        do.call(
                            function(...) paste(..., sep="\t"),
                            smm[oo,2:lst, drop=F]
                        )
                    ), 100) # limitamos la impresión a 100 renglones
                )
            }
            write("----", "")
            writeLines(
                paste(
                    (lnoo+1):(lnoo+2), 
                    c("**REMPLAZAR CON...**", "**NULIFICAR**"), 
                    sep = ") "
                )
            )
            write("----", "")
            n <- as.integer(readline("Elija su opción:>"))
            
            if (!is.na(n) & n %in% 1:(lnoo+2)) break 
            write      ("******************************", "")
            write(paste("* ATENCIÓN: Debe dar un número entre", 1, "y", (lnoo+2)), "")
            write      ("******************************", "")
        } # END-repeat2 
        
        subcond <- if (numCols==1) T else SubTabla[[2]] %=% ss[[2]] # Compara con NAs
        
        if (n <= lnoo) {
            ind <- oo[n] # ind. de Campo SubTabla[[1]] en smm
            # peso del campo:
            ## iss[ind]
            # Todos los que coincidan en yy con este se deben cambiar en ii
            # a indpio
            
            # -----
            # Para comparación, se agrega o no se agrega campo secundario?
            # si el match de la opción elegida es casi perfecto, no se necesita 
            # agregar subcondición, o si yy es de una sola columna:
            subcond <- if (iss[ind] >= 0.8) T else subcond
            
            cambiar <- if (is.null(cambiar)) {
                which(subcond & SubTabla[[1]] == ss[[1]])
            } else {cambiar}
            ii[cambiar] <- ind
        } else {
            #>> subcond <- if (numCols==1) T else yy[[2]] == ss[[2]]
            n <- n - lnoo
            if (n==1) { # REMPLAZAR con ...
                reemp <- StandardText(
                    readline("Indique su string de REEMPLAZO:>")
                )
                # Se reemplazaran donde ha habido coincidencias
                cambiar <- if (is.null(cambiar)) {
                    which(subcond & SubTabla[[1]] == ss[[1]])
                } else {cambiar}
                ss[[1]] <- reemp
                # y va de nuevo desde el principio
                next
            }
            # NULIFICAR
            cambiar <- if (is.null(cambiar)) {
                which(subcond & SubTabla[[1]] == ss[[1]])
            } else {cambiar}
            ii[cambiar] <- 0
        } # END-else
        cambiar <- NULL # Manejo de reemplazos
    } # END-repeat1
    
    # Reemplazamos nuevamente los índices 0 por NA
    ii[ii==0] <- NA
    return (smm[ii,-2, drop=F]) # Todas sus columnas menos la descripcion
}

# Normalización de los números de anexo

normaNAnx00 <- calculaNAnexo %cmp% as.character
normaNAnx   <- normaNAnx00 %cmp% bareId


# Cálculo del determinante del Anexo
TstTransfDet00 <- function(SubTabla, ...) {
    TstTransfDet(SubTabla[[1]], normaNAnx00(SubTabla[[2]]), ...)
}

LonSign <- `-` %cmp% abs %cmp% forceNum %cmp% bareId

calcLat <- function (SubTabla) {
    # Vienen Grados, Minutos y Segundos
    forceNum(SubTabla[[1]]) + 
        forceNum(SubTabla[[2]],T)/60 + forceNum(SubTabla[[3]],T)/3600
}

calcLon <- function (SubTabla) {
    # Vienen Grados, Minutos y Segundos
    -(abs(forceNum(SubTabla[[1]])) + forceNum(SubTabla[[2]],T)/60 + forceNum(SubTabla[[3]],T)/3600)
}


prioriza <- function (SubTabla, grps, funcs, tst=tstchr, ...) {
    # Por ejemplo, la longitud se puede calcular o estar dada
    #  prioriza (yy[,c("LONGRADC","LONMINC","LONSEGC","LONGITUD")],
    #            c(3,1),
    #            list(calcLon, bareId)
    #  )
    j <- Reduce('+', grps, accumulate = T)
    i <- j-grps+1
    rng <- mapply(':', i, j)
    tablitas <- lapply(rng, function(ii) SubTabla[,ii, drop=F])
    # tRes <- mapply(function(ff,arg) ff(arg), funcs, tablitas, SIMPLIFY = F)
    nn <- nrow(SubTabla)
    ii <- rep(TRUE, nn) # no cuantificados
    res <- NULL
    ng <- length(grps)
    ind <- 0
    repeat {
        ind <- ind+1
        if (ind > ng) return (res)
        res[ii] <- funcs[[ind]](tablitas[[ind]][ii,, drop=F])
        ii <- tst(res, ...) # no cuantificados
        if (sum(ii)==0) return (res)
    }
}

filtraCatalogo <- function (SubTabla, Catalogo, tstfun=tstchr, ..., valOtro="OTRO") {
    nr <- nrow(SubTabla)
    v1 <- v2 <- character(nr) # reslutado
    # jj <- rep(T, nr)
    asignados <- rep(F,nr) # ninguno
    
    for (Testing in SubTabla) {
        ii <- integer(nr)
        jj <- logical(nr)
        #>NOnecesario>> jj[asignados] <- F 
        jj[!asignados] <- !tstfun(Testing[!asignados], ...)
        ii[jj] <- amatch(StandardText(Testing[jj]), 
                     Catalogo[[2]], 
                     method='jw', 
                     p=0.1, 
                     maxDist = 0.2)
        kk <- is.na(ii)
        v2[kk] <- Testing[kk]
        v1[kk] <- valOtro
        kk <- jj & !kk
        v1[kk] <- Catalogo[[1]][ii[kk]]
        asignados <- asignados | jj
        if (sum(asignados) == nr) break
    }
    data.frame(v1, v2, stringsAsFactors = F)
}

filtraUnaCol <- function (SubTabla, 
                          Catalogo, 
                          sep=E_comaYOblancos) {
    # Semejante a la anterior, pero todos los valores están metidos en una sola
    # columna y separados mediante la expresión regular sep
    Ls <- strsplit(SubTabla[[1]], sep) 
    Ls[is.na(Ls)] <- ""

    fun <- function(ee) {
        ss <- StandardText(ee)
        ii <- amatch(ss, Catalogo[[2]], method='jw', p=0.1, maxDist = 0.2)
        jj <- is.na(ii)
        # Conservo solo elementos no NA
        rr <- paste0((Catalogo[[1]][ii])[!jj], collapse=" ")
        kk <- paste0(ss[jj], collapse=" ") # Los elementos que no se encontraron en catalogo
        c(rr,kk) # dos cadenas la encontrados en catalogo y la NO encontrados
    }
    as.data.frame(t(sapply(Ls, fun)), stringsAsFactors = F)
}


MfiltraCatalogo <- function (SubTabla, Catalogo, tstfun=tstchr, ...) {
    # Esta función opera sobre un conjuntito relacionado de
    # columnas
    tst1arg <- function(a) !tstfun(a, ...)
    nr <- nrow(SubTabla)
    v1 <- v2 <- character(nr) # reslutado
    # jj <- rep(T, nr)
    A_Asignar <- apply(
        sapply(SubTabla, tst1arg),
        1,
        function(r) Reduce('|',r)
    )
    
    # funciones:
    ff <- function(Col) amatch(StandardText(Col), Catalogo[[2]], method='jw', p=0.1, maxDist = 0.2)
    asigna <- function(v) ifelse(is.na(v), "", Catalogo[[1]][v])
    asignaOtro <- function(v, r) ifelse(is.na(v), r, "")
    retocaOtro <- function(r, m) ifelse(tstfun(r), m, "OTRO")

    indices <- lapply(SubTabla[A_Asignar,],ff) # Matriz de índices al catálogo
    ss <- lapply(indices, asigna)
    otros <- as.data.frame(mapply(asignaOtro, indices, SubTabla[A_Asignar,]), stringsAsFactors = F)
    ssNew <- as.data.frame(mapply(retocaOtro, otros, ss), stringsAsFactors = F)
    v1[A_Asignar] <- trim(do.call(paste, ssNew))
    v2[A_Asignar] <- trim(do.call(paste, otros))
    
    data.frame(v1, v2, stringsAsFactors = F)
}


calcGastoLPS <- function (SubTabla) {
    tryCatch.W.E(
        SubTabla[[1]] <- as.numeric(SubTabla[[1]])
    )
    tryCatch.W.E(
        SubTabla[[2]] <- as.numeric(SubTabla[[2]])
    )
    ii <- !is.na(SubTabla[[1]])
    v <- numeric(length(ii))
    v[ii] <- SubTabla[[1]][ii]
    jj <- !is.na(SubTabla[[2]])
    kk <- (!ii) & jj
    v[kk] <- SubTabla[[2]][kk]*0.001
    return (v)
}

noEmpty <- function(SubTabla, tstfun=tstchr, ...) {
    ifelse(tstfun(SubTabla[[1]], ...), NA, SubTabla[[1]])
}

testFor <- function(SubTabla, tstStr, resp=c("NO", "SI"), tstfun=tstchr, ...) {
    ss <- bareId(SubTabla)
    jj <- !tstfun(ss, ...) # No vacíos
    rr <- as.character(rep(NA, length(ss)))
    rr[jj] <- resp[1+grepl(tstStr, StandardText(ss[jj]))] # Usa como índice resp. lógica
    return(rr)
}


combinaNumeric <- function(SubTabla, nn=3) {
    forceNum(SubTabla[[1]]) + 
        as.integer(10^nn)*forceNum(SubTabla[[2]])
}

# Obtiene la clave del estado a partir de sus siglas (tipo REPDA)
CveEdoDeSiglas00 <- (function(i) EdosHash[i]) %cmp% ExtraeEdo
CveEdoDeSiglas <-  CveEdoDeSiglas00 %cmp% bareId

# Obtiene la clave de la región administrativa de una clave tipo REPDA
CveRHAdeRPD <- ExtraeRegAdm %cmp% bareId

Id <- function(x) x

convert <- function(SubTabla, factor=1/2.54) {
    factor * forceNum(SubTabla[[1]])
}

mapCatalogo <- function(subTabla, Catalogo, ord=NULL, strSet=NULL) {
    # USO: P.ej. SubTabla contiene valores: (NA o vacíos, "SUPERFICIAL", "SUBTERRANEO")

    arrNoms <- if (is.null(ord)) Catalogo[[1]] else Catalogo[[1]][ord]
    lst <- length(arrNoms)
    # Manejo del conjunto de valores de prueba (strSet), si es NULL, se toma el segundo
    # campo del Catalogo (filtrado por 'ord')
    strSet <- if (is.null(strSet)) {
        if (is.null(ord)) Catalogo[[2]] else Catalogo[[2]][ord]
    } else strSet
    ii <- amatch(StandardText(bareId(subTabla)), 
                 StandardText(strSet), 
                 method='jw', 
                 p=0.1, 
                 maxDist = 0.2)
    return (arrNoms[ii])
}

discierne00 <- function(v, key) {
    rr <- rep(as.numeric(NA), length(v))
    ii <- grepl(key, v)
    rr[ii] <- as.numeric(sub(key %,% ".*", "", v[ii]))
    rr
}

discierne <-function(L, ...) discierne00(bareId(L), ...)

# ====================================

# EJEMPLO DE USO PARA LAS SIGUIENTES FUNCIONES:
# Plantillas Vacias:
#  a0 <- readCsv("EMPTY//A-tblAprvAct.csv")
#  c0 <- readCsv("EMPTY//A-tblCenso.csv")
# 
#  a1 <- readCsv("2011//ENTREGA5A//Censos2011-tblAprvAct.csv")
#  aa <- mjoin(a0,a1)
#  aa <- restaT(aa)
#  writeCsv(aa, "2011//ENTREGA5A//ESTANDAR//Censos2011-tblAprvAct.csv")
#
#  c1 <- readCsv("2011//ENTREGA5A//Censos2011-tblCenso.csv")
#  cc <- mjoin(c0,c1)
#  cc <- restaT(cc)
#  writeCsv(cc, "2011//ENTREGA5A//ESTANDAR//Censos2011-tblCenso.csv")


mjoin <- function(t0, t1) {
    # junta dos tablas, de acuerdo con los encabezados de t0, pero
    # con los tipos de t1
    tipos.t1 <- sapply(t1, class)
    # ii <- match(names(t0), names(t1))
    tipos.t0 <- tipos.t1[names(t0)]
    names(tipos.t0) <- names(t0)
    ii <- !is.na(tipos.t0)
    for (jj in names(t0)[ii]) {
        t0[[jj]] <- get("as." %,% tipos.t0[jj])(t0[[jj]])
    }
    full_join(t0, t1)
}

restaT <- function(t, rr=1) {
    # quita renglones de una tabla; default= 1er. renglón
    t[-rr,]
}

stdjoin <- restaT %cmp% mjoin

estandariza <- function(path) {
    a0 <- readCsv("EMPTY//A-tblAprvAct.csv")
    c0 <- readCsv("EMPTY//A-tblCenso.csv")
    
    ss <- strsplit(path, "//")[[1]]
    ss0 <- ss[-length(ss)]
    bare <- ss[length(ss)]
    adir <- paste0(ss0, collapse="//")
    ndir <-  adir %,% "//ESTANDAR"
    dir.create(ndir)
    
    na1 <- path %,% "-tblAprvAct.csv"
    a1 <- readCsv(na1)
    aa <- stdjoin(a0,a1)
    writeCsv(aa, ndir %,% "//" %,% bare %,% "-tblAprvAct.csv")
    
    nc1 <- path %,% "-tblCenso.csv"
    c1 <- readCsv(nc1)
    cc <- stdjoin(c0,c1)
    # De una vez voy a particionar la tabla 'cc' en lo que es estándar (c0)
    # y lo que no:
    # núm de campos en c0:
    nn <- length(c0)
    nn1 <-length(cc) # El total de columnas
    ccStd <- cc[,1:nn]
    writeCsv(ccStd, ndir %,% "//" %,% bare %,% "-tblCenso.csv")
    # Hay extras?
    if (nn1 != nn) {
        ccExt <- data.frame(idAprovechamiento=cc$idAprovechamiento, cc[,(nn+1):nn1, drop=F])
        writeCsv(ccExt, ndir %,% "//" %,% bare %,% "-EXTRAS.csv")
    }
}

# ===========================================

flatten <- function(s, elim="-") {
    # quita caracteres estorbosos en strings
    gsub(elim, "", s)
}

inmatch <- function(s, tb, elim="-") {
    # match "interior"
    match(flatten(s, elim), flatten(tb, elim))
}


