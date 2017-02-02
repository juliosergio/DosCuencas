# ======================
# JSS: AA_Colecta.R
# 
#     Permite concatenar un conjunto de archivos dispersos por
#     directorios, pero con cierta uniformidad en los nombres
#     y ponerlos en una sola tabla continua, que es guardada
#     en un archivo de objetos de R.


if (!exists("LEIDO.AA_Biblioteca")) source("AA_Biblioteca.R")

# source("AA_ComparaREPDA_F.R")
# source("AA_ResuEstados_F.R")

dirs <- list.dirs(full=F) # , recur=F)

repeat {
    filtro <- readline("Filtro directorios(vacio==Default):>")
    
    filtro <- if(filtro=="") "ENTREGA[1-4]A?$" else filtro
    
    ii <- grepl(filtro, dirs)
    cdir <- dirs[ii]
    
    archs <-readline("Expr.Reg archivos (sin .csv):>")
    
    files <- list.files(cdir, archs, full.names = T)
    print(files)
    if (readline("Est?n correctos sus filtros [S/N]:>") %in% c("S","s")) break
}

strippedFiles <- sub(archs %+% ".csv", "", files, fixed = T)


ntbl <- readline("Nombre de su tabla:>")

assign(ntbl, NULL)
for (ff in files) {
    assign(
        ntbl,
        rbind(
            get(ntbl),
            readCsv(ff)
        )
    )
}

adir <- readline("Donde escribo su tabla [vac?o==Default]:>")
adir <- if(adir=="") "." else adir

save(list=ntbl, file= adir %+% "//" %+% ntbl %+% ".RData")



