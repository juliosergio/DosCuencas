# ======================
# JSS: CollectFilesF.R
# 
#     Permite encontrar un conjunto de archivos dispersos por
#     directorios, pero con cierta uniformidad en los nombres
#     y ponerlos su descripción en un vector de strings


if (!exists("LEIDO.MiBiblioteca")) source("MiBiblioteca.R")


CollectFiles <- function(descr="\\.txt",filtroDirs="", path=".") {
    dirs <- list.dirs(path=path)
    ii <- grepl(filtroDirs, dirs) # solo se consideran estos dirs.
    dirs <- dirs[ii]
    list.files(dirs, descr, full.names = T)
}

ExtractDir <- function(ss, filtroDirs) {
    aa <- strsplit(ss, filtroDirs)[[1]]
    aa[1] %,% filtroDirs
}


