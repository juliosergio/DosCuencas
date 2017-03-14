#====================================
# ClimaMensual.R
#
#   Calcula climatología mensual (mes)
#====================================
source("../RR/MiBiblioteca.R", chdir = T)
# library(dplyr)
# Directorio base
bdir <- "CUENCAS"

cuencas <- list.files(bdir)

for (cc in cuencas) {
    # Directorio c/cuenca:
    cdir <- paste0(bdir, "/", cc)
    # Los archivos en cada cuenca que terminan en ".._Rmens.txt":
    files <- system2("ls", paste0(cdir, "/*_Rmens.txt"), stdout=T)
    for (ff in files) {
        # El nombre del archivo sin el apéndice ".._Rmens.txt"
        bare <- strsplit(ff, "_Rmens." , fixed=T)[[1]][1] # Sin "_Rmens.txt"
        # Se lee el archivo como una tabla de "dplyr"
        tt <- tbl_df(read.table(ff, header=T)) 
        # La operación de resumen, agrupada por (mes)
        tt <- tt %>% 
            group_by(mes) %>%
            summarise(mApp=mean(ppAcc), 
                      sdApp=sd(ppAcc),
                      mmTmax=mean(mTmax),
                      sdTmax=sd(mTmax),
                      mmTmin=mean(mTmin),
                      sdTmin=sd(mTmin))  
        # Nuevo nombre del archivo:
        postfijo <- "_ClimaMens.txt"
        newname <- paste0(bare, postfijo)
        # reescribimos la tabla, en el mismo directorio, con el nuevo nombre:
        write.table(tt, newname, row.names=F)
    }
}
