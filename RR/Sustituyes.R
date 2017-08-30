# ==================
# JSS: Sustituyes.R
#      Ejecuta substituciones múltiples
#      en un vector de caracteres
trim <- function (x) gsub("^\\s+|\\s+$", "", x) # quita espacios


Sustituyes <- function (originales, reemplazos, Vchar, fixed=T) {
    # uso: 
    #    miarrch <- Sustituyes(c("uno", "dos", "tres"), c("1","2","3"), miarrch)
    
    # función recursiva
    rSus <- function(originales, reemplazos) {
        if (length(originales) == 0) return (Vchar) # Salida
        Vchar <<- gsub(originales[1],reemplazos[1], Vchar, fixed = fixed)
        rSus(originales[-1], reemplazos[-1])
    }
    # Llamado:
    rSus(originales, reemplazos)
}

StandardText <- function(vv) {
    # Se convierte a mayúsculas 
    # y se quitan acentos y comillas
    trim(
        Sustituyes(
            c('"',"Á","É","Í","Ó","Ú", "Ñ", "Ã‘", 'Ã“', "Ã‰"), 
            c("","A","E","I","O","U", "NI", "NI", "O",   "E"),
            toupper(vv)
        )
    )
}

cambiaNLs <- function(s) {
    # Cambia la secuencia "\n" por 
    # <NEWLINE> en la entrada
    gsub("\\\\n", "\n", s)
}
