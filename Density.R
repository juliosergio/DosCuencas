# ========================
# JSS: Density.R
#      Funciones para el manejo de densidades estadísticas.
# ========================

if (!exists("LEIDO.MiBiblioteca")) source("RR/MiBiblioteca.R", chdir = T)

# La función de densidad es la composición de dos funciones:

# Creador de funciones de densidad:
dfunCreate <- approxfun %cmp% density 
# USO: 
#    dfun <- dfunCreate(X) # donde X es el vector de datos
#      y luego se puede usar esta función
#      p.ej.
#
#    X <- log(rgamma(150,5)) # Creación aleatoria de datos
#    dfun <- dfunCreate(X)
#    dfun(c(0.45, 1.84, 2.3))
#      o para graficarla:
#    plot(dfun, xlim=c(-0.1, 3.0))

