# Combina dos listas intercalando sus elementos
LEIDO.Intercala <- TRUE

intercala <- function(L1, L2) {
    if (length(L1)==0)
        return(L2)
    if (length(L2)==0)
        return(L1)
    c(list(L1[[1]],L2[[1]]), intercala(tail(L1,-1),tail(L2,-1)))
}

# La misma operación con Data Frames

intercalaDF <- function(D1, D2) {
    if (length(D1)==0)
        return(D2)
    if (length(D2)==0)
        return(D1)
    data.frame(D1[,1, drop=F],D2[,1, drop=F], 
      intercalaDF(D1[,-1, drop=F],D2[,-1, drop=F]))
}
   
