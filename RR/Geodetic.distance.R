# Geodetic.distance.R


geodetic.distance <- function(point1, point2) { 
    # Points are (Longitude, Latitude)
    R <- 6371 # Kilometers
    p1rad <- point1 * pi/180 
    p2rad <- point2 * pi/180 
    dd <- sin(p1rad[2])*sin(p2rad[2])+cos(p1rad[2])*cos(p2rad[2])*cos(abs(p1rad[1]-p2rad[1]))
    dd <- if (dd > 1) trunc(dd) else dd
    ss <- acos(dd) 
    R*ss
} 