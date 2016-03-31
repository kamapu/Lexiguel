# TODO:   Function calculating SLOSS curves and index
# 
# Author: Miguel Alvarez
################################################################################

# Internal function calculating area below the curve (trapezoids)
curve_area <- function(x, y) {
    D1 <- c(0, diff(x))
    D2 <- c(0, diff(y))
    Area <- sum(D1*y + D1*D2/2)
    return(Area)
}

# SLOSS curves and index
sloss <- function(table, env=data.frame(), area) {
    if(class(table) != "matrix") table <- as.matrix(table)
    area <- substitute(area)
    area <- eval(area, env, parent.frame())
    SLOSS <- list(SL=list(), LS=list())
    # First the calculation from small to large
    SLOSS$SL$area <- c(0, cumsum(area[order(area)]))
    Flor <- apply(table[order(area),], 2, cumsum)
    Flor[Flor > 0] <- 1
    SLOSS$SL$species <- c(0, apply(Flor, 1, sum))
    # Now the calculation from large to small
    SLOSS$LS$area <- c(0, cumsum(area[order(area, decreasing=TRUE)]))
    Flor <- apply(table[order(area, decreasing=TRUE),], 2, cumsum)
    Flor[Flor > 0] <- 1
    SLOSS$LS$species <- c(0, apply(Flor, 1, sum))
    # Calculation of SLOSS index
    SLOSS$Index <- with(SLOSS$SL, curve_area(area,
                    species))/with(SLOSS$LS, curve_area(area, species))
    # Final object
    class(SLOSS)[2] <- "SLOSS"
    return(SLOSS)
}
