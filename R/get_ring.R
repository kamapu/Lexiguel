# TODO:   Get a ring matrix for function raster::focal
# 
# Author: Miguel Alvarez
################################################################################

get_ring <- function(inner, outer) {
    win <- matrix(NA, nrow=outer*2 + 1, ncol=outer*2 + 1)
    r_ind <- matrix(rep(1:(outer*2 + 1), outer*2 + 1), nrow=outer*2 + 1,
            ncol=outer*2 + 1)
    c_ind <- matrix(rep(1:(outer*2 + 1), outer*2 + 1), nrow=outer*2 + 1,
            ncol=outer*2 + 1, byrow=TRUE)
    Dist <- sqrt(((outer + 1) - r_ind)^2 + ((outer + 1) - c_ind)^2)
    win <- Dist <= outer & Dist >= inner
    win <- matrix(as.numeric(win), ncol=ncol(win), nrow=nrow(win))
    return(win)
}
