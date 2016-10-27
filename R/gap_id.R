# TODO:   Functions detecting and filling gaps
# 
# Author: Miguel Alvarez
################################################################################

# Identify gaps
gap_id <- function(x) {
    gap_id <- 0
    gap <- rep(NA, length(x))
    for(i in 1:length(x)) {
        if(!is.na(x[i])) gap[i] <- 0 else {
            if(!is.na(x[i - 1]) || i == 1) {
                gap_id <- gap_id + 1
                gap[i] <- gap_id
            } else gap[i] <- gap[i - 1]
        }
    }
    return(gap)
}

# Filling the gaps
gap_fill <- function(x, gap, method="linear") {
    if(method == "linear") x <- linear_interpolation(x, gap)
    return(x)
}

# Custom functions to fill gaps ------------------------------------------------

# linear
linear_interpolation <- function(x, gap) {
    gap_id <- 0
    repeat {
        gap_id <- gap_id + 1
        if(gap_id %in% gap) {
            gap_temp <- which(gap == gap_id)
            diff_x <- (x[max(gap_temp) + 1] - x[min(gap_temp) - 1])/
                    (length(gap_temp) + 1)
            x[min(gap_temp):max(gap_temp)] <- cumsum(rep(diff_x,
                            length(gap_temp))) + x[min(gap_temp) - 1]
        } else break
    }
    return(x)
}
