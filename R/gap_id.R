#' @name gap_id
#' 
#' @title Identifying and filling gaps
#' 
#' @description 
#' These functions are able to identify gaps in vectors (contiguous `NA` values)
#' and fill them with custom values.
#' 
#' The function `gap_id()` identify contiguous NA values and assign to them
#' an integer as identifier. The 0 is indicated for values outside of the gaps.
#' 
#' For the function `gap_fill()` only the linear method is yet implemented
#' (linear interpolation between the values neighbouring the gap). The argument
#' `gap` in this function can be the output of `gap_id()`.
#' 
#' @param x A vector where gaps have to be detected.
#' @param gap An integer vector with the identity of gaps (see details).
#' @param method Method used to fill gaps.
#' @return An integer vector with the IDs of gaps for `gap_id()`, while
#'     `gap_fill()` is the vector `x` with NAs replaced by custom values.
#' 
#' @author Miguel Alvarez (\email{malvarez@@uni-bonn.de}).
#'  
#' @examples
#' ## A vector with two gaps
#' A <- c(1:5, NA, NA, 7:10, NA, NA, NA, 16:20)
#' A
#' 
#' ## Identification of gaps
#' B <- gap_id(A)
#' B
#' 
#' ## Linear interpolation in gaps
#' A_filled <- gap_fill(A, B)
#' 
#' par(mfrow=c(2,1))
#' plot(1:length(A), A, type="l")
#' plot(1:length(A_filled), A_filled, type="l")
#' 
#' @rdname gap_id
#' 
#' @export gap_id
#' 
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


#' @rdname gap_id
#' 
#' @aliases gap_fill
#' 
#' @export gap_fill
#' 
gap_fill <- function(x, gap, method="linear") {
    if(method == "linear") x <- linear_interpolation(x, gap)
    return(x)
}

# TODO: Check more elegant way to interpolate in R
# TODO: What about not declared gaps and data frames

#' @keywords internal
#' 
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
