#' @name sloss
#' 
#' @title Calculation of SLOSS-Curves and -Index
#' 
#' @description 
#' Calculation of SLOSS curves and SLOSS index according to
#' **Quinn & Harrison (1988)**.
#' 
#' This function uses a vegetation matrix with presence or abundance of species
#' in units (units as rows and species as columns). If `table` is a data
#' frame, it will be coerced to a matrix.
#' 
#' If `area` provided as a vector, then `env` is not necessary.
#' This function assumes that the sorting of units in `area` (or in `env`) is
#' the same as the sorting in `table`.
#' 
#' This function also calculates the SLOSS-index as the ratio of surfaces below
#' the SLOSS-curves (small to large divided by large to small), following
#' **Quinn & Harrison (1988)** and **Vargas et al. (2013)**.
#' 
#' The function `curve_area()` calculates the area below a curve by the
#' trapezoidal rule.
#' 
#' The argument \code{bottom} can be adjusted to the minimum observed values an
#' will be particularly useful when values of \code{y} are negative.
#' 
#' Curves going bellow the \code{bottom} value will get a negative value of
#' area and therefore subtracted when \code{y} cuts the bottom value.
#' 
#' This function was originally written as internal function for the
#' calculations done by \code{\link{sloss}}.
#' 
#' @param table A matrix or data frame with the species abundance in units.
#' @param env A data frame containing the units' sizes (e.g. as surface).
#' @param area Symbol, vector or character value indicating the size of units.
#' @param x A numerical vector with the ordinate values.
#' @param y A numerical vector with the abscissa values.
#' @param bottom A numerical value indicating the bottom for area calculation.
#'     It can be adjusted to the minimum observed values an will be particularly
#'     useful when values of `y` are negative.
#' 
#' @return
#' `sloss()` returns an object of class `SLOSS`, which is a list with following
#' elements:
#' \describe{
#' \item{SL}{two vectors (`area` and `species`), showing the cumulative increase
#'     on species versus cumulative area accounting from smallest to largest
#'     unit.}
#' \item{LS}{two vectors (`area` and `species`), showing the cumulative increase
#'     on species versus cumulative area accounting from largest to smallest
#'     unit.}
#' \item{Index}{Numeric value with the calculated SLOSS-index.}
#' }
#' 
#' 
#' @author Miguel Alvarez (\email{malvarez@@uni-bonn.de}).
#' 
#' @seealso [plot.SLOSS()].
#' 
#' @references
#' **Quinn JF, Harrison SP (1988).**
#' Effects on habitat fragmentation and isolation on species richness: Evidence
#' from biogeographic patterns.
#' *Oecologia* 75: 132-140.
#' 
#' **Vargas RI, Gärtner S, Alvarez M, Hagen E, Reif A (2013).**
#' Does restoration help the conservation of the threatened forest of Robinson
#' Crusoe Island? The impact of forest gap attributes on endemic plant species
#' richness and exotic invasions.
#' *Biodiversity and Conservation* 22: 1283-1300.
#' 
#' @examples
#' ## Load gaps from the Robinson Crusoe Island
#' data(rc_gaps)
#' data(rc_gaps.env)
#' 
#' ## Calculation of curves
#' rc_curves <- sloss(rc_gaps, rc_gaps.env, area)
#' 
#' ## Plot the curves
#' plot(rc_curves, show.legend=TRUE)
#' 
#' ## Calculation of curves
#' rc_curves <- sloss(rc_gaps, rc_gaps.env, area)
#' 
#' ## Area calculated by function
#' rc_curves$Index
#' 
#' ## Cross-check
#' with(rc_curves$SL, curve_area(area, species))/with(rc_curves$LS,
#' 	curve_area(area, species))
#' 
#' @rdname sloss
#' 
#' @export sloss
#' 
sloss <- function(table, env=data.frame(), area) {
	if(!is.matrix(table)) table <- as.matrix(table)
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
	class(SLOSS) <- c("SLOSS","list")
	return(SLOSS)
}

#' @rdname sloss
#' 
#' @aliases curve_area
#' 
#' @export curve_area
#' 
curve_area <- function(x, y, bottom=0) {
    D1 <- c(diff(x))
    D2 <- c(diff(y))
    Area <- sum(D1*((y - bottom)[-length(y)]) + D1*D2/2, na.rm=TRUE)
    return(Area)
}
