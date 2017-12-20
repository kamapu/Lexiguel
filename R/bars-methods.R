# TODO:   Function bars and associated S3 methods
# 
# Author: Miguel Alvarez
################################################################################

# Low level plotting function
br <- function(x, x0=0, at, maxwidth=0.5, vertical=TRUE, ...) {
	maxwidth <- maxwidth/2
	if(vertical) {
		x1 <- c(at - maxwidth, at + maxwidth, at + maxwidth, at - maxwidth)
		y1 <- c(x0, x0, x, x)
	}
	if(!vertical) {
		y1 <- c(at - maxwidth, at + maxwidth, at + maxwidth, at - maxwidth)
		x1 <- c(x0, x0, x, x)
	}
	polygon(x1, y1, ...)
}

# set generic function and default function
bars <- function(x, ...) UseMethod("bars", x)

bars.default <- function(x, ...)
	cat("The class of 'x' does not match any method for 'bars'", "\n")

# method for numeric vectors
bars.numeric <- function(x, x0=0, at, maxwidth=0.5, vertical=TRUE, ...) {
	if(length(x0) == 1 & length(x) > 1)
		x0 <- rep(x0, length(x))
	if(length(at) == 1 & length(x) > 1)
		at <- c(at:(at + length(x) - 1))
	for(i in 1:length(x))
		br(x[i], x0[i], at[i], ...)
}
