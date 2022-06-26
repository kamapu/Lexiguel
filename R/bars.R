#' @name bars
#'
#' @title Plotting Bars
#'
#' @description
#' Plotting function alternative to [barplot()].
#'
#' This function is using [polygon()] to plot bars in an existing
#' plot device. Note that this function is working as a low level plotting
#' function, thus at least an empty plot should be drawn in advance.
#'
#' @param x Numeric vector with the height of bars.
#' @param x0 Numeric vector with the values for the bottom of the bars.
#' @param at Numeric vector with the position of bars.
#' @param maxwidth Width of bars in plot units.
#' @param vertical Logical value indicating whether the bars should be drawn
#'     vertically (`TRUE`, the default) or horizontally (`FALSE`).
#' @param ... Further arguments passed to [polygon()].
#'
#' @author Miguel Alvarez (\email{kamapu78@@gmail.com}).
#'
#' @examples
#' data(iris)
#'
#' ## Mean value of petal length by species
#' iris <- aggregate(Petal.Length ~ Species, iris, mean)
#'
#' ## Plotting steps
#' plot(NA,
#'   xlim = c(0.5, 3.5), ylim = c(0, max(iris$Petal.Length)), xaxt = "n",
#'   xlab = "Species", ylab = "petal length (cm)", type = "n"
#' )
#' axis(side = 1, at = c(1:3), labels = iris$Species)
#' bars(iris$Petal.Length, at = 1, col = "grey")
#'
#' @rdname bars
#'
#' @aliases br
#'
#' @export br
#'
br <- function(x, x0 = 0, at, maxwidth = 0.5, vertical = TRUE, ...) {
  maxwidth <- maxwidth / 2
  if (vertical) {
    x1 <- c(at - maxwidth, at + maxwidth, at + maxwidth, at - maxwidth)
    y1 <- c(x0, x0, x, x)
  }
  if (!vertical) {
    y1 <- c(at - maxwidth, at + maxwidth, at + maxwidth, at - maxwidth)
    x1 <- c(x0, x0, x, x)
  }
  polygon(x1, y1, ...)
}

#' @rdname bars
#'
#' @aliases bars
#'
#' @export
#'
bars <- function(x, ...) UseMethod("bars", x)

#' @rdname bars
#'
#' @aliases bars.default
#'
#' @export
#'
bars.default <- function(x, ...) {
  cat("The class of 'x' does not match any method for 'bars'", "\n")
}

#' @rdname bars
#'
#' @aliases bars.numeric
#'
#' @export
#'
bars.numeric <- function(x, x0 = 0, at, maxwidth = 0.5, vertical = TRUE, ...) {
  if (length(x0) == 1 & length(x) > 1) {
    x0 <- rep(x0, length(x))
  }
  if (length(at) == 1 & length(x) > 1) {
    at <- c(at:(at + length(x) - 1))
  }
  for (i in 1:length(x)) {
    br(x[i], x0[i], at[i], maxwidth, vertical, ...)
  }
}
