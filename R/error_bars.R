#' @name error_bars
#'
#' @title Plotting Dots and Error Bars
#'
#' @description
#' Insert points and error bars indicating tendency and distribution (error).
#'
#' This function is using [points()] to plot the tendency values and
#' [segments()] for the error bars.
#'
#' It is also possible to draw just error bars using the option
#' `show.tendency=FALSE`.
#'
#' @param tendency Numeric vector with tendency values (e.g. mean values).
#' @param upper Numeric vector with values for upper error intervals.
#' @param lower Numeric vector with values for lower error intervals.
#' @param at Numeric vector indicating the position for points.
#' @param show.tendency Logical value indicating whether points should be drawn
#'     or not.
#' @param bar.col Color for bars.
#' @param lwd Width of bars (see \code{\link{par}}).
#' @param lty Line type for bars (see \code{\link{par}}).
#' @param ... Further arguments to be passed to \code{\link{points}}
#'
#' @author Miguel Alvarez (\email{malvarez@@uni-bonn.de}).
#'
#' @examples
#' data(PlantGrowth)
#'
#' ## Calculation of mean and standard deviation
#' Growth <- aggregate(weight ~ group, PlantGrowth, mean)
#' Growth$sd <- aggregate(weight ~ group, PlantGrowth, sd)$weight
#'
#' ## Error bars overlaid to boxplots
#' boxplot(weight ~ group, data = PlantGrowth, border = "grey", main = "Error Bars")
#' with(
#'   Growth,
#'   error_bars(tendency = weight, upper = sd, pch = 21, bg = "red", lwd = 2, cex = 1.5)
#' )
#'
#' @export error_bars
#'
error_bars <- function(tendency, upper, lower, at, show.tendency = TRUE,
                       bar.col = "black", lwd = 1, lty = 1, ...) {
  if (missing(at)) at <- 1:length(tendency)
  if (missing(lower)) lower <- upper
  segments(at, tendency + upper, at, tendency - lower,
    col = bar.col, lwd = lwd,
    lty = lty
  )
  if (show.tendency) points(at, tendency, ...)
}
