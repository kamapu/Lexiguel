#' @name lim_bars
#'
#' @title Limits bar plot
#'
#' @description
#' Graphic of bars showing the range (minimum and maximum values) of a variable
#' 'y' along segments of a second 'x' variable.
#'
#' @param x Either a numeric vector or a matrix with two columns.
#' @param y A numeric vector. If missing the second column of 'x' will be
#'     used instead.
#' @param breaks An integer value indicating the number of segments used to
#'     split 'x'.
#' @param pos A function used to determine the position of the segment. For
#'     instance 'mean' or 'median' can be used. By default no position will be
#'     displayed.
#' @param horiz A logical value indicating whether bars should be drawn
#'     horizontally or vertically.
#' @param poly_args A list including arguments to be passed to [polygon()]. This
#'     will affect the layout of the bars.
#' @param pos_args A list including arguments to be passed to [segments()]. This
#'     will affect the layout of the position lines.
#' @param na.rm A logical value indicating whether NA values in 'x' and 'y'
#'     should removed or not.
#' @param ... Further arguments passed to [plot()].
#'
#' @export lim_bars
#'
lim_bars <- function(x, y, breaks = 10, pos = NULL, horiz = FALSE,
                     poly_args = list(), pos_args = list(), na.rm = FALSE, ...) {
  if (missing(y)) {
    y <- x[, 2]
    x <- x[, 1]
  }
  if (na.rm) {
    x <- x[!is.na(x) & !is.na(y)]
    y <- y[!is.na(x) & !is.na(y)]
  }
  breaks <- seq(from = min(x), to = max(x), length.out = breaks + 1)
  x_class <- cut(x, breaks, labels = FALSE, include.lowest = TRUE)
  if (!is.null(pos)) {
    trend <- sapply(split(y, x_class), pos)
  }
  if (horiz) {
    plot(x = y, y = x, type = "n", ...)
    for (i in 1:(length(breaks) - 1)) {
      do.call(polygon, c(
        list(
          y = c(
            rep(breaks[i], 2),
            rep(breaks[i + 1], 2)
          ),
          x = c(
            min(y[x_class == i]),
            rep(max(y[x_class == i]), 2),
            min(y[x_class == i])
          )
        ),
        poly_args
      ))
    }
    if (!is.null(pos)) {
      do.call(segments, c(list(
        y0 = breaks[-length(breaks)],
        x0 = trend, y1 = breaks[-1]
      ), pos_args))
    }
  } else {
    plot(x = x, y = y, type = "n", ...)
    for (i in 1:(length(breaks) - 1)) {
      do.call(polygon, c(
        list(
          x = c(
            rep(breaks[i], 2),
            rep(breaks[i + 1], 2)
          ),
          y = c(
            min(y[x_class == i]),
            rep(max(y[x_class == i]), 2),
            min(y[x_class == i])
          )
        ),
        poly_args
      ))
    }
    if (!is.null(pos)) {
      do.call(segments, c(list(
        x0 = breaks[-length(breaks)],
        y0 = trend, x1 = breaks[-1]
      ), pos_args))
    }
  }
}
