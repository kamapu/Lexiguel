#' @name shadowplot
#'
#' @title Shadowplots for categorical variables
#'
#' @description
#' Plot single 'shadow'. This function is called by `shadowplot()` and works
#' only as a low-level plot function.
#'
#' The function `shadow()` calculates the frequency of single values in the
#' vector `x` and displays them as a shadow using the function [polygon()].
#' The resulting shadow indicates by its with the relative frequency of the
#' respective values in the input vector.
#' If not provided, the values of `start` and `end` will be calculated by
#' [min()] and [max()], respectively.
#' This plot function is offered as an alternative to [boxplot()] for
#' categorical and ordinal data and should be interpreted as an histogram.
#'
#' The function `shadowplot()` produces a plot of frequencies for classes of
#' categorical variables and is similar to *box-and-whisker plots* [boxplot()])
#' but also to *histograms* [hist()]).
#'
#' This plot function is offered as an alternative to [boxplot()] for
#' categorical and ordinal data and single shadows should be interpreted as
#' histograms. To plot single shadows the function `shadows()` is called
#' internally.
#' Alternatively [stringplot()] or a combination of both can be used.
#'
#' @param x Either an integer vector or a data frame.
#' @param start Lowest possible value for the variable 'x' as integer.
#' @param end Highest possible value for the variable 'x' as integer.
#' @param at Position of the shadow in the x-axis of the plot.
#' @param maxwidth Magnification factor for the maximum width of shadow.
#' @param col Color of the shadow (passed to [polygon()]).
#' @param border Color of the shadow's border (passed to [polygon()]).
#' @param data Data frame containing the vectors to be plotted.
#' @param formula A formula for plotting.
#' @param xlim,ylim Limits for x and y axes (see [par()]).
#' @param bty Type of box around plotting area (see [par()]).
#' @param xaxt Specification of x axis type (see [par()]).
#' @param xlab Labels for x axis (see [par()]).
#' @param ylab Labels for y axis (see [par()]).
#' @param what Name of the variable to be plotted (in data frame method).
#' @param by Name of the splitting variable (in data frame method).
#' @param ... Further arguments to be passed among methods.
#'
#' @author Miguel Alvarez (\email{malvarez@@uni-bonn.de}).
#'
#' @examples
#' ## Similar example as for strp
#' data(WHscores)
#'
#' ## Shadow overlaid to box and whisker plot using transparent color
#' ## (note the bimodal distribution)
#' boxplot(WHscores$WET_veg, col = "grey")
#' shadows(WHscores$WET_veg,
#'   col = rgb(153 / 255, 255 / 255, 153 / 255, alpha = 0.5),
#'   at = 1
#' )
#'
#' ## Plot using formula
#' shadowplot(WET_veg ~ Landuse, WHscores,
#'   start = 0, end = 10,
#'   col = c("orangered", "palegreen", "orange", "lightblue")
#' )
#'
#' ## Adding a stringplot
#' stringplot(WET_veg ~ Landuse, WHscores, lwd = 3, at = 1)
#'
#' @rdname shadowplot
#'
#' @aliases shadows
#'
#' @export shadows
#'
shadows <- function(x, start, end, at, maxwidth = 1, col = "grey",
                    border = NA, ...) {
  if (!is.integer(x)) {
    stop("'x' have to be of class integer", call. = FALSE)
  }
  x <- summary(as.factor(x))
  x <- x / sum(x)
  if (missing(start)) start <- min(as.numeric(names(x)))
  if (missing(end)) end <- max(as.numeric(names(x)))
  shadows <- data.frame(
    class = c(start:end),
    freq = x[match(start:end, as.numeric(names(x)))],
    row.names = c(start:end)
  )
  shadows$group <- cumsum(as.numeric(is.na(shadows[, 2])))
  shadows <- shadows[!is.na(shadows$freq), ]
  shadows <- split(shadows[, c("class", "freq")], shadows$group)
  ## shadows <- split(shadows, cumsum(as.numeric(is.na(shadows[,2]))))
  for (i in 1:length(shadows)) {
    shadows[[i]] <- shadows[[i]][!is.na(shadows[[i]]$freq), ]
    if (min(shadows[[i]]$class) != start) {
      shadows[[i]] <- do.call(
        rbind,
        list(data.frame(
          class = min(shadows[[i]]$class) - 0.5,
          freq = 0
        ), shadows[[i]])
      )
    }
    if (max(shadows[[i]]$class) != end) {
      shadows[[i]] <- do.call(rbind, list(
        shadows[[i]],
        data.frame(
          class = max(shadows[[i]]$class) + 0.5,
          freq = 0
        )
      ))
    }
  }
  # Now one polygon per element in the list
  for (i in 1:length(shadows)) {
    polygon(
      c(
        shadows[[i]]$freq * maxwidth / 2 + at,
        at - rev(shadows[[i]]$freq) * maxwidth / 2
      ),
      c(shadows[[i]]$class, rev(shadows[[i]]$class)),
      col = col,
      border = border, ...
    )
  }
}

#' @rdname shadowplot
#'
#' @aliases shadowplot
#'
#' @export
#'
shadowplot <- function(x, ...) UseMethod("shadowplot", x)

#' @rdname shadowplot
#'
#' @aliases shadowplot.default
#'
#' @export
#'
shadowplot.default <- function(x, ...) {
  cat("The class of 'x' does not match any method for 'shadowplot'", "\n")
}

#' @rdname shadowplot
#'
#' @aliases shadowplot.numeric
#'
#' @export
#'
shadowplot.numeric <- function(x, start, end, at, maxwidth = 1, col = "grey",
                               border = NA, bty = "n", xaxt = "n", ylim, xlab = "", ylab = "classes", ...) {
  if (!is.integer(x)) x <- as.integer(x)
  if (missing(start)) start <- min(x)
  if (missing(end)) end <- max(x)
  if (!missing(at)) {
    shadows(x, start, end, at, maxwidth, col, border)
  } else {
    if (missing(ylim)) ylim <- c(start, end)
    plot(NA,
      bty = bty, xaxt = xaxt, ylim = ylim, xlab = xlab, ylab = ylab, type = "n",
      ...
    )
    shadows(x, start, end, at = 1, maxwidth, col, border)
  }
}

#' @rdname shadowplot
#'
#' @aliases shadowplot.data.frame
#'
#' @export
#'
shadowplot.data.frame <- function(x = data.frame(), what, by, start, end, at,
                                  maxwidth = 1, col = "grey", border = NA, xlab = "", ylab = "classes", xlim, ylim,
                                  xaxt = "s", ...) {
  what <- substitute(what)
  what <- eval(what, x, parent.frame())
  by <- substitute(by)
  by <- eval(by, x, parent.frame())
  if (!is.factor(by)) by <- as.factor(by)
  what <- split(what, by)
  col <- rep_len(col, length(what))
  border <- rep_len(border, length(what))
  # Format start and end
  if (missing(start)) start <- unlist(lapply(what, min))
  if (missing(end)) end <- unlist(lapply(what, max))
  start <- rep_len(start, length(what))
  end <- rep_len(end, length(what))
  if (missing(at)) {
    at <- 1:length(what)
    if (xaxt == "s") xaxt1 <- "n"
    if (missing(xlim)) xlim <- c(0.5, length(what) + 0.5)
    if (missing(ylim)) ylim <- c(min(start), max(end))
    plot(NA,
      xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, xaxt = xaxt1,
      type = "n"
    )
    if (xaxt == "s") axis(1, at = at, labels = names(what))
  } else {
    at <- at:(at + length(what) - 1)
  }
  for (i in at) {
    shadowplot(
      x = what[[i]], start = start[i], end = end[i], at = at[i],
      maxwidth = maxwidth, col = col[i], border = border[i]
    )
  }
}

#' @rdname shadowplot
#'
#' @aliases shadowplot.formula
#'
#' @export
#'
shadowplot.formula <- function(formula, data = data.frame(), ...) {
  if (missing(formula) || !inherits(formula, "formula")) {
    stop("'formula' missing or incorrect")
  }
  if (length(formula) != 3L) {
    stop("'formula' must have both left and right hand sides")
  }
  what <- as.character(formula)[2]
  what <- eval(substitute(get(what)), data, parent.frame())
  by <- attr(terms(formula), "term.labels")
  y <- list()
  for (i in by) {
    y[[i]] <- eval(substitute(get(i)), data, parent.frame())
    if (!is.factor(y[[i]])) y[[i]] <- as.factor(y[[i]])
  }
  y <- interaction(y)
  shadowplot(data, what, y, ...)
}
