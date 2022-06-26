#' @name stringplot
#'
#' @title Plot for Distribution of Categorical Variables
#'
#' @description
#' Alternative plot to display distribution of categorical variables.
#' The function `stringplot()` produces a plot of frequencies for classes of
#' categorical variables and is similar to *box-and-whisker plots* but
#' it may be interpreted as histograms.
#'
#' This plot function is offered as an alternative to [boxplot()] for
#' categorical (ordinal) variables and single string piles should be interpreted
#' as histograms.
#' To plot single string piles the function [strp()] is called internally.
#' Alternatively [shadowplot()] or a combination of both can be used.
#'
#' The function `strp()` calculates the frequency of single values in the vector
#' `x` and displays them in a string pile by using the function [segments()].
#' The resulting pile indicates by the with of strings the relative frequency of
#' the respective values in the input vector.
#' This plot function is offered as an alternative to [boxplot()] for
#' categorical and ordinal data and should be interpreted as an histogram.
#'
#' @param x Either an integer vector or a data frame.
#' @param lwd Line width for strings (passed to [segments()]).
#' @param data Data frame containing the vectors to be plotted.
#' @param formula A formula for plotting.
#' @param lwd Line width for strings (see [par()]).
#' @param maxwidth Magnification factor for line width.
#' @param col Colour for strings.
#' @param at Position of string piles in existing plot.
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
#' @seealso \code{\link{strp}}, \code{\link{shadowplot}}.
#'
#' @references
#' **Beuel S, Alvarez M, Amler E, Behn K, Kotze D, Kreye C, Leemhuis C,
#' Wagner K, Willy DK, Ziegler S, Becker M (2016).**
#' A rapid assessment of anthropogenic disturbances in East African wetlands.
#' *Ecological Indicators* 67: 684-692.
#'
#' @examples
#' data(WHscores)
#'
#' ## Hydrology scores per land use
#' stringplot(WET_hydro ~ Landuse,
#'   data = WHscores, xlab = "Land uses",
#'   ylab = "WET-health scores", main = "Hydrology module", lwd = 3
#' )
#'
#' ## Adding tendencies
#' Tendency <- aggregate(WET_hydro ~ Landuse, WHscores, median)
#' with(Tendency, {
#'   lines(1:4, WET_hydro, lty = 2)
#'   points(1:4, WET_hydro, pch = 21, bg = "white", cex = 1.5)
#' })
#'
#' #' ## Strings overlaid to box and whisker plot (note the bimodal distribution)
#' boxplot(WHscores$WET_veg, border = "grey", col = "palegreen")
#' strp(WHscores$WET_veg, lwd = 4, at = 1)
#'
#' @author Miguel Alvarez (\email{malvarez@@uni-bonn.de}).
#'
#' @seealso [stringplot()], [error_bars()].
#'
#' @rdname stringplot
#'
#' @aliases strp
#'
#' @export strp
#'
strp <- function(x, lwd = 1, maxwidth = 1, col = "black", at) {
  if (!(is.integer(x))) {
    stop("'x' have to be of class integer", call. = FALSE)
  }
  x <- summary(as.factor(x))
  y <- as.integer(names(x))
  x <- x / sum(x)
  x0 <- at - x * maxwidth / 2
  x1 <- at + x * maxwidth / 2
  # plotting
  segments(x0 = x0, y0 = y, x1 = x1, y1 = y, col = col, lwd = lwd)
}

#' @rdname stringplot
#'
#' @export
#'
stringplot <- function(x, ...) UseMethod("stringplot", x)

#' @rdname stringplot
#'
#' @aliases stringplot.default
#'
#' @export
#'
stringplot.default <- function(x, ...) {
  cat("The class of 'x' does not match any method for 'stringplot'", "\n")
}

#' @rdname stringplot
#'
#' @aliases stringplot.numeric
#'
#' @export
#'
stringplot.numeric <- function(x, lwd = 1, maxwidth = 1, col = "black", at, bty = "n",
                               xaxt = "n", ylim, xlab = "", ylab = "classes", ...) {
  if (!is.integer(x)) x <- as.integer(x)
  if (!missing(at)) {
    strp(x, lwd, maxwidth, col, at)
  } else {
    at <- 1
    if (missing(ylim)) ylim <- range(as.integer(x))
    plot(NA,
      bty = bty, xaxt = xaxt, ylim = ylim, xlab = xlab, ylab = ylab, type = "n",
      ...
    )
    strp(x, lwd, maxwidth, col, at)
  }
}

#' @rdname stringplot
#'
#' @aliases stringplot.data.frame
#'
#' @export
#'
stringplot.data.frame <- function(x = data.frame(), what, by, at, col, lwd = 2,
                                  maxwidth = 1, xlab = "", ylab = "classes", xlim, ylim, xaxt = "s", ...) {
  what <- substitute(what)
  what <- eval(what, x, parent.frame())
  if (missing(ylim)) ylim <- range(as.integer(what))
  by <- substitute(by)
  by <- eval(by, x, parent.frame())
  if (!is.factor(by)) by <- as.factor(by)
  what <- split(what, by)
  if (missing(col)) col <- "black"
  col <- rep_len(col, length(what))
  if (missing(at)) {
    at <- 1:length(what)
    if (xaxt == "s") xaxt1 <- "n"
    if (missing(xlim)) xlim <- c(0.5, length(what) + 0.5)
    plot(NA,
      xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, xaxt = xaxt1,
      type = "n", ...
    )
    if (xaxt == "s") axis(1, at = at, labels = names(what))
  } else {
    at <- at:(at + length(what) - 1)
  }
  for (i in at) {
    stringplot(
      x = what[[i]], at = at[i], col = col[i], lwd = lwd,
      maxwidth = maxwidth
    )
  }
}

#' @rdname stringplot
#'
#' @aliases stringplot.formula
#'
#' @export
#'
stringplot.formula <- function(formula, data = data.frame(), ...) {
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
  stringplot(data, what, y, ...)
}
