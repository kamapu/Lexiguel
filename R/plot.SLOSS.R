#' @name plot.SLOSS
#' 
#' @title Plotting SLOSS curves
#' 
#' @description 
#' This function is a plotting option for objects of class \code{'SLOSS'},
#' including some alternatives for the layout.
#' 
#' @param x Object of class `SLOSS` (output of [sloss()]).
#' @param y Argument not used in this method.
#' @param sl.lty,sl.lwd,sl.col Arguments passed to [plot()] as `lty` (type of
#'     line), `lwd` (width of line), and `col` (color of the line) for the
#'     *small-to-large curve*.
#' @param ls.lty,ls.lwd,ls.col Arguments passed to [lines()] as `lty` (type of
#'     line), `lwd` (width of line), and `col` (color of the line) for the
#'     *large-to-small curve*.
#' @param show.index A logical value, whether SLOSS-index should be displayed
#'     or not.
#' @param digits.index Argument passed to [round()], indicating the amount of
#'     digits to be displayed in SLOSS-index.
#' @param cex.index Argument passed to [text()], indicating the size of
#'     SLOSS-index in the plot.
#' @param pos.index A numeric vector of size 2, indicating the x an y position
#'     of the index in relation to the respective maxima.
#' @param show.legend A logical value, whether a legend should be displayed or
#'     not.
#' @param pos.legend,bty.legend Arguments passed to [legend()] as `x` (position
#'     of legend as keyword) and `bty` (type of box surrounding the legend),
#'     respectively.
#' @param main Character value with title of the plot.
#' @param ... Further arguments passed to \code{\link{plot}}.
#' 
#' @author Miguel Alvarez (\email{malvarez@@uni-bonn.de}).
#' 
#' @seealso [sloss()]
#' 
#' @references \bold{Quinn JF, Harrison SP (1988).} Effects on habitat
#' fragmentation and isolation on species richness: evidence from biogeographic
#' patterns. \emph{Oecologia} 75: 132--140.
#' 
#' \bold{Vargas RI, Gärtner S, Alvarez M, Hagen E, Reif A (2013).} Does
#' restoration help the conservation of the threatened forest of Robinson
#' Crusoe Island? The impact of forest gap attributes on endemic plant species
#' richness and exotic invasions. \emph{Biodiversity and Conservation} 22:
#' 1283--1300.
#' 
#' @examples
#' ## Load gaps from the Robinson Crusoe Island
#' library(Lexiguel)
#' data(rc_gaps)
#' data(rc_gaps.env)
#' 
#' ## Calculation of curves
#' rc_curves <- sloss(rc_gaps, rc_gaps.env, area)
#' 
#' ## Plot the curves
#' plot(rc_curves, show.legend=TRUE)
#' 
#' @export
#' 
plot.SLOSS <- function(x, y=NULL, sl.lty=2, sl.lwd=1, sl.col="black", ls.lty=1,
        ls.lwd=1, ls.col="black", show.index=TRUE, digits.index=2, cex.index=1,
        pos.index=c(0.05,0.95), show.legend=FALSE, pos.legend="bottomright",
        bty.legend="o", main="SLOSS curves",...) {
    with(x$SL, plot(area, species, type="l", lty=sl.lty, lwd=sl.lwd, col=sl.col,
                    main=main, ...))
    with(x$LS, lines(area, species, lty=ls.lty, lwd=ls.lwd, col=ls.col))
    if(show.legend) {
        legend(pos.legend, lty=c(sl.lty,ls.lty), lwd=c(sl.lwd,ls.lwd),
                legend=c("small to large","large to small"), bty=bty.legend)
    }
    if(show.index) {
        with(x$SL, text(max(area)*pos.index[1], max(species)*pos.index[2],
                        labels=paste("SLOSS-index =",
                                round(x$Index, digits.index)),
                        cex=cex.index, pos=4))
    }
}
