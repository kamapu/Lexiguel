\name{plot.SLOSS}
\alias{plot}
\alias{plot.SLOSS}

\title{Plotting SLOSS curves}
\description{
This function is a plotting option for objects of class \code{'SLOSS'},
including some alternatives for the layout.
}
\usage{
\method{plot}{SLOSS}(x, y=NULL, sl.lty=2, sl.lwd=1, sl.col="black", ls.lty=1,
	ls.lwd=1, ls.col="black", show.index=TRUE, digits.index=2, cex.index=1,
	pos.index=c(0.05,0.95), show.legend=FALSE, pos.legend="bottomright",
	bty.legend="o", main="SLOSS curves",...)
}
\arguments{
\item{x}{Object of class \code{SLOSS} (output of \code{\link{sloss}}).}
\item{y}{Argument not used in this method}.
\item{sl.lty,sl.lwd,sl.col}{Arguments passed to \code{\link{plot}} as
	\code{lty} (type of line), \code{lwd} (width of line), and \code{col}
	(color of the line) for the \sQuote{small-to-large curve}.}
\item{ls.lty,ls.lwd,ls.col}{Arguments passed to \code{\link{lines}} as
	\code{lty} (type of line), \code{lwd} (width of line), and \code{col}
	(color of the line) for the \sQuote{large-to-small curve}.}
\item{show.index}{A logical value, whether SLOSS-index should be displayed or
	not.}
\item{digits.index}{Argument passed to \code{\link{round}}, indicating the
	amount of digits to be displayed in SLOSS-index.}
\item{cex.index}{Argument passed to \code{\link{text}}, indicating the size of
	SLOSS-index in the plot.}
\item{pos.index}{A numeric vector of size 2, indicating the x an y position of
	the index in relation to the respective maxima.}
\item{show.legend}{A logical value, whether a legend should be displayed or
	not.}
\item{pos.legend,bty.legend}{Arguments passed to \code{\link{legend}} as
	\code{x} (possition of legend as keyword) and \code{bty} (type of box
	surrounding the legend), respectively.}
\item{main}{Character value with title of the plot.}
\item{...}{Further arguments passed to \code{\link{plot}}.}
}
\author{
Miguel Alvarez, \email{malvarez@uni-bonn.de}
}
\seealso{
\code{\link{sloss}}
}
\references{
\bold{Quinn JF, Harrison SP (1988).} Effects on habitat fragmentation and
isolation on species richness: evidence from biogeographic patterns.
\emph{Oecologia} 75: 132--140.

\bold{Vargas RI, Gärtner S, Alvarez M, Hagen E, Reif A (2013).} Does restoration
help the conservation of the threatened forest of Robinson Crusoe Island? The
impact of forest gap attributes on endemic plant species richness and exotic
invasions. \emph{Biodiversity and Conservation} 22: 1283--1300.
}
\examples{
## Load gaps from the Robinson Crusoe Island
library(Lexiguel)
data(rc_gaps)
data(rc_gaps.env)

## Calculation of curves
rc_curves <- sloss(rc_gaps, rc_gaps.env, area)

## Plot the curves
plot(rc_curves, show.legend=TRUE)
}
