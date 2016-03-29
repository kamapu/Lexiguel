\name{strp}
\alias{strp}

\title{Low-Level Stringplot}
\description{
Plot single stingplot pile.
This function is called by \code{\link{stringplot}} and works only as a
low-level plot function.
}
\usage{
strp(x, lwd=1, maxwidth=1, col="black", at)
}
\arguments{
	\item{x}{Integer vector.}
	\item{lwd}{Line width for strings (passed to \code{\link{segments}}).}
	\item{maxwidth}{Magnification factor for the maximum width of pile.}
	\item{col}{Colours for strings.}
	\item{at}{Position of the string pile in the x-axis of the plot.}
}
\details{
This function calculates the frequency of single values in the vector \code{x}
and displays them in a string pile by using the function \code{\link{segments}}.
The resulting pile indicates by the with of strings the relative frequency of
the respective values in the input vector.
This plot function is offered as an alternative to \code{\link{boxplot}} for
categorical and ordinal data and should be interpreted as an histogram.
}
\author{
Miguel Alvarez, \email{malvarez@uni-bonn.de}
}
\seealso{
\code{\link{stringplot}}, \code{\link{error_bars}}.
}
\examples{
## Similar example as for error_bars
library(Lexiguel)
data(WHscores)

## Strings overlaid to box and whisker plot (note the bimodal distribution)
boxplot(WHscores$WET_veg, border="grey", col="palegreen")
strp(WHscores$WET_veg, lwd=4, at=1)
}
