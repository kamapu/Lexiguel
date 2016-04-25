\name{shadows}
\alias{shadows}

\title{Low-Level Shadowplot}
\description{
Plot single 'shadow'.
This function is called by \code{\link{shadowplot}} and works only as a
low-level plot function.
}
\usage{
shadows(x, start, end, at, maxwidth=1, col="grey", border=NA, ...)
}
\arguments{
\item{x}{Integer vector.}
\item{start}{Lowest possible value for the variable \code{x} as integer.}
\item{end}{Highest possible value for the variable \code{x} as integer.}
\item{at}{Position of the shadow in the x-axis of the plot.}
\item{maxwidth}{Magnification factor for the maximum width of shadow.}
\item{col}{Color of the shadow (passed to \code{\link{polygon}}).}
\item{border}{Color of the shadow's border (passed to \code{\link{polygon}}).}
\item{...}{Further arguments passed to \code{\link{polygon}}.}
}
\details{
This function calculates the frequency of single values in the vector \code{x}
and displays them as a shadow using the function \code{\link{polygon}}.
The resulting shadow indicates by its with the relative frequency of
the respective values in the input vector.
If not provided, the values of \code{start} and \code{end} will be calculated by
\code{\link{min}} and \code{\link{min}}, respectively.
This plot function is offered as an alternative to \code{\link{boxplot}} for
categorical and ordinal data and should be interpreted as an histogram.
}
\author{
Miguel Alvarez (\email{malvarez@uni-bonn.de}).
}
\seealso{
\code{\link{polygon}}, \code{\link{shadowplot}}, \code{\link{strp}}.
}
\examples{
## Similar example as for strp
library(Lexiguel)
data(WHscores)

## Shadow overlaid to box and whisker plot using transparent color
## (note the bimodal distribution)
boxplot(WHscores$WET_veg, col="grey")
shadows(WHscores$WET_veg, col=rgb(153/255, 255/255, 153/255, alpha=0.5), at=1)
}
