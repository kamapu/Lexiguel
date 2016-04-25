\name{shadowplot}
\alias{shadowplot}
\alias{shadowplot.default}
\alias{shadowplot.factor}
\alias{shadowplot.integer}
\alias{shadowplot.data.frame}
\alias{shadowplot.formula}

\title{Plot for Distribution of Categorical Variables (2)}
\description{
Alternative plot to display distribution of categorical variables.
The function \code{shadowplot} produces a plot of frequencies for classes of
categorical variables and is similar to \dQuote{box-and-whisker plots}
(\code{\link{boxplot}}) but also to \dQuote{histograms} (\code{\link{hist}}).
}
\usage{
shadowplot(x, ...)

\method{shadowplot}{default}(x, ...)

\method{shadowplot}{factor}(x, ...)

\method{shadowplot}{integer}(x, start, end, at, maxwidth=1, col="grey",
	border=NA, bty="n", xaxt="n", ylim, xlab="", ylab="classes", ...)

\method{shadowplot}{data.frame}(x=data.frame(), what, by, start, end, at,
	maxwidth=1, col="grey", border=NA, xlab="", ylab="classes", xlim, ylim,
	xaxt="s", ...)

\method{shadowplot}{formula}(formula, data=data.frame(), ...)
}
\arguments{
    \item{x}{Either a factor, an integer vector or a data frame.}
    \item{start}{Lowest possible value as integer.}
	\item{end}{Highest possible value as integer.}
	\item{at}{Position of the shadow in the x-axis of the plot.}
    \item{data}{Data frame containing the vectors to be plotted.}
    \item{formula}{A formula for plotting.}
    \item{maxwidth}{Magnification factor for line width.}
    \item{col}{Colour for shadows.}
    \item{border}{Colour for border of shadows.}
    \item{xlim, ylim}{Limits for x and y axes (see \code{\link{par}}).}
    \item{bty}{Type of box around plotting area (see \code{\link{par}}).}
    \item{xaxt}{Specification of x axis type (see \code{\link{par}}).}
    \item{xlab}{Labels for x axis (see \code{\link{par}}).}
    \item{ylab}{Labels for y axis (see \code{\link{par}}).}
    \item{what}{Name of the variable to be plotted (in data frame method).}
    \item{by}{Name of the splitting variable (in data frame method).}
    \item{...}{Further arguments to be passed among methods.}
}
\details{
This plot function is offered as an alternative to \code{\link{boxplot}} for
categorical and ordinal data and single shadows should be interpreted as
histograms.
To plot single shadows the function \code{\link{shadows}} is called
internally.
Alternatively \code{\link{stringplot}} or a combination of both can be used.
}
\author{
Miguel Alvarez (\email{malvarez@uni-bonn.de}).
}
\seealso{
\code{\link{shadows}}, \code{\link{stringplot}}.
}
\examples{
## Similar example as stringplot
library(Lexiguel)
data(WHscores)

## The plot
shadowplot(WET_veg ~ Landuse, WHscores, start=0, end=10,
        col=c("orangered","palegreen","orange","lightblue"))

## Adding a stringplot
stringplot(WET_veg ~ Landuse, WHscores, lwd=3, at=1)
}
