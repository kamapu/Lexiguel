\name{stringplot}
\alias{stringplot}
\alias{stringplot.default}
\alias{stringplot.factor}
\alias{stringplot.integer}
\alias{stringplot.data.frame}
\alias{stringplot.formula}

\title{Plot for Distribution of Categorical Variables}
\description{
Alternative plot to display distribution of categorical variables.
The function \code{stringplot} produces a plot of frequencies for classes of
categorical variables and is similar to \dQuote{box-and-whisker plots}
(\code{\link{boxplot}}) but also to \dQuote{histograms} (\code{\link{hist}}).
}
\usage{
stringplot(x, ...)

\method{stringplot}{default}(x, ...)

\method{stringplot}{factor}(x, ...)

\method{stringplot}{integer}(x, lwd=1, maxwidth=1, col="black", at, bty="n",
	xaxt="n", ylim, xlab="", ylab="classes", ...)

\method{stringplot}{data.frame}(x=data.frame(), what, by, at, col, lwd=2,
	maxwidth=1, xlab="", ylab="classes", xlim, ylim, xaxt="s", ...)

\method{stringplot}{formula}(formula, data=data.frame(), ...)
}
\arguments{
    \item{x}{Either a factor, an integer vector or a data frame.}
    \item{data}{Data frame containing the vectors to be plotted.}
    \item{formula}{A formula for plotting.}
    \item{lwd}{Line width for strings (see \code{\link{par}}).}
    \item{maxwidth}{Magnification factor for line width.}
    \item{col}{Colour for strings.}
    \item{at}{Position of string piles in existing plot.}
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
categorical and ordinal data and single string piles should be interpreted as
histograms.
To plot single string piles the function \code{\link{strp}} is called
internally.
Alternatively \code{\link{shadowplot}} or a combination of both can be used.
}
\author{
Miguel Alvarez (\email{malvarez@uni-bonn.de}).
}
\references{
\bold{Beuel S, Alvarez M, Amler E, Behn K, Kotze D, Kreye C, Leemhuis C,
Wagner K, Willy DK, Ziegler S, Becker M (2016).} A rapid assessment of
anthropogenic disturbances in East African wetlands.
\emph{Ecological Indicators} 67: 684--692.
}
\seealso{
\code{\link{strp}}, \code{\link{shadowplot}}.
}
\examples{
library(Lexiguel)
data(WHscores)

## Hydrology scores per land use
stringplot(WET_hydro ~ Landuse, data=WHscores, xlab="Land uses",
	ylab="WET-health scores", main="Hydrology module", lwd=3)

## Adding tendencies
Tendency <- aggregate(WET_hydro ~ Landuse, WHscores, median)
with(Tendency, {
    lines(1:4, WET_hydro, lty=2)
    points(1:4, WET_hydro, pch=21, bg="white", cex=1.5)
})
}
