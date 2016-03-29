\name{error_bars}
\alias{error_bars}

\title{Plotting Dots and Error Bars}
\description{
Insert points and error bars indicating tendency and distribution (error).
}
\usage{
error_bars(tendency, upper, lower, at, show.tendency=TRUE, bar.col="black",
    lwd=1, lty=1, ...)
}
\arguments{
	\item{tendency}{Numeric vector with tendency values (e.g. mean values).}
	\item{upper}{Numeric vector with values for upper error intervals.}
	\item{lower}{Numeric vector with values for lower error intervals.}
	\item{at}{Numeric vector indicating the position for points.}
	\item{show.tendency}{Logical value indicating whether points should be drawn
	    or not.}
	\item{bar.col}{Color for bars.}
	\item{lwd}{Width of bars (see \code{\link{par}}).}
	\item{lty}{Line type for bars (see \code{\link{par}}).}
	\item{...}{Further arguments to be passed to \code{\link{points}}}
}
\details{
This function is using \code{\link{points}} to plot the tendency values and
\code{\link{segments}} for the error bars.

It is also possible to draw just error bars using the option
\code{show.tendency=FALSE}.
}
\examples{
library(Lexiguel)
data(PlantGrowth)

## Calculation of mean and standard deviation
Growth <- aggregate(weight ~ group, PlantGrowth, mean)
Growth$sd <- aggregate(weight ~ group, PlantGrowth, sd)$weight

## Error bars overlaid to boxplots
boxplot(weight ~ group, data=PlantGrowth, border="grey", main="Error Bars")
with(Growth,
	error_bars(tendency=weight, upper=sd, pch=21, bg="red", lwd=2, cex=1.5))
}
