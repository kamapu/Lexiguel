\name{sloss}
\alias{sloss}

\title{Calculation of SLOSS-Curves and -Index}
\description{
Calculation of SLOSS curves and SLOSS index according to
\bold{Quinn & Harrison (1988)}.
}
\usage{
sloss(table, env=data.frame(), area)
}
\arguments{
\item{table}{A matrix or data frame with the species abundance in units.}
\item{env}{A data frame containing the units' sizes (e.g. as surface).}
\item{area}{Symbol, vector or character value indicating the size of units.}.
}
\details{
This function uses a vegetation matrix with presence or abundance of species in
units (units as rows and species as columns).
If \code{table} is a data frame, it will be coerced to a matrix.

If \code{area} provided as a vector, then \code{env} is not necessary.
This function assumes that the sorting of units in \code{area}
(or in \code{env}) is the same as the sorting in \code{table}. 

This function also calculates the SLOSS-index as the ratio of surfaces below the
SLOSS-curves (small to large divided by large to small), following
\bold{Quinn & Harrison (1988)} and \bold{Vargas et al. (2013)}.
}
\value{
An object of class \code{'SLOSS'}, which is a list with following elements:
\describe{
	\item{SL}{two vectors (\code{area} and \code{species}), showing the
		cumulative increase on species versus cumulative area accounting from
		smallest to largest unit.}
	\item{LS}{two vectors (\code{area} and \code{species}), showing the
		cumulative increase on species versus cumulative area accounting from
		largest to smallest unit.}
	\item{Index}{Numeric value with the calculated SLOSS-index.}
}
}
\author{
Miguel Alvarez (\email{malvarez@uni-bonn.de}).
}
\seealso{
\code{\link{plot.SLOSS}}
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
