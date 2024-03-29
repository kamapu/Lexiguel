% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sloss.R
\name{sloss}
\alias{sloss}
\alias{curve_area}
\title{Calculation of SLOSS-Curves and -Index}
\usage{
sloss(table, env = data.frame(), area)

curve_area(x, y, bottom = 0)
}
\arguments{
\item{table}{A matrix or data frame with the species abundance in units.}

\item{env}{A data frame containing the units' sizes (e.g. as surface).}

\item{area}{Symbol, vector or character value indicating the size of units.}

\item{x}{A numerical vector with the ordinate values.}

\item{y}{A numerical vector with the abscissa values.}

\item{bottom}{A numerical value indicating the bottom for area calculation.
It can be adjusted to the minimum observed values an will be particularly
useful when values of \code{y} are negative.}
}
\value{
\code{sloss()} returns an object of class \code{SLOSS}, which is a list with following
elements:
\describe{
\item{SL}{two vectors (\code{area} and \code{species}), showing the cumulative increase
on species versus cumulative area accounting from smallest to largest
unit.}
\item{LS}{two vectors (\code{area} and \code{species}), showing the cumulative increase
on species versus cumulative area accounting from largest to smallest
unit.}
\item{Index}{Numeric value with the calculated SLOSS-index.}
}
}
\description{
Calculation of SLOSS curves and SLOSS index according to
\strong{Quinn & Harrison (1988)}.

This function uses a vegetation matrix with presence or abundance of species
in units (units as rows and species as columns). If \code{table} is a data
frame, it will be coerced to a matrix.

If \code{area} provided as a vector, then \code{env} is not necessary.
This function assumes that the sorting of units in \code{area} (or in \code{env}) is
the same as the sorting in \code{table}.

This function also calculates the SLOSS-index as the ratio of surfaces below
the SLOSS-curves (small to large divided by large to small), following
\strong{Quinn & Harrison (1988)} and \strong{Vargas et al. (2013)}.

The function \code{curve_area()} calculates the area below a curve by the
trapezoidal rule.

The argument \code{bottom} can be adjusted to the minimum observed values an
will be particularly useful when values of \code{y} are negative.

Curves going bellow the \code{bottom} value will get a negative value of
area and therefore subtracted when \code{y} cuts the bottom value.

This function was originally written as internal function for the
calculations done by \code{\link{sloss}}.
}
\examples{
## Load gaps from the Robinson Crusoe Island
data(rc_gaps)
data(rc_gaps.env)

## Calculation of curves
rc_curves <- sloss(rc_gaps, rc_gaps.env, area)

## Plot the curves
plot(rc_curves, show.legend = TRUE)

## Calculation of curves
rc_curves <- sloss(rc_gaps, rc_gaps.env, area)

## Area calculated by function
rc_curves$Index

## Cross-check
with(rc_curves$SL, curve_area(area, species)) / with(
  rc_curves$LS,
  curve_area(area, species)
)

}
\references{
\strong{Quinn JF, Harrison SP (1988).}
Effects on habitat fragmentation and isolation on species richness: Evidence
from biogeographic patterns.
\emph{Oecologia} 75: 132-140.

\strong{Vargas RI, Gärtner S, Alvarez M, Hagen E, Reif A (2013).}
Does restoration help the conservation of the threatened forest of Robinson
Crusoe Island? The impact of forest gap attributes on endemic plant species
richness and exotic invasions.
\emph{Biodiversity and Conservation} 22: 1283-1300.
}
\seealso{
\code{\link[=plot.SLOSS]{plot.SLOSS()}}.
}
\author{
Miguel Alvarez (\email{malvarez@uni-bonn.de}).
}
