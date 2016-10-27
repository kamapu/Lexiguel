\name{curve_area}
\alias{curve_area}

\title{Calculation of area below a curve.}
\description{
This function calculates the area below a curve by the trapezoidal rule.
}
\usage{
curve_area(x, y, bottom=0)
}
\arguments{
	\item{x}{A numerical vector with the ordinate values.}
	\item{y}{A numerical vector with the abscissa values.}
	\item{bottom}{A numerical value indicating the bottom for area calculation.}
}
\details{
The argument \code{bottom} can be adjusted to the minimum observed values an
will be particularly useful when values of \code{y} are negative.

Curves going bellow the \code{bottom} value will get a negative value of area
and therefore subtracted when \code{y} cuts the bottom value.

This function was originally written as internal function for the calculations
done by \code{\link{sloss}}.
}
\value{
A numeric value.
}
\author{
Miguel Alvarez (\email{malvarez@uni-bonn.de}).
}
\examples{
## Load gaps from the Robinson Crusoe Island
library(Lexiguel)
data(rc_gaps)
data(rc_gaps.env)

## Calculation of curves
rc_curves <- sloss(rc_gaps, rc_gaps.env, area)

## Area calculated by function
rc_curves$Index

## Cross-check
with(rc_curves$SL, curve_area(area, species))/with(rc_curves$LS,
	curve_area(area, species))
}
