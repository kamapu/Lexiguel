\name{WHscores-data}
\docType{data}
\alias{WHscores-data}
\alias{WHscores}

\title{WET-health Scores in East African Assessment Units.}
\description{
A data frame containing information of 114 assessment units sampled in 4
countries (localities) and considering 4 land use types (unbalanced sampling).
}
\details{
The data frame contains following variables (columns):
\describe{
	\item{UseunitID}{Character vector with identifier for each assessment unit.}
	\item{Country}{Country of sampling.}
	\item{Locality}{Locality of sampling (one per country).}
	\item{Landuse}{Type of land use.}
	\item{Area}{Surface area of the unit in squared meters.}
	\item{WET_hydro}{WET-health scores for the hydrology module.}
	\item{WET_geo}{WET-health scores for the geomorphology module.}
	\item{WET_veg}{WET-health scores for the vegetation module.}
	\item{WET_wat}{WET-health scores for the water quality module.}
}
}
\author{
Typology Team, \url{https://www.wetlands-africa.de/}
}
\references{
\bold{Kotze DC, Ellery WN, Macfarlane DM, Jewitt GPW (2012).} A rapid assessment
method for coupling anthropogenic stressors and wetland ecological condition.
\emph{Ecological Indicators} 13: 284--293.

\bold{Beuel S, Alvarez M, Amler E, Behn K, Kleißler K, Kotze, DC, Kreye C,
Leemhuis C, Wagner K, Ziegler S, Willy DK (2015).} Analyse von on-site und
off-site Faktoren der hydrologischen Feuchtgebietsdegradation in Ostafrika.
\emph{Forum für Hydrologie und Wasserbewirtschaftung} 35.15: 171--180.

\bold{Beuel S, Alvarez M, Amler E, Behn K, Kotze D, Kreye C, Leemhuis C,
Wagner K, Willy DK, Ziegler S, Becker M (2016).} A rapid assessment of
anthropogenic disturbances in East African wetlands.
\emph{Ecological Indicators} 67: 684--692.
}
\examples{
library(Lexiguel)
data(WHscores)
}
\keyword{data}
