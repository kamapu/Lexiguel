\name{fill_nas}
\alias{fill_nas}

\title{Replacing missing values from a reference table.}
\description{
Partial filling of tables, when only part of the data is missing.
}
\usage{
fill_nas(x, y, x_id, x_var, y_id, y_var)
}
\arguments{
\item{x}{Data frame with the variable to be completed.}
\item{y}{Data frame providing values for those missing in \code{'x'}.}
\item{x_id,y_id}{Character values indicating the variable to be used as
	identifier.}
\item{x_var,y_var}{Character values indicating the name of the variable to be
	completed.}
}
\details{
Description to be completed.
}

\value{
Data frame \code{'x'} with completed values.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
## No example at the moment.
}
