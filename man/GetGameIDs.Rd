% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GetGameIDs.R
\name{GetGameIDs}
\alias{GetGameIDs}
\title{Get Game IDs.}
\usage{
GetGameIDs(year = CurrentYear(), season.type = "Regular Season", date)
}
\arguments{
\item{year}{Season (e.g. 2008 for the 2007-08 season)}

\item{season.type}{Either "Regular Season", "Playoffs", or "Both"}

\item{date}{If provided, target date for game IDs}
}
\value{
data.frame
}
\description{
Get Game IDs.
}
\examples{
GetGameIDs(2014, 'Regular Season')
}
\keyword{Game}

