\name{weather}
\alias{weather}
\docType{data}
\title{
	Ten years of daily weather data
}
\description{
	The \code{VFS} offers the capability of importing weather data from the GHCN, either from local files or the online repository, but this import is slow, so the result of the import is saved as an R object for those examples that need it.

	The original GHCN file has many more columns. 
}
\usage{data("weather")}
\format{
  A data frame with 3653 observations on the following 8 variables.
  \describe{
    \item{\code{YEAR}}{a numeric vector}
    \item{\code{MONTH}}{a numeric vector}
    \item{\code{DAY}}{a numeric vector}
    \item{\code{PRCP.VALUE}}{a numeric vector}
    \item{\code{SNOW.VALUE}}{a numeric vector}
    \item{\code{SNWD.VALUE}}{a numeric vector}
    \item{\code{TMAX.VALUE}}{a numeric vector}
    \item{\code{TMIN.VALUE}}{a numeric vector}
  }
}

\source{
\href{https://www.ncdc.noaa.gov/data-access/land-based-station-data/land-based-datasets/global-historical-climatology-network-ghcn}{GHCN} data comprises both current and historical weather station data world-wide.
}

\seealso{
\code{\link{wth.param}}, \code{\link{read.dly}}
}
\examples{

# state college GHCN data
#
# created by:
# weather <- read.dly(system.file("extdata", "USC00368449.dly", package = "VFS"))
data("weather") # same object: 10 years of daily weather data

weather.param <- wth.param(weather, method="markov")

rain.compare <- rainfall(365*3, weather.param)
temp.compare <- temperature(365*3, weather.param)

}
\keyword{datasets}
