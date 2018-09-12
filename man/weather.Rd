\name{weather}
\alias{weather}
\docType{data}
\title{
	Imported weather data.
}
\description{
	The \code{VFS} package includes thirty years GHCN weather data for State College, PA, to demonstrate the import process, but this import is slow, so the result of the import is saved as an R object for those examples that need it.
}
\usage{data("weather")}
\format{
	Returns a data frame with date as three columns, YEAR, MONTH, DAY, and each data value present in the original file along with its quality flags. Please see the \href{https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt}{GHCN readme} for details.

}


\source{
\href{https://www.ncdc.noaa.gov/data-access/land-based-station-data/land-based-datasets/global-historical-climatology-network-ghcn}{GHCN} data comprises both current and historical weather station data world-wide.
}

\examples{
	
	# state college GHCN data
	#
	# import is slow: 
	# weather <- read.dly(system.file("extdata", "USC00368449.dly", package = "VFS"))
	data("weather") # same object

	weather.param <- wth.param(weather)

	rain.compare <- rainfall(365*3, weather.param)
	temp.compare <- temperature(365*3, weather.param)


}
\keyword{datasets}
