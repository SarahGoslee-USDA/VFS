\name{USC00368449.dly}
\alias{USC00368449.dly}
\docType{data}
\title{
	GHCN Data for State College, PA, 1980-2009
}
\description{
	A ten-year sample of \href{https://www.ncdc.noaa.gov/data-access/land-based-station-data/land-based-datasets/global-historical-climatology-network-ghcn}{Global Historical Climatology Network (GHCN)} daily weather data for State College, PA (station USC00368449). This is in a complex fixed-width format. Please see the \href{https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt}{GHCN readme} for details. 
}
\usage{"USC00368449.dly"}
\format{
  A complex fixed-width ASCII file.
}

\source{
	\href{https://www.ncdc.noaa.gov/data-access/land-based-station-data/land-based-datasets/global-historical-climatology-network-ghcn}{GHCN} data comprises both current and historical weather station data world-wide. 

}
\seealso{
\code{\link{read.dly}}
}

\examples{
# A sample DLY file for State College, PA, is included with this package.
# This file contains a subset of data (1980-2009) for station USC00368449
#
weather <- read.dly(system.file("extdata", "USC00368449.dly", package = "VFS"))
# identical to data("weather")
}
\keyword{datasets}
