\name{read.dly}
\alias{read.dly}
\title{
	Read GHCN DLY daily weather file into a data frame
}
\description{
	Imports daily data files from the \href{https://www.ncdc.noaa.gov/data-access/land-based-station-data/land-based-datasets/global-historical-climatology-network-ghcn}{Global Historical Climatology Network (GHCN)}, replaces nodata values with NA, and converts precipitation to mm and temperature to C.
}
\usage{
read.dly(filename)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
	Name and path (if necessary) of a GHCN DLY file.
}
}
\details{
All GHCN DLY files should have these five elements: PRCP (precipitation, originally tenths of a mm but mm in the function output); SNOW (snowfall, mm); SNWD (snow depth, mm), TMAX (maximum temperature, originally tenths of degree C but C in the function output), and  TMIN (minimum temperature, originally tenths of degree C but C in the function output). 

Depending on the station, there may be many other recorded variables. Each variable is accompanied by a series of quality flags, which are preserved from the original file.

}
\value{
	Returns a data frame with date as three columns, YEAR, MONTH, DAY, and each data value present in the original file along with its quality flags. Please see the \href{https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt}{GHCN readme} for details.
}
\references{
	\href{https://www.ncdc.noaa.gov/data-access/land-based-station-data/land-based-datasets/global-historical-climatology-network-ghcn}{GHCN} data comprises both current and historical weather station data world-wide. 
}
\author{
	Sarah Goslee
}

\seealso{
\code{\link{wth.param}}
}
\examples{
	# A sample DLY file for State College, PA, is included with this package.
	# This file contains a subset of data (1980-2009) for station USC00368449
	weather <- read.dly(system.file("extdata", "USC00368449.dly", package = "VFS"))

	# daily precipitation
	summary(weather$PRCP.VALUE)

	# monthly average maximum temperature
	aggregate(TMAX.VALUE ~ MONTH, FUN = mean, data = weather)

	# generate simulation values
	weather.params <- wth.param(weather)
}

\keyword{ IO }
\keyword{ file }
