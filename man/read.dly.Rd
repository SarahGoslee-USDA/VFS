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
\arguments{
  \item{filename}{
	Filename or URL of a GHCN DLY file.
}
}
\details{
All GHCN DLY files should have these five elements: PRCP (precipitation, originally tenths of a mm but mm in the function output); SNOW (snowfall, mm); SNWD (snow depth, mm), TMAX (maximum temperature, originally tenths of degree C but C in the function output), and  TMIN (minimum temperature, originally tenths of degree C but C in the function output). 

Depending on the station, there may be many other recorded variables. Each variable is accompanied by a series of quality flags, which are preserved from the original file.

Data are in a complex fixed-width format. Please see the \href{https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt}{GHCN readme} for details.

}
\value{

    Returns a data frame with date as three columns, YEAR, MONTH, DAY, and each data value present in the original file along with its quality flags. Please see the \href{https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt}{GHCN readme} for details.

	Note that units for temperature and precipitation have been converted from the GHCN values.

    These columns will always be present in the output:
    \describe{
	\item{\code{YEAR}}{Year.}
	\item{\code{MONTH}}{Month number.}
	\item{\code{DAY}}{Day of month.}
	\item{\code{PRCP}}{Precipitation (mm).}
	\item{\code{TMAX}}{Maximum temperature (C).}
	\item{\code{TMIN}}{Minimum temperature (C).}
    }

}
\references{
	\href{https://www.ncdc.noaa.gov/data-access/land-based-station-data/land-based-datasets/global-historical-climatology-network-ghcn}{GHCN} data comprises both current and historical weather station data world-wide. 
}
\author{
	Sarah Goslee
}

\seealso{
\code{\link{wth.param}}, \code{\link{weather}}
}
\examples{
# ten years of daily weather data, 2000-2009, for State College, PA
weather <- read.dly(system.file("extdata", "USC00368449.dly", package = "VFS"))

# could also use: 
# weather <- read.dly("ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/all/USC00368449.dly")
# weather <- subset(weather, YEAR >= 2000 & YEAR <= 2009)

# daily precipitation
summary(weather$PRCP.VALUE)

# monthly average maximum temperature
aggregate(TMAX.VALUE ~ MONTH, FUN = mean, data = weather)

# generate simulation values
weather.params <- wth.param(weather)
}

\keyword{ IO }
