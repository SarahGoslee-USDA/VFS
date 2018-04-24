\name{wth.param}
\alias{wth.param}
\title{
	Calculate weather parameters from daily data for use in climate simulations
}
\description{
	The climate generation functions for rainfall and temperature require parameters calculated from daily weather data. This function calculates those parameters from GHCN data, or from any data frame with columns YEAR, MONTH, DAY, PRCP.VALUE, TMIN.VALUE, TMAX.VALUE.
	Partial years at the beginning or end of the dataset are removed. Leap days are also removed to standardize day-of-year calculation.

}
\usage{
	wth.param(dly)
}

\arguments{
	\item{dly}{
		A data frame, such as the output of \code{read.dly()}, with days as rows and columns including YEAR, MONTH, DAY, PRCP.VALUE, TMIN.VALUE, TMAX.VALUE
	}
}

\value{
	\item{lambda }{Mean rainfall inter-arrival frequency (d-1)}
	\item{depth }{Mean rainfall depth (mm)}
	\item{A }{Mean annual temperature (C)}
	\item{B }{Temperature half-amplitude (C)}
	\item{C }{Day of the year with minimum temperature (DOY)}
	\item{start }{First full year of the weather data}
	\item{end }{Last full year of the weather data}
}
\author{
	Sarah Goslee
}

\seealso{
	\code{\link{read.dly}},
	\code{\link{rainfall}},
	\code{\link{temperature}},
}
\examples{
	# A sample DLY file for State College, PA, is included with this package.
	# This file contains a subset of data (1980-2009) for station USC00368449
	weather <- read.dly(system.file("extdata", "USC00368449.dly", package = "VFS"))

	weather.param <- wth.param(weather)

	# simulate 10 years of rainfall and temperature
	rain.compare <- rainfall(365*10, weather.param)
	temp.compare <- temperature(365*10, weather.param)

	
}

\keyword{ manip }
\keyword{ datagen }
