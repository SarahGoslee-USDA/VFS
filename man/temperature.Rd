\name{temperature}
\alias{temperature}
\title{
	Generate simulated mean temperature
}
\description{
	Generates simulated daily temperature minimum and maximum based on parameters derived from daily weather data.
}
\usage{
	temperature(ndays, thiswth)
}

\arguments{
  \item{ndays}{
	number of days to simulate
}
  \item{thiswth}{
	list output of \code{wth.param}. 
}

}

\value{
	Returns a vector of daily mean temperature (X).
}

\details{
	This is a very simple temperature simulation, using three parameters derived from daily weather data and the day of year to calculate a smooth annual temperature change derived from the first harmonic of a Fourier function.
}


\author{
	Heather Gall and Sarah Goslee
}
\references{
Grimenes, A. and Nissen, O. (2004) Mathematical modeling of the annual temperature wave based on monthly mean temperatures, and comparisons between local climate trends at seven Norwegian stations. \emph{Theor Appl Climatol} \bold{78}, 229--246. \doi{10.1007/s00704-004-0036-9}
}


\seealso{
	\code{\link{wth.param}},
	\code{\link{rainfall}}
}
\examples{
# A sample GHCN daily weather file for State College, PA, is included with this package.
# This file contains a subset of data (1980-2009) for station USC00368449
data("weather") 

# calculate parameters for the poisson model, using 0.3 mm as the lower limit for wet days.
weather.param.p <- wth.param(weather, method = "poisson", llim = 0.3)

# simulate 10 years of temperature
temp10 <- temperature(365*10, weather.param.p)

}

\keyword{ datagen }

