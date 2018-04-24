\name{temperature}
\alias{temperature}
\title{
	Generate simulated mean temperature
}
\description{
	Generates simulated daily rainfall based on parameters derived from daily weather data.
}
\usage{
	temperature(ndays, thiswth, A, B, C)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ndays}{
	number of days to simulate
}
  \item{thiswth}{
	list output of \code{wth.param}
}
  \item{A}{
	Mean annual temperature (C). Not used if thiswth is supplied.
}
  \item{B}{
	Temperature half-amplitude (C). Not used if thiswth is supplied.
}
  \item{C}{
	Day of the year with minimum temperature (DOY). Not used if thiswth is supplied.
}
}

\value{
	Returns a vector of daily mean temperature (X).
}

\details{
	This is a very simple temperature simulation, using three parameters derived from daily weather data and the day of year to calculate a smooth annual temperature change.
}


\author{
	Heather Gall and Sarah Goslee
}



\seealso{
	\code{\link{wth.param}},
	\code{\link{rainfall}}
}
\examples{

	# state college GHCN data
	weather <- read.dly(system.file("extdata", "USC00368449.dly", package = "VFS"))
	weather.param <- wth.param(weather)
	
	temp1 <- temperature(365, weather.param)

	# increase temperature
	temp2 <- temperature(365, A = weather.param$A + 5, B = weather.param$B, C = weather.param$C)
}

\keyword{ datagen }

