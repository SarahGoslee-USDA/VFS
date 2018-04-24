\name{rainfall}
\alias{rainfall}
\title{
	Generate simulated daily rainfall
}
\description{
	Generates simulated daily rainfall based on parameters derived from daily weather data.
}
\usage{
	rainfall(ndays, thiswth, depth, lambda)
}

\arguments{
  \item{ndays}{
	number of days to simulate
}
  \item{thiswth}{
	list output of \code{wth.param}
}
  \item{depth}{
	Mean rainfall depth (mm). Not used if thiswth is supplied.
}
  \item{lambda}{
	Mean rainfall inter-arrival frequency (d-1). Not used if thiswth is supplied.
}
}

\value{
	Returns a vector of daily rainfall totals (mm).
}

\author{
	Heather Gall and Sarah Goslee 
}

\seealso{
	\code{\link{wth.param}},
	\code{\link{temperature}}
}
\examples{

	# state college GHCN data
	weather <- read.dly(system.file("extdata", "USC00368449.dly", package = "VFS"))
	weather.param <- wth.param(weather)

	rain1 <- rainfall(365, weather.param)

	# increase rainfall
	rain2 <- rainfall(365, depth = weather.param$depth + 5, lambda = weather.param$lambda)
}

\keyword{ datagen }
