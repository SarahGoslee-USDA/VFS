\name{rainfall}
\alias{rainfall}
\title{
	Generate simulated daily rainfall
}
\description{
	Generates simulated daily rainfall based on parameters derived from daily weather data.
}
\usage{
	rainfall(ndays, thiswth, months)
}

\arguments{
  \item{ndays}{
	Number of days to simulate.
}
  \item{thiswth}{
	Output of \code{wth.param} The choice of method in \code{wth.param()} determines what coefficients are provided, and thus which rainfall simulation method is used.
}
  \item{months}{
  If the rainfall simulation method uses monthly statistics (Markov), a vector of month numbers of \code{length(ndays)} may be provided. If it is missing, then January 1 is assumed to be the first day of a 365-day year.
}
}

\value{
	A vector of daily rainfall totals.
}

\details{
	The rainfall simulation currently offers choice of two methods: the simple Poisson model of Rodriguez-Iturbe et al. (1999), and the Markov chain model of Nicks (1974). The latter rainfall calculation is used by the APEX farm model, among others, and is based on monthly statistics.
}
\references{
Rodriguez-Iturbe, I., Porporato, A., Ridolfi, L., Isham, V. and Coxi, D. R. (1999) Probabilistic modelling of water balance at a point: the role of climate, soil and vegetation. \emph{Proc Royal Soc A} \bold{455}, 269--288.

Nicks, A. D. (1974) Stochastic generation of the occurrence, pattern and location of maximum amount of daily rainfall. Pp. 154--171 in: \emph{Proceedings Symposium on Statistical Hydrology.} USDA Agricultural Research Service Miscellaneous Publication No. 1275, Washington, DC.
}

\author{
	Heather Gall and Sarah Goslee 
}
\seealso{
	\code{\link{wth.param}},
	\code{\link{temperature}}
}
\examples{

# GHCN daily weather file for State College, PA
# subset of data (2000-2009) for station USC00368449
#
data("weather") # same object


# calculate parameters for the poisson model
# using 0.3 mm as the lower limit for wet days.
weather.param.p <- wth.param(weather, method = "poisson", llim = 0.3)

# simulate ten years of rainfall
rain10.p <- rainfall(365*10, weather.param.p)

# increase per-event rainfall by 5 mm
weather.param.p5 <- weather.param.p
weather.param.p5$params$depth <- weather.param.p5$params$depth + 5
rain10.p5 <- rainfall(365*10, weather.param.p5)

# calculate parameters for the Markov chain model
# using 0.3 mm as the lower limit for wet days.
weather.param.m <- wth.param(weather, method = "markov", llim = 0.3)

# rainfall() selects Markov model based on input parameter types
rain10.m <- rainfall(365*10, weather.param.m)

# simulate 10 years of temperature
temp10 <- temperature(365*10, weather.param.p)

}

\keyword{ datagen }
