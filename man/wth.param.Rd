\name{wth.param}
\alias{wth.param}
\title{
	Calculate weather parameters from daily data for use in climate simulations
}
\description{
	The climate generation functions for rainfall and temperature require parameters calculated from GHCN daily weather data, or from any data frame with columns containing year, month, day, precipitation, and minimum and maximum temperature.
	Partial years at the beginning or end of the dataset are removed. Leap days are also removed to standardize day-of-year calculation.

}
\usage{
wth.param(dly, llim = 0, method = "poisson", year.col = "YEAR", 
	month.col = "MONTH", day.col = "DAY", prcp.col = "PRCP.VALUE", 
	tmin.col = "TMIN.VALUE", tmax.col = "TMAX.VALUE")
}

\arguments{
	\item{dly}{
		A data frame, such as the output of \code{read.dly()}, with days as rows and columns including YEAR, MONTH, DAY, PRCP.VALUE, TMIN.VALUE, TMAX.VALUE (for GHCN data), or with those columns having names specivied by arguments.
	}
	\item{llim}{
		The minimum daily rainfall for a wet day to be counted.
	}
	\item{method}{
		Choice of model for which to calculate parameters, either "poisson" or "markov".
	}
	\item{year.col}{
		Name of the column containing year number.
	}
	\item{month.col}{
		Name of the column containing month number.
	}
	\item{day.col}{
		Name of the column containing day number.
	}
	\item{prcp.col}{
		Name of the column containing daily precipitation.
	}
	\item{tmin.col}{
		Name of the column containing daily minimum temperature.
	}
	\item{tmax.col}{
		Name of the column containing daily maximum temperature.
	}
}

\value{
    	\item{params}{Parameters for simulating long-term point rainfall. For \code{method = "poisson"}, a list of parameters containing:
		\itemize{
			\item{lambda: Mean rainfall inter-arrival frequency (d-1).}
			\item{depth: Mean rainfall depth (mm).}
			\item{A: Mean annual temperature (C).}
			\item{B: Temperature half-amplitude (C).}
			\item{C: Day of the year with minimum temperature (DOY).}
			\item{start: First full year of the weather data.}
			\item{end: Last full year of the weather data.}
		}
		For \code{method = "markov"}, a data frame with one row per month and columns:
		\itemize{
		    \item{tmin: Minimum temperature.}
		    \item{tminsd: Maximum temperature.}
		    \item{tmax: Minimum temperature standard deviation.}
		    \item{tmaxsd: Maximum temperature standard deviation.}
		    \item{prcp: Monthly precipitation.}
		    \item{prcpmean: Mean size of a precipitation event.}
		    \item{prcpmax: Maximum size of a precipitation event.}
		    \item{prcpsd: Standard deviation of precipitation event sizes.}
		    \item{prcpskew: Skew of precipitation event sizes.}
		    \item{prcpwet: Number of wet days (greater than \code{llim}).}
		    \item{prcpww: Probability of a wet day following a wet day.}
		    \item{prcpdw: Probability of a wet day following a dry day.}
		}
	}
	\item{temperature}{Parameters for simulating long-term daily temperature.}
	\item{llim}{Minimum daily rainfall for a wet day.}
	\item{start}{First full year of weather data}
	\item{end}{Last full year of weather data}
}

\details{
	The rainfall simulation currently offers choice of two methods: the simple Poisson model of Rodriguez-Iturbe et al. (1999), and the Markov chain model of Nicks (1974). The latter rainfall calculation is used by the APEX farm model, among others, and is based on monthly statistics.
NOTE: For reasons of time and space, the example contains only ten years of daily weather data. We suggest using thirty years for estimating parameter values.
}
\references{
Rodriguez-Iturbe, I., Porporato, A., Ridolfi, L., Isham, V. and Coxi, D. R. (1999) Probabilistic modelling of water balance at a point: the role of climate, soil and vegetation. \emph{Proc Royal Soc A} \bold{455}, 269--288.

Nicks, A. D. (1974) Stochastic generation of the occurrence, pattern and location of maximum amount of daily rainfall. Pp. 154--171 in: \emph{Proceedings Symposium on Statistical Hydrology.} USDA Agricultural Research Service Miscellaneous Publication No. 1275, Washington, DC.
}
\author{
	Sarah Goslee
}

\seealso{
	\code{\link{read.dly}},
	\code{\link{rainfall}},
	\code{\link{temperature}}
}
\examples{

# GHCN daily weather file for State College, PA
# subset of data (2000-2009) for station USC00368449
#
data("weather") 

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

