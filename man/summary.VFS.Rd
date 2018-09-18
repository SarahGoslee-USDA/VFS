\name{summary.VFS}
\alias{summary.VFS}
\title{
	Summarize the result of VFS
}
\description{
	Summary method for VFS objects.
}
\usage{
	\method{summary}{VFS}(object, ...)
}
\arguments{
  \item{object}{
	A VFS object produced by VFS().
	}
  \item{\dots}{
  	Other arguments to summary
  }
}

\details{
	Calculates means and standard deviations for annual removal efficiencies. If model type is erosion-only (no vegetated filter strip simulated), then only the relevant variables describing erosion from the field are returned.
}

\value{
	\item{ALR }{Annual load reduction.} 
	\item{ALRsd }{SD of annual load reduction.}
	\item{ALR1000 }{Load reduction across full timespan.}
	\item{ALR1000sd }{SD of load reduction across full timespan.}
	\item{APEA }{Annual per-event average reduction.}
	\item{APEAsd }{SD of annual per-event average reduction.}
	\item{SedIn }{Sediment entering the vegetated filter strip per year.} 
	\item{SedInsd }{SD of sediment entering the vegetated filter strip per year.}
	\item{SedLoss }{Sediment lost per year.} 
	\item{SedLosssd }{SD of sediment lost per year.}
	\item{TLR }{Total load reduction.}
	\item{RunoffDays }{Days per year with runoff.}
	\item{RunoffDayssd }{SD of days per year with runoff.}
	\item{Days }{Total number of days.}
}

\author{
	Sarah Goslee
}

\seealso{
\code{\link{VFS}}, \code{\link{print.VFS}}
}
\examples{

# state college GHCN data
#
# weather <- read.dly(system.file("extdata", "USC00368449.dly", package = "VFS"))
data("weather") # same object

weather.param <- wth.param(weather, method="markov")

rain.compare <- rainfall(365*2, weather.param)
temp.compare <- temperature(365*2, weather.param)

data(soildat)
data(bufferdat)

# bluegrass buffer, clay loam soil
# short simulation to cut down on time required
vfs.CL <- VFS(nyears = 2, thissoil = subset(soildat, Soil == "CL"), 
rain=rain.compare, temperature=temp.compare, 
thisbuffer = subset(bufferdat, Species == "bluegrass"), Duration = 2, 
FieldArea = 4000, VFSwidth = 10.7, VFSslope = 0.02, 
z = 1000, b = 1.5)

print(vfs.CL)
summary(vfs.CL)

}

\keyword{ methods }

