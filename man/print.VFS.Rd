\name{print.VFS}
\alias{print.VFS}
\title{
	Printing the result of VFS
}
\description{
	Print method for VFS objects.
}
\usage{
	\method{print}{VFS}(x, ...)
}
\arguments{
  \item{x}{
	A VFS x produced by VFS().
}
  \item{\dots}{
  	Other arguments to print.
  }
}
\value{
	It returns the VFS object invisibly.
}

\author{
	Sarah Goslee
}
\details{
If the VFS object has a modeled filter strip, the mean annual loads in and out, as well as removal efficiencies are printed. Otherwise only the erosion values from the field are printed.
}


\seealso{
\code{\link{VFS}}, \code{\link{summary.VFS}}
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
\keyword{ print }

