\name{VFS-package}
\alias{VFS-package}
\docType{package}
\title{
\packageTitle{VFS}
}
\description{
\packageDescription{VFS}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{VFS}
\packageIndices{VFS}
	This package implements runoff, erosion, filter strip, and phosphorus loss models in R.
}
\author{
\packageAuthor{VFS}

Maintainer: \packageMaintainer{VFS}
}
\references{
Gall, H. E., Schultz, D., Veith, T. L, Goslee, S. C., Mejia, A., Harman, C. J., Raj, C. and Patterson, P. H. (2018) The effects of disproportional load contributions on quantifying vegetated filter strip sediment trapping efficiencies. \emph{Stoch Environ Res Risk Assess} \bold{32}(8), 2369--2380. \doi{10.1007/s00477-017-1505-x}
}
\keyword{ package }

\examples{

# state college GHCN data
#
# weather <- read.dly(system.file("extdata", "USC00368449.dly", package = "VFS"))
data("weather") # same object

weather.param <- wth.param(weather, method="markov")

rain.compare <- rainfall(365*3, weather.param)
temp.compare <- temperature(365*3, weather.param)

data(soildat)
data(bufferdat)

# bluegrass buffer, clay loam soil
vfs.CL <- VFS(nyears = 3, thissoil = subset(soildat, Soil == "CL"), 
rain=rain.compare, temperature=temp.compare, 
thisbuffer = subset(bufferdat, Species == "bluegrass"), Duration = 2, 
FieldArea = 4000, VFSwidth = 10.7, VFSslope = 0.02, 
z = 1000, b =1.5)

print(vfs.CL)
summary(vfs.CL)

aple.CL <- VFSAPLE(vfs.CL, soilP = 120, OM = 2)

print(aple.CL)
summary(aple.CL)

}
