\name{bufferdat}
\alias{bufferdat}
\docType{data}
\title{
	Parameters for vegetated buffers
}
\description{
	Contains parameters describing vegetated filter strips for use in VFS modeling.
}
\usage{data("bufferdat")}
\format{
  A data frame with 2 observations on the following 3 variables.
  \describe{
    \item{\code{Species}}{Type of buffer}
    \item{\code{bg}}{Average stem spacing (cm)}
    \item{\code{n}}{Manning's roughness coefficient (s m^(-1/3))}
  }
}
\details{
	Currently contains data for a cool-season and a warm-season grass buffer.
}

\source{
Haan CT, Barfield BJ, Hayes JC (1994) Design hydrology and sedimentology for small catchments. Acad. Press, San Diego
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

aple.CL <- VFSAPLE(vfs.CL, soilP = 120, OM = 2)

print(aple.CL)
summary(aple.CL)

}
\keyword{datasets}
