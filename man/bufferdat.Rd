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
	Currently contains data for a cool-season and a warm-seasong grass buffer.
}

\source{
Haan CT, Barfield BJ, Hayes JC (1994) Design hydrology and sedimentology for small catchments. Acad. Press, San Diego
}

\examples{
	
	# state college GHCN data
	#
	# import is slow: 
	# weather <- read.dly(system.file("extdata", "USC00368449.dly", package = "VFS"))
	data("weather") # same object
	
	weather.param <- wth.param(weather)

	rain.compare <- rainfall(365*3, weather.param)
	temp.compare <- temperature(365*3, weather.param)

	data(soildat)
	data(bufferdat)

	# bluegrass buffer, clay loam soil
	vfs.CL <- VFS(nyears = 3, thissoil = subset(soildat, Soil == "CL"), 
	rain=rain.compare, temperature=temp.compare, 
	thisbuffer = subset(bufferdat, Species == "bluegrass"), Duration = 2, 
	FieldArea = 4000, VFSwidth = 10.7, VFSslope = 0.02, 
	z = 1000, b = c(.5, 1, 1.5, 2.5))

	vfs.CL

	summary(vfs.CL)

}
\keyword{datasets}
