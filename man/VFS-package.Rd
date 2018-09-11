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
	This package implements the vegetated filter strip model of Haan et al. (1994) in R.
}
\author{
\packageAuthor{VFS}

Maintainer: \packageMaintainer{VFS}
}
\references{

Haan C.T., Barfield B.J., Hayes J.C. 1994. Design hydrology and sedimentology for small catchments. Acad. Press, San Diego

Gall, H.E., Schultz, D., Veith, T.L, Goslee, S.C., Mejia, A., Harman, C.J., Raj, C., and Patterson, P.H. 2018. The effects of disproportional load contributions on quantifying vegetated filter strip sediment trapping efficiencies. Stoch Environ Res Risk Assess https://doi.org/10.1007/s00477-017-1505-x.

}
\keyword{ package }

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
