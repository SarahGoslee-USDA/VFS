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
  	Other arguments to print
  }
}
\value{
	It returns the VFS x invisibly.
}

\author{
	Sarah Goslee
}

\seealso{
\code{\link{VFS}}, \code{\link{summary.VFS}},
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
\keyword{ print }

