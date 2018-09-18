\name{soildat}
\alias{soildat}
\docType{data}
\title{
	Soil texture class properties
}
\description{
	Basic hydrologic properties for twelve soil texture classes.
}
\usage{data("soildat")}
\format{
  A data frame with 12 observations on the following 9 variables.
  \describe{
    \item{\code{Soil}}{Texture class abbreviation.}
    \item{\code{SoilName}}{Texture class name.}
    \item{\code{Ksat}}{Saturated hydraulic conductivity (mm d^(-1)).}
    \item{\code{ThetaSAT}}{Water potential at saturation.}
    \item{\code{ThetaFC}}{Water potential at field capacity.}
    \item{\code{ThetaWP}}{Water potential at wilt point.}
    \item{\code{ff}}{Fraction of fine (clay) particles.}
    \item{\code{fm}}{Fraction of medium (silt) particles.}
    \item{\code{fc}}{Fraction of coarse (sand) particles.}
  }
}

\source{

Clapp, RB, Hornberger, GM. 1978. Empirical equations for some soil hydraulic properties. Water Resour Res 14:601-604. DOI: 10.1029/WR014i004p00601.

Karkanis, PG. 1983. Determining field capacity and wilting point using soil saturation by capillary rise. Can Agr Eng 25:19-21.

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
