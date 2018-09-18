\name{VFSAPLE}
\alias{VFSAPLE}

\title{
    Link the VFS and APLE models.
}
\description{
    Uses the erosion and runoff output of VFS as input to APLE. 
}
\usage{
VFSAPLE(x, soilP, OM, manureP = 25, manureSolids = 25, manureWEP = 50,
    manureIn = 40, fertP = 10, fertIn = 40)
}
\arguments{
  \item{x}{
VFS object.
}
  \item{soilP}{
soil test Mehlich 3 phosphorus (mg/kg).
}
  \item{OM}{
soil organic matter (\%).
}
  \item{manureP}{
manure P applied (kg/ha).
}
  \item{manureSolids}{
manure solids (\%).
}
  \item{manureWEP}{
manure water-extractable phosphorus/TP (\%).
}
  \item{manureIn}{
 manure incorporated (\%).
}
  \item{fertP}{
fertilizer phosphorus applied (kg/ha).
}
  \item{fertIn}{
fertilizer incorporated (\%).
}
}
\details{
The \code{VFSAPLE} function handles unit conversion and parameter estimation. Erosion, precipitation, runoff, and field parameters from \code{VFS} are passed to \code{APLE}. It also runs \code{APLE} for both pre- and post-filter strip erosion values, so the efficacy of the filter strip at phosphorus removal can be calculated.
}
\value{
\item{preVFS}{APLE object for pre-filter strip erosion values.}
\item{postVFS}{APLE object for post-filter strip erosion values.}
\item{pErosion}{Efficacy of the filter strip at removing erosion phosphorus (\%).}
\item{pTotal}{Efficacy of the fiter strip at removing total phosphorus (\%).} 
}

\author{
    Sarah Goslee
}


\seealso{
\code{\link{APLE}}, \code{\link{VFS}}
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

