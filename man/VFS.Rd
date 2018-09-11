\name{VFS}
\alias{VFS}
\title{
	Vegetated filter strip and erosion model
}
\description{
	Simulated erosion and runoff given climate and soil texture, with or without a vegetated filter strip in place.
}
\usage{
VFS(nyears = 1000, thissoil, thisbuffer, rain, temperature, 
  Duration = 2, FieldArea = 4000, VFSwidth = 10.7, VFSslope = 0.02,
  FieldSlope,  z = 1000, a = 1, b = 1.5,
  carrysoilwater = TRUE, runoffcalc = TRUE)
}

\arguments{
  \item{nyears}{
	Number of years to simulate.
}
  \item{thissoil}{
	Soil properties for the site, as from soildat.
}
  \item{thisbuffer}{
	Vegetation properties for the buffer strip, as from bufferdat.
}
  \item{rain}{
	Daily rainfall (mm).
}
  \item{temperature}{
	Daily mean temperature (C).
}
  \item{Duration}{
	Rainfall event length. Default is 2 hours.
}
  \item{FieldArea}{
	Field area (m^2).
}
  \item{VFSwidth}{
	Filter strip width (m).
}
  \item{VFSslope}{
	Filter strip slope (m/m).
}
  \item{FieldSlope}{
	Optional field slope (m/m). If missing, VFSslope will be used.
}
  \item{z}{
	Rooting zone depth (mm). Default is 1000 mm. 
}
  \item{a}{
	Empirical parameter that relates concentration and flow in the concentration-discharge relationship, C = aQ^b.
}
  \item{b}{
	Empirical parameter that relates concentration and flow in the concentration-discharge relationship, C = aQ^b. May be a single value or a vector of values.
}
  \item{carrysoilwater}{
	Boolean describing whether to store soil water; if FALSE, soil is always at field capacity.
}
  \item{runoffcalc}{
	Boolean describing whether to use intensity and saturation exceedances; if FALSE, all rainfall becomes runoff.
}
}
\details{
    The C-Q model particularly is intended to produce relative erosion values, rather than absolute values. 
	Blaney-Criddle coefficients for erosion from a cornfield are hard-coded; a future update will allow for varying the type of field.

}
\value{

Returns an object of class VFS, comprising:

	\item{daily}{Daily output of all public variables that change as a function of time.}
	\item{field}{Data on the field being modeled: area, clay content}
	\item{Conc}{Sediment concentration (in mass/volume) as calculated by the relationship C = aQ^b; specific units depend on units conversions lumped into the value of a.}
	\item{Load}{Sediment load (mass) as calculated by multiplying concentration and runoff volume. If concentration is assumed to be in g/L, then the load is calculated in g.}
	\item{MassRemoved}{Sediment mass that remains in the vegetated filter strip at the end of a runoff event.}
	\item{MassOut}{Sediment mass that leaves the vegetated filter strip at the end of a runoff event (i.e., the mass that is not removed).}
	\item{AnnualLoadIn}{Sum of the sediment loads entering the vegetated filter strip over the course of one year.}
	\item{AnnualLoadOut}{Sum of the sediment loads leaving the vegetated filter strip over the course of one year.}
	\item{AnnualRemovalEfficiency}{The removal effficiency of the vegetated filter strip at an annual time scale, calculated as (Annual Load In - Annual Load Out)/Annual Load In.}
	\item{Ftannual}{Filter strip removal efficiency.}
	\item{Ftannualavg}{The average of all per-event trapping efficiencies over the course of one year.}
	\item{Ftannualstdev}{The standard deviation of all per-event trapping efficiencies over the course of one year.}
}
\references{

Haan C.T., Barfield B.J., Hayes J.C. 1994. Design hydrology and sedimentology for small catchments. Acad. Press, San Diego

Gall, H.E., Schultz, D., Veith, T.L, Goslee, S.C., Mejia, A., Harman, C.J., Raj, C., and Patterson, P.H. 2018. The effects of disproportional load contributions on quantifying vegetated filter strip sediment trapping efficiencies. Stoch Environ Res Risk Assess https://doi.org/10.1007/s00477-017-1505-x."


}
\author{
	Heather Gall, Sarah Goslee, and Tamie Veith
}

\seealso{
\code{\link{print.VFS}}, \code{\link{summary.VFS}}, \code{\link{wth.param}}, \code{\link{soildat}}, \code{\link{bufferdat}},
}
\examples{

# state college GHCN data
#
# import is slow: 
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
	z = 1000, b = c(.5, 1, 1.5, 2.5))

vfs.CL

summary(vfs.CL)

}

