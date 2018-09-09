\name{VFSsim}
\alias{VFSsim}
\title{
	Vegetated filter strip and erosion model
}
\description{
	Simulated erosion and runoff given climate and soil texture, with or without a vegetated filter strip in place.
}
\usage{
VFSsim(nyears = 1000, thissoil, thisclimate, thisbuffer, rain, Temp, 
  Duration = 2, FieldArea = 4000, VFSwidth = 10.7, VFSslope = 0.02, 
  z = 1000, b = c(0.5, 1, 1.5, 2.5), carrysoilwater = TRUE, runoffcalc = TRUE)
}

\arguments{
  \item{nyears}{
	Number of years to simulate.
}
  \item{thissoil}{
	Soil properties for the site, as from soildat.
}
  \item{thisclimate}{
	Climate parameters for the simulated site, as produced by wth.param.
}
  \item{thisbuffer}{
	Vegetation properties for the buffer strip, as from bufferdat.
}
  \item{rain}{
	Daily rainfall (mm). If missing, simulated using parameters in thisclimate.
}
  \item{Temp}{
	Daily mean temperature (C). If missing, simulated using parameters in thisclimate.
}
  \item{Duration}{
	Rainfall event length. Default is 2 hours.
}
  \item{FieldArea}{
	Field area (m^2)
}
  \item{VFSwidth}{
	filter strip width (m)
}
  \item{VFSslope}{
	slope (m/m)
}
  \item{z}{
	rooting zone depth (mm). Default is 3 m (3000 mm).
}
  \item{b}{
	Empirical parameter that relates concentration and flow in the concentration-discharge relationship, C = aQ^b. Values used in the code are 0.5, 1, 1.5, and 2.5.
}
  \item{carrysoilwater}{
	Boolean describing whether to store soil water; if FALSE, soil is always at field capacity.
}
  \item{runoffcalc}{
	Boolean describing whether to use intensity and saturation exceedances; if FALSE, all rainfall becomes runoff.
}
}
\details{
	To be added.
}
\value{

Returns an object of class VFS, comprising:

	\item{dat }{
	Daily output of all public variables that change as a function of time}
	\item{Conc }{
	Sediment concentration (in mass/volume) as calculated by the relationship C = aQ^b; specific units depend on units conversions lumped into the value of a. Here, simulation results make the most sense when interpreted as g/L}
	\item{Load }{
	Sediment load (mass) as calculated by multiplying concentration and runoff volume. If concentration is assumed to be in g/L, then the load is calculated in g}
	\item{MassRemoved }{
	Sediment mass that remains in the vegetated filter strip at the end of a runoff event}
	\item{MassOut }{
	Sediment mass that leaves the vegetated filter strip at the end of a runoff event (i.e., the mass that is not removed)}
	\item{AnnualLoadIn }{
	Sum of the sediment loads entering the vegetated filter strip over the course of one year}
	\item{AnnualLoadOut }{
	Sum of the sediment loads leaving the vegetated filter strip over the course of one year}
	\item{AnnualRemovalEfficiency }{
	The removal effficiency of the vegetated filter strip at an annual time scale, calculated as (Annual Load In - Annual Load Out)/Annual Load In }
	\item{Ftannual }{}
	\item{Ftannualavg }{
	The average of all per-event trapping efficiencies over the course of one year}
	\item{Ftannualstdev }{
	The standard deviation of all per-event trapping efficiencies over the course of one year}
}
\references{

Haan C.T., Barfield B.J., Hayes J.C. 1994. Design hydrology and sedimentology for small catchments. Acad. Press, San Diego

Gall, H.E., Schultz, D., Veith, T.L, Goslee, S.C., Mejia, A., Harman, C.J., Raj, C., and Patterson, P.H. 2018. The effects of disproportional load contributions on quantifying vegetated filter strip sediment trapping efficiencies. Stoch Environ Res Risk Assess https://doi.org/10.1007/s00477-017-1505-x."


}
\author{
	Sarah Goslee, Heather Gall, and Tamie Veith
}

\seealso{
\code{\link{print.VFS}}, \code{\link{summary.VFS}}, \code{\link{wth.param}}, \code{\link{soildat}}, \code{\link{bufferdat}},
}
\examples{

	# state college GHCN data
	weather <- read.dly(system.file("extdata", "USC00368449.dly", package = "VFS"))
	weather.param <- wth.param(weather)

	rain.compare <- rainfall(365*10, weather.param)
	temp.compare <- temperature(365*10, weather.param)

	data(soildat)
	data(bufferdat)

	# bluegrass buffer, clay loam soil
	vfs.CL <- VFSsim(nyears = 10, thissoil = subset(soildat, Soil == "CL"), 
	rain=rain.compare, Temp=temp.compare, 
	thisbuffer = subset(bufferdat, Species == "bluegrass"), Duration = 2, 
	FieldArea = 4000, VFSwidth = 10.7, VFSslope = 0.02, 
	z = 1000, b = c(.5, 1, 1.5, 2.5))

	vfs.CL

	summary(vfs.CL)

}

\keyword{ ~kwd1 }
