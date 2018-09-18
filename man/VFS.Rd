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
	Boolean describing whether to store soil water; if FALSE, soil is always at field capacity. This option allows the effect of soil water storage to be quantified.
}
  \item{runoffcalc}{
	Boolean describing whether to use intensity and saturation exceedances; if FALSE, all rainfall becomes runoff. This option allows the effect of runoff calculation to be quantified.
}
}
\details{
    The concentration-discharge (C-Q) model of erosion is intended to produce relative erosion values, rather than absolute values, but will produce absolute values if a and b are known.
    The MUSLE field erosion model is run alongside the C-Q model. The K factor is estimated from soil texture data using \code{MUSLE.K}, and the LS factor from field properties using \code{MUSLE.LS}.
	Blaney-Criddle coefficients for evapotranspiration calculations from a cornfield are hard-coded; a future update will allow for varying the type of field.

}
\value{

Returns an object of class VFS, comprising:

    \item{daily}{Daily output of all public variables that change as a function of time. The data frame has columns:}
	\itemize{
	    \item{rain: precipitation (mm).}
	    \item{temperature: mean temperature (C).}
	    \item{S: soil water storage, (mm).}
	    \item{kt: Blaney-Criddle temperature coefficient.}
	    \item{ET: evapotranspiration (mm).}
	    \item{intensity: rainfall intensity (mm).}
	    \item{runoff: runoff (mm).}
	    \item{Q: discharge (ft^3/s).}
	    \item{fd: flow depth through VFS (ft).}
	    \item{R: hydraulic radius of filter strip (ft).}
	    \item{Vm: Manning's velocity (ft/s).}
	    \item{Re: Reynold's number.}
	    \item{Va: actual shear stress (ft/s).}
	    \item{Nfc: Fall number for coarse particles.}
	    \item{Nfm: Fall number for medium particles.}
	    \item{Nff: Fall number for fine particles.}
	    \item{fdc: Trapping efficiency for coarse particles.}
	    \item{fdm: Trapping efficiency for medium particles.}
	    \item{fdf: Trapping efficiency for fine particles.}
	    \item{Ft: Total trapping efficiency of filter strip.}
	    \item{peakflow: peak flow (m^3/s).}
	}
    \item{field}{Data on the field being modeled:}
	\itemize{
	    \item{clay: soil clay content (\%}.)
	    \item{area: field area (m^2).}
	}
    \item{Conc}{Sediment concentration (in mass/volume) as calculated by the relationship C = aQ^b; specific units depend on units conversions included in the value of a.}
    \item{MassIn}{Sediment load (mass) from the C-Q model, as calculated by multiplying concentration and runoff volume. If concentration is assumed to be in g/L, then the load is calculated in g.}
    \item{MassOut}{Sediment mass from the C-Q model that leaves the vegetated filter strip at the end of a runoff event (i.e., the mass that is not removed).}
    \item{MassRemoved}{Sediment mass from the C-Q model that remains in the vegetated filter strip at the end of a runoff event.}
    \item{AnnualMassIn}{Sum of the sediment loads from the C-Q model entering the vegetated filter strip over the course of one year.}
    \item{AnnualMassOut}{Sum of the sediment loads from the C-Q model leaving the vegetated filter strip over the course of one year.}
    \item{AnnualRemovalEfficiency}{The removal effficiency from the C-Q model of the vegetated filter strip at an annual time scale (\%).}
    \item{MassInMUSLE}{Sediment mass from the MUSLE model leaving the crop field .}
    \item{MassOutMUSLE}{Sediment mass from the MUSLE model that leaves the vegetated filter strip at the end of a runoff event (t/day).}
    \item{MassRemovedMUSLE}{Sediment mass that remains in the vegetated filter strip at the end of a runoff event (t/day).}
    \item{AnnualMassInMUSLE}{Sum of the sediment loads entering the vegetated filter strip over the course of one year (t).}
    \item{AnnualMassOutMUSLE}{Sum of the sediment loads leaving the vegetated filter strip over the course of one year (t).}
    \item{AnnualRemovalEfficiencyMUSLE}{The removal effficiency of the vegetated filter strip at an annual time scale (\%).}
    \item{Ftannual}{Filter strip removal efficiency.}
    \item{Ftannualavg}{The average of all per-event trapping efficiencies over the course of one year.}
}
\references{
Gall, H. E., Schultz, D., Veith, T. L, Goslee, S. C., Mejia, A., Harman, C. J., Raj, C. and Patterson, P. H. (2018) The effects of disproportional load contributions on quantifying vegetated filter strip sediment trapping efficiencies. \emph{Stoch Environ Res Risk Assess} \bold{32}(8), 2369--2380. \doi{10.1007/s00477-017-1505-x}

Haan C. T., Barfield B. J. and Hayes J. C. (1994) \emph{Design hydrology and sedimentology for small catchments.} Acad Press, San Diego.

Williams, J. R. (1975) Sediment-yield prediction with universal equation using runoff energy factor. Pp. 244--251 in: \emph{Present and prospective technology for predicting sediment yield and sources.} ARS.S-40, US Gov. Print. Office, Washington, DC. 244-252.

Wischmeier, W. H. and Smith, D. D. (1978) \emph{Predicting rainfall erosion losses-a guide to conservation planning.} U.S. Department of Agriculture, Agriculture Handbook No. 537.
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

