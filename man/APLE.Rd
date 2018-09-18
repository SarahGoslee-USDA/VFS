\name{APLE}
\alias{APLE}
\title{
    Agricultural Phosphorus Loss Estimator 
}
\description{
    Agricultural loss of phosphorus based on soil phosphorus level, additions of fertilizer and manure, and erosion.
}
\usage{
APLE(soilP, clay, OM, precip, runoff, erosion, manureP = 25,
    manureSolids = 25, manureWEP = 50, manureIn = 40,
	fertP = 10, fertIn = 40)
}
\arguments{
  \item{soilP}{
soil test Mehlich 3 phosphorus (mg/kg).
}
  \item{clay}{
soil clay (\%).
}
  \item{OM}{
soil organic matter (\%).
}
  \item{precip}{
annual precipitation (in).
}
  \item{runoff}{
annual runoff (in)
}
  \item{erosion}{
annual erosion (ton/ac).
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
This function implements the basic version of the spreadsheet-based Agricultural Phosphorus Loss Estimator model (APLE) in R, and is vectorized. This model calculates annual phosphorus loss by compartment (due to erosion, dissolved soil phosphorus, dissolved manure, dissolved fertilizer) based rainfall, soil properties, and management.
The units match those of the original spreadsheet.
}
\value{
\item{lossErosion}{soil erosion phosphorus loss (kg/ha).}
\item{lossDissolvedSoil}{soil dissolved phosphorus loss (kg/ha).}
\item{lossDissolvedManure}{manure dissolved phosphorus loss (kg/ha).}
\item{lossDissolvedFertilizer}{fertilizer dissolved phosphorus loss (kg/ha).}
\item{lossTotal}{total phosphorus loss (kg/ha).}

}
\references{
Vadas, P. A., Good, L. W., Moore, P. A., Jr. and Widman, N. (2009) Estimating phosphorus loss in runoff from manure and fertilizer for a phosphorus loss quantification tool. \emph{J Environ Qual} \bold{38}, 1645--1653. \doi{10.2134/jeq2008.0337}
}
\author{
Sarah Goslee
}


\seealso{
\code{\link{VFSAPLE}}
}
\examples{

APLE(soilP = 127, clay = 17, OM = 6, precip = 35, runoff = 6,
erosion = 7, manureP = 25, manureSolids = 25, manureWEP = 50,
manureIn = 40, fertP = 10, fertIn = 40)

}

