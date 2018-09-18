\name{MUSLE}
\alias{MUSLE}
\title{
    Modified Universal Soil Loss Equation
}
\description{
    Simulation of soil erosion on a daily timestep.
}
\usage{
MUSLE(Q, qp, A, K, LS, C = 0.085, P = 0.40, a = 11.8, b = 0.56)
}
\arguments{
  \item{Q}{
    Runoff volume (m^3/d).
}
  \item{qp}{
    Runoff peak discharge (m^3/s).
}
  \item{A}{
    Field area (ha).
}
  \item{K}{
    Soil erodibility factor.
}
  \item{LS}{
    Landscape factor.
}
  \item{C}{
    Crop management factor. Default is for a corn field.
}
  \item{P}{
    Erosion control practice factor. 
}
  \item{a}{
    Location coefficient. Default value from Williams 1975.
}
  \item{b}{
    Location coefficient. Default value from Williams 1975.
}
}
\details{
    Uses the Modified Universal Soil Loss Equation to estimate daily sediment yield. If K and LS are not known, they can be estimated from soil or field properties using \code{MUSLE.K} and \code{MUSLE.LS}.

	If the location coefficients are known from measured sedimentation data, more accurate estimates can be made.
}
\value{
    Sediment yield (t/day).
}
\references{
Williams, J. R. (1975) Sediment-yield prediction with universal equation using runoff energy factor. Pp. 244--251 in: \emph{Present and prospective technology for predicting sediment yield and sources.} ARS.S-40, US Gov. Print. Office, Washington, DC. 244-252.

Wischmeier, W. H. and Smith, D. D. (1978) \emph{Predicting rainfall erosion losses-a guide to conservation planning.} U.S. Department of Agriculture, Agriculture Handbook No. 537.
}
\author{
Sarah Goslee
}


\seealso{
\code{\link{MUSLE.K}}, \code{\link{MUSLE.LS}}, \code{\link{peak}}
}
\examples{

# Approximate erodibility factor from soil texture.
Kf <- MUSLE.K(.3, .5, .2)

# Calculate landscape factor from field size and shape.
# 100-m field length with 2% slope
# note that MUSLE.LS takes feet
LS <- MUSLE.LS(100 * 3.28, .02)

# assume 0.4 ha cornfield with known rainfall intensity
peakd <- peak(intensity = 55, area = 0.4)

SedYield <- MUSLE(85, qp = peakd, A = .4, K = Kf, LS = LS)

}

