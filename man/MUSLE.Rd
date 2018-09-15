\name{MUSLE}
\alias{MUSLE}
\title{
    Modified Universal Soil Loss Equation
}
\description{
    Simulation of soil erosion on a daily timestep.
}
\usage{
MUSLE(Q, qp, A, C = 0.085, P = 0.4, K, LS)
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
  \item{C}{
    Crop management factor. Default is for a corn field.
}
  \item{P}{
    Erosion control practice factor. 
}
  \item{K}{
    Soil erodibility factor.
}
  \item{LS}{
    Landscape factor.
}
}
\details{
    Uses the Modified Universal Soil Loss Equation to estimate daily sediment yield. If K and LS are not known, they can be estimated from soil or field properties using \code{MUSLE.K} and \code{MUSLE.LS}.
}
\value{
    Sediment yield (t/day)
}
\references{
Williams JR. 1975. Sediment-yield prediction with universal equation using runoff energy factor. Pp. 244-251 In: Present and prospective technology for predicting sediment yield and sources. ARS.S-40, US Gov. Print. Office, Washington, DC. 244-252.

Wischmeier, W. H., and Smith, D.D. 1978. Predicting rainfall erosion losses-a guide to conservation planning. U.S. Department of Agriculture, Agriculture Handbook No. 537

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
    # 100-foot field length with 2% slope
    LS <- MUSLE.LS(100, .02)

    # assume 1 ha cornfield
    peakd <- peak(intensity = 55, area = 1)

#    SedYield <- MUSLE()

}
