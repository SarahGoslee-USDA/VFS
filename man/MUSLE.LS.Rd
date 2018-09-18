\name{MUSLE.LS}
\alias{MUSLE.LS}

\title{
    Estimate landscape factor LS
}
\description{
    Estimates MUSLE landscape factor for a homogeneous field from the field length and slope.
}
\usage{
MUSLE.LS(FieldLength, FieldSlope)
}
\arguments{
  \item{FieldLength}{
    Field length (ft).
}
  \item{FieldSlope}{
    Field slope (ft/ft).
}
}
\value{
    MUSLE landscape factor LS.
}
\references{
Wischmeier, W. H. and Smith, D. D. (1978) \emph{Predicting rainfall erosion losses-a guide to conservation planning.} U.S. Department of Agriculture, Agriculture Handbook No. 537.
}
\author{
Sarah Goslee
}

\seealso{
\code{\link{MUSLE}}
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


