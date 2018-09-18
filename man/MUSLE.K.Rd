\name{MUSLE.K}
\alias{MUSLE.K}
\title{
    Estimate soil erodibility factor K.
}
\description{
    Estimates MUSLE soil erodibility from a multiple regression model of soil texture. 
}
\usage{
MUSLE.K(fc, fm, ff)
}
\arguments{
  \item{fc}{
    Fraction of coarse material (sand) in the soil (0-1).
}
  \item{fm}{
    Fraction of medium material (silt) in the soil (0-1).
}
  \item{ff}{
    Fraction of fine material (clay) in the soil (0-1).
}
}
\details{
    If K is not available from other sources, it can be estimated from soil texture (Goslee, in review).
}
\value{
    Returns the soil erodibility factor K.
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

