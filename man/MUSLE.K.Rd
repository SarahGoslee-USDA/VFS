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
Wischmeier, W.H., and Smith, D.D. 1978. Predicting rainfall erosion losses-a guide to conservation planning. U.S. Department of Agriculture, Agriculture Handbook No. 537.
}
\author{
    Sarah Goslee
}

\seealso{
\code{\link{MUSLE}}
}
\examples{

    Kf <- MUSLE.K(.3, .5, .2)

}

