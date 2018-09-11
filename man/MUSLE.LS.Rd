\name{MUSLE.LS}
\alias{MUSLE.LS}

\title{
    Estimate landscape factor LS.
}
\description{
    Estimates MUSLE landscape factor for a homogeneous field from the field length and slope.
}
\usage{
MUSLE.LS(FieldLength, FieldSlope)
}
\arguments{
  \item{FieldLength}{
    Field length (ft)
}
  \item{FieldSlope}{
    Field slope (ft/ft)
}
}
\value{
    MUSLE landscape factor LS.
}
\references{
Wischmeier, W. H., and Smith, D.D. 1978. Predicting rainfall erosion losses-a guide to conservation planning. U.S. Department of Agriculture, Agriculture Handbook No. 537.
}
\author{
Sarah Goslee
}

\seealso{
\code{\link{MUSLE}}
}
\examples{

# 100-foot field with 2% slope
LS <- MUSLE.LS(100, .02)

}


