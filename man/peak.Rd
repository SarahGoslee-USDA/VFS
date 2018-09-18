\name{peak}
\alias{peak}
\title{
Rational method to calculate peak discharge
}
\description{
    Very simple estimate of peak discharge.
}
\usage{
peak(intensity, area, c = 0.25)
}
\arguments{
  \item{intensity}{
    Precipitation intensity (mm/hr).
}
  \item{area}{
    Field area (ha).
}
  \item{c}{
    Runoff coefficient, related to slope, soil type, and land cover (0-1). Forest may be 0.05 - 0.25, while paved surfaces may be 0.95.
}
}

\value{
    Peak discharge (m^3/s).
}
\references{
Hromadka, T. V, II and Whitley, R. J. (1994) The rational method for peak flow rate estimation. \emph{Water Res Bull} \bold{30}(6), 1001--1009.
}
\author{
    Sarah Goslee
}
\examples{

    # peak discharge from a grassy meadow
    peakd.meadow <- peak(intensity = 55, area = 1, c = 0.1)

    # peak discharge from an urban area
    peakd.urban <- peak(intensity = 55, area = 1, c = 0.8)

}
