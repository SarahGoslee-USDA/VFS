\name{print.APLE}
\alias{print.APLE}
\title{
	Printing the result of APLE
}
\description{
	Print method for APLE objects.
}
\usage{
	\method{print}{APLE}(x, ...)
}
\arguments{
  \item{x}{
	An APLE object produced by \code{APLE} or \code{VFSAPLE}.
}
  \item{\dots}{
  	Other arguments to print.
  }
}
\details{
    Prints the annual mean for erosion, soil dissolved, manure dissolved, fertilizer dissolved, and total phosphorus losses.
}
\value{
	Returns the APLE object x invisibly.
}
\author{
	Sarah Goslee
}

\seealso{
\code{\link{APLE}}, \code{\link{VFSAPLE}}, \code{\link{summary.APLE}},
}
\examples{
x <- APLE(soilP = 127, clay = 17, OM = 6, precip = 35, runoff = 6,
	erosion = 7, manureP = 25, manureSolids = 25, manureWEP = 50,
	manureIn = 40, fertP = 10, fertIn = 40)

print(x)
summary(x)

}
\keyword{ print }

