\name{summary.APLE}
\alias{summary.APLE}
\title{
	Summarize the result of APLE
}
\description{
	Summary method for APLE objects.
}
\usage{
	\method{summary}{APLE}(object, ...)
}
\arguments{
  \item{object}{
	APLE object produced by \code{APLE}.
	}
  \item{\dots}{
  	Other arguments to summary
  }
}

\details{
	Calculates means for phosphorus loss.
}

\value{
	Summary of APLE object.
}

\author{
	Sarah Goslee
}

\seealso{
\code{\link{APLE}}, \code{\link{VFSAPLE}}, \code{\link{print.APLE}},
}
\examples{

x <- APLE(soilP = 127, clay = 17, OM = 6, precip = 35, runoff = 6,
	erosion = 7, manureP = 25, manureSolids = 25, manureWEP = 50,
	manureIn = 40, fertP = 10, fertIn = 40)

print(x)
summary(x)

}

\keyword{ methods }

