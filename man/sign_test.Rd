\name{sign_test}
\alias{sign_test}
\title{Sign-test}
\usage{
	sign_test(x, h0=0, conf.level=0.95)
}
\arguments{
	\item{x}{numeric values for a single sample. (May also be differences between paired values.)}
	\item{h0}{null hypothesis for the median value of \code{x}.}
	\item{conf.level}{confidence level for a confidence interval.}
}
\description{
	Conducts a sign test.
}
\value{
	Returns an object of class \code{"sign.test"} which can be printed or plotted using corresponding methods.
}
\author{Liam Revell \email{liam.revell@umb.edu}}
\examples{
	x1<-runif(n=20)
	object<-sign_test(x1, h0=0.5)
	object
}
\keyword{statistics}
