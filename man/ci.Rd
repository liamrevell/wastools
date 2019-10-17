\name{ci}
\alias{ci}
\title{Confidence interval}
\usage{
	ci(x, conf.level=0.95)
}
\arguments{
	\item{x}{numeric values for a single sample. (May also be differences between paired values.)}
	\item{conf.level}{confidence level for a confidence interval.}
}
\description{
	Computes a confidence interval.
}
\value{
	Returns a vector with two numeric values.
}
\author{Liam Revell \email{liam.revell@umb.edu}}
\examples{
	x<-rnorm(n=100, mean=6, sd=2)
	ci(x)
}
\keyword{statistics}
