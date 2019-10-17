\name{permute_test}
\alias{permute_test}
\title{Permutation test for a difference in means between two groups}
\usage{
	permute_test(x1, x2, nsim=1000)
}
\arguments{
	\item{x1}{numerical values in group one.}
	\item{x2}{numerical values in group two.}
	\item{nsim}{number of permutation simulations.}
}
\description{
	Conducts a permutation test for a difference in means between two groups.
}
\value{
	Returns an object of class \code{"permute.test"} which can be printed or plotted using corresponding methods.
}
\author{Liam Revell \email{liam.revell@umb.edu}}
\examples{
	x1<-rexp(n=20, rate=1)
	x2<-rexp(n=30, rate=2)
	object<-permute_test(x1, x2)
	object
	plot(object)
}
\keyword{statistics}
