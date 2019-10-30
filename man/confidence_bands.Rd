\name{confidence_bands}
\alias{confidence_bands}
\alias{prediction_interval}
\title{Confidence bands or prediction intervals for a bivariate linear regression}
\usage{
	confidence_bands(fit,type=c("polygon","lines","none"),
		level=0.95)
	prediction_interval(fit,type=c("polygon","lines","none"),
		level=0.95)
}
\arguments{
	\item{fit}{object of class \code{"lm"}.}
	\item{type}{type of band or interval to plot.}
	\item{level}{confidence level.}
}
\description{
	Plots a confidence band or prediction interval.
}
\value{
	Returns a data frame invisibly containing the values for \emph{X} and lower/upper bounds of \emph{Y}.
}
\author{Liam Revell \email{liam.revell@umb.edu}}
\keyword{statistics}
