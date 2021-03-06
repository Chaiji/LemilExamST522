% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dens.estimator.R
\name{dens.estimator}
\alias{dens.estimator}
\title{Density Estimator}
\usage{
dens.estimator(x,d,h,method="naive")
}
\arguments{
\item{x}{is data given as a vector of numbers, to estimate density from.}

\item{d}{is a number or a vector of numbers, that specifies what points density should be returned for. If nothing is chosen, a table of
relevant values are given: Min, 1st quantile, median, mean, 3rd quantile and max of the input data x.}

\item{h}{is a positive number that specifies the bandwidth used for the density estimation. If nothing is chosen, h is estimated
using either Struges method or Silverman's method, depending on the method parameter.}

\item{method}{is an argument that is either "naive" or "kernel", deciding the method of estimating the density. Defaults to "naive".}
}
\value{
Depending on input, either returns density on specific points d, or returns a bandwidth as well as a table of densities estimated
at points on data given as: minimum, 1st quantile, median, mean, 3rd quantile, and maximum of x.
}
\description{
This function takes between 1 and 4 arguments to estimate density of data.
}
\examples{
dens.estimator(cars$speed)
dens.estimator(cars$speed,method="kernel")
dens.estimator(cars$speed,10,3,method="naive")
}
\author{
Nguyen Khanh Le Ho & Emil H. Andersen \cr
Department of Mathematics and Computer Science (IMADA) \cr
University of Southern Denmark, Denmark \cr
\email{emila14@student.sdu.dk} \cr
\email{ngho14@student.sdu.dk} \cr
}
\keyword{bandwidth}
\keyword{data}
\keyword{density}
\keyword{estimate}
\keyword{gaussian}
\keyword{kernel}
\keyword{naive}
\keyword{silverman}
\keyword{struges}
\keyword{sturges}

