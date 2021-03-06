% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mc.discrete.R
\name{mc.discrete}
\alias{mc.discrete}
\title{Markov Chain Simulator}
\usage{
mc.discrete(p,k,n)
}
\arguments{
\item{p}{is a transition probability matrix for Markov Chain simulation.}

\item{k}{is a single integer number that states which state the simulation starts at.}

\item{n}{is a single natural number that decides how many simulation steps is to be done.}
}
\value{
Returns a vector of values corresponding to the stationary probability for each state in the given transition probability matrix.
}
\description{
This function takes 3 arguments to simulate the stationary probabilities from a transition probability matrix.
}
\examples{
mc.discrete(matrix(c(0,0,0,0.4,0.7,0.5,0.6,0.3,0.5),nrow=3,ncol=3),1,100)
}
\author{
Nguyen Khanh Le Ho & Emil H. Andersen \cr
Department of Mathematics and Computer Science (IMADA) \cr
University of Southern Denmark, Denmark \cr
\email{emila14@student.sdu.dk} \cr
\email{ngho14@student.sdu.dk} \cr
}
\keyword{chain}
\keyword{markov}
\keyword{matrix}
\keyword{probability}
\keyword{simulation}
\keyword{transition}

