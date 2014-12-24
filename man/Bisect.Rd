\name{Bisect}
\alias{Bisect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
An implementation of the bisection algorithm for root finding
}
\description{
Most of the optimization is \code{Icens} have a one dimensional root-finding component. Since the quantities involved are generally restricted to a subset of \eqn{[0, 1]} we use bisection to find the roots. This function is borrowed from package \code{Icens}.
}
%\usage{
%Bisect(tA, pvec, ndir, Meps, tolbis=1e-07)
%}
%- maybe also 'usage' for other objects documented here.
%\arguments{
%  \item{tA}{The transpose of the clique matrix.}
%  \item{pvec}{The current estimate of the probability vector.}
%  \item{ndir{The direction to explore.}
%  \item{Meps}{Machine epsilon, elements of \code{pvec} that are less than this are assumed to be zero.}
%  \item{tolbis}{The tolerance used to determine if the algorithm has converged.}
%}
\details{
We search from \code{pvec} in the direction \code{ndir} to obtain the new value of \code{pvec} that maximizes the likelihood.
}
%\value{
%The new estimate of \code{pvec}.
%}
\references{
Any book on optimization.
}
\author{
Alain Vandal and Robert Gentleman
}
\note{ 
It is used in \code{\link{ModifiedEMICMmac}} and not to be called by the user.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{ModifiedEMICMmac}}
}
%\examples 
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
