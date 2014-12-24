\name{PMGA}
\alias{PMGA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Implement the pool monotone group algorithm
}
\description{
For isotonization problems some increase in speed and decrease in complexity can be achieved through the use of the pool monotone groups algorithm of Y.L. Zhang and M.A. Newton (1997). It isotonizes a weighted and ordered set of values. This function is borrowed from package \code{Icens}.
}
\usage{
PMGA(est, ww=rep(1, length(est)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{est}{
The vector of values, in the appropriate order.
}
  \item{ww}{
The weight vector.
}
}
\details{
To be supplied at some later date.
}
\value{
An Object containing the following components:
  \item{est}{The isotonized estimates.}
  \item{ww}{The weight associated with the isotonized estimates.}
  \item{poolnum}{The number of values pooled in the current estimate.}
  \item{passes}{The number of passes which were required to isotonize the list.}
}
\references{
Y. L. Zhang and M. A. Newton (1997), http://www.stat.wisc.edu/~newton/newton.html.
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
