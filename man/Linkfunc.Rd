\name{Linkfunc}
\alias{Linkfunc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A link function used in tests \code{gLRT2}, \code{gLRT3}, and \code{gLRT4}
}
\description{
A link function over (0, 1) used in \code{\link{gLRT2}}, \code{\link{gLRT3}}, and \code{\link{gLRT4}}, which is a function of the original link function \eqn{\xi(x) = x log(x) x^\rho (1 - x)^\gamma.}
}
\usage{
Linkfunc(x, rho = 0, gamma = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a value between 0 and 1
}
  \item{rho}{a non-negative parameter in \eqn{[0, 1]}
}
  \item{gamma}{a non-negative parameter in \eqn{[0, 1]}
}
}
\details{
This function \eqn{\eta(x)} = \eqn{1 - \xi(1-x)}.
}
\value{
Function value is returned.
}
\references{
X. Zhao, Q. Zhao, J. Sun, Q. and J. S. Kim (2008), "Generalized Log-rank Tests for Partly Interval-Censored Failure Time Data", Biometrical Journal, 50 (3): 375-385.
}
\author{
Qiang Zhao and Jianguo Sun
}
\note{
This is not to be called by the user.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{LinkfuncDir}}
}

\examples{
Linkfunc(0.2, rho=0, gamma=1)
}

