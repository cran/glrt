\name{LinkfuncDir}
\alias{LinkfuncDir}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Direvative of the link function
}
\description{
Direvative of the link function \eqn{\eta(x), \eta'(x),} used in \code{\link{gLRT2}},  \code{\link{gLRT3}}, and \code{\link{gLRT4}},.
}
\usage{
LinkfuncDir(x, rho = 0, gamma = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a value between 0 and 1
}
  \item{rho}{
a non-negative parameter in \eqn{[0, 1]}
}
  \item{gamma}{
a non-negative parameter in \eqn{[0, 1]}
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
The first derivative of the function.
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
\code{\link{Linkfunc}}
}
\examples{
LinkfuncDir(0.2, rho=0, gamma=1)
}
