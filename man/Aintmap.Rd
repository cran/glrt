\name{Aintmap}
\alias{Aintmap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create A matrix and intmap
}
\description{
This function is borrowed from package \code{interval} by Professor Michael Fay and used in function \code{\link{ModifiedEMICM}} to provide an NPMLE for a survival function. The A matrix is an n by m matrix of zeros and ones, where each row represents one of n failure times, and each column represents a possible interval for the nonparametric maximum likelihood estimate (NPMLE). The function \code{Aintmap} creates an A matrix and associated intmap from left and right intervals (L and R) which may not include the boundary of the interval (using Lin or Rin). The matrix intmap denotes the intervals of the potential jumps in the distribution of the NPMLE, and its attribute LRin denotes whether to include each of the interval or not.
}
\usage{
Aintmap(L, R, Lin = NULL, Rin = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{L}{
numeric vector of left endpoints of censoring interval
}
  \item{R}{
numeric vector of right endpoints of censoring interval
}
  \item{Lin}{
logical vector, should L be included in the interval? (see details)
}
  \item{Rin}{
logical vector, should R be included in the interval? (see details)
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A list with two objects:
  \item{A }{an n by m matrix of 0 and 1s}
  \item{intmap }{the associated intmap}
}
%\references{
%% ~put references to the literature/web site here ~
%}
%\author{
%Michael Fay
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{ModifiedEMICM}}
}
\examples{Aintmap(c(2,3,3,7),c(3,5,5,8),Lin=c(FALSE,TRUE,FALSE,FALSE),
Rin=c(TRUE,FALSE,TRUE,FALSE))}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
