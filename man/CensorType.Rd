\name{CensorType}
\alias{CensorType}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Determine types of censoring based on censoring intervals
}
\description{
This function determines types of censoring based on censoring intervals. It is called by function \code{\link{gLRT1}}, \code{\link{gLRT2}}, and \code{\link{gLRT3}}.
}
\usage{
CensorType(A, inf)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{
a matrix containing the end points of censoring intervals in the first two columns
}
  \item{inf}{
value representing infinity
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
 \item{cens }{a vector of censoring types with value 1 for left-censoring, 2 for interval-censoring, 3 for right-censoring, and 4 for exact observation }
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Qiang Zhao and Jianguo Sun
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
A = matrix(c(1, 3, 0, 4, 3, 100, 3, 3), ncol=2, byrow=TRUE)
CensorType(A, 100)
}
