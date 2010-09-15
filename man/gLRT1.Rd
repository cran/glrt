\name{gLRT1}
\alias{gLRT1}
\title{
Conduct a generalized logrank test for interval-censored data
}
\description{
Function \code{gLRT1} conducts a k(>=2)-sample test for interval-censored survival data. The test is based on Zhao and Sun (2004). The null hypothesis is that all k survival functions of the failure time are the same, and the alternative hypothesis is that not all functions are the same. 
}
\usage{
gLRT1(A, k = 2, M = 50, EMstep = TRUE, ICMstep = TRUE, tol = 1e-07, 
maxiter = 1000, inf = Inf)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{
an n by 3 data matrix with the censoring interval of the format (L, R] in columns 1 & 2 and treatmentment indicator ranging from 0 to k-1 in column 3.
}
  \item{k}{
number of treatments. The default is 2.
}
  \item{M}{
number of multiple imputations used in estimating the covariance of the test statistic. The default is 50.
}
  \item{EMstep}{
a boolean variable indicating whether to take an EM step in the iteration when estimating the common distribution function. The default is TRUE.
}
  \item{ICMstep}{
a boolean variable indicating whether to take an ICM step in the iteration when estimating the common distribution function. The default is TRUE.
}
  \item{tol}{
the maximal L1 distance between successive estimates before stopping iteration when estimating the common distribution function. The default is 1.0e-7.
}
  \item{maxiter}{
the maximal number of iterations to perform before stopping when estimating the common distribution function. The default is 1000.}
  \item{inf}{value used in data for infinity. The default is Inf.}
}
\details{
Under the null hypothesis, the NPMLE of the common distribution function is computed by function \code{\link{ModifiedEMICM}}.

Censoring interval for each observation take the form (Li, Ri]. For exact observations, Li = Ri.

The chi-square test used in \code{gLRT1} has k-1 degrees of freedom.
}
\value{
The function returns an object containing the following components: 
\item{method}{test procedure used}
\item{u}{the test statistic} 
\item{v}{the estimated covariance of the test statistic}
\item{chisq}{the chisquare test statistic}
\item{df}{the degrees of freedom of the test}
\item{p}{p-value of the test}
}
\references{
Q. Zhao and J. Sun (2004), "Generalized Log-rank Test for Mixed-Censored Failure Time Data", Statistics in Medicine, 23: 1621-1629.
}
\author{
Qiang Zhao and Jianguo Sun
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{gLRT}}, \code{\link{gLRT2}},  \code{\link{gLRT3}},  \code{\link{ScoreTest}}  
}

\examples{
data(cosmesis)
gLRT1(cosmesis, inf=100)

data(diabetes)
gLRT1(diabetes, M=20)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
