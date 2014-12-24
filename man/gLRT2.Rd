\name{gLRT2}
\alias{gLRT2}
\title{
Conduct a generalized logrank test for interval-censored data
}
\description{
The \code{gLRT2} function conducts a \eqn{k}(>=2)-sample test for interval-censored survival data. The test is based on Sun, Zhao, and Zhao (2005). The null hypothesis is that all \eqn{k} survival functions of the failure time are the same, and the alternative hypothesis is that not all functions are the same. 
}
\usage{
gLRT2(A, k = 2, rho = 0, gamma = 0, EMstep = TRUE, ICMstep = TRUE, 
tol = 1e-06, maxiter = 1000, inf = Inf)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{an \eqn{n} by 3 data matrix with the censoring interval of the format \eqn{(L, R]} in columns 1 & 2 and treatmentment indicator ranging from 0 to \eqn{k-1} in column 3. }
  \item{k}{number of treatments. The default is 2.}
  \item{rho}{non-negative parameter of the link function used for calculating the test statistics. The default is 0.}
  \item{gamma}{non-negative parameter of the link function used for calculating the test statistics. The default is 0.}
  \item{EMstep}{a boolean variable indicating whether to take an EM step in the iteration when estimating the common distribution function. The default is TRUE.}
  \item{ICMstep}{a boolean variable indicating whether to take an ICM step in the iteration when estimating the common distribution function. The default is TRUE.}
  \item{tol}{the maximal \eqn{L_1} distance between successive estimates before stopping iteration when estimating the commondistribution function. The default is 1.0e-6.}
  \item{maxiter}{the maximal number of iterations to perform before stopping when estimating the common distribution function. The default is 1000.}
  \item{inf}{value used in data for infinity. The default is Inf.}
}
\details{
Under the null hypothesis, the NPMLE of the common distribution function is computed by function \code{\link{ModifiedEMICM}}.

Censoring interval for each observation take the form \eqn{(L_i, R_i]}. No exact observations are allowed, i.e., \eqn{L_i < R_i}. 

The chi-square test used in \code{gLRT2} has k-1 degrees of freedom.

The link function used in \code{gLRT2} is \eqn{\xi(x) = x log(x) x^\rho (1 - x)^\gamma. }
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
J. Sun, Q. Zhao, and X. Zhao (2005), "Generalized Log-rank Test for Interval-Censored Data", Scandinavian Journal of Statistics, 32: 45-57.

Q. Zhao (2012), "gLRT - A New R Package for Analyzing Interval-censored Survival Data", Interval-Censored Time-to-Event Data: Methods and Applications, CRC Press, 377-396.
}
\author{ Qiang Zhao and Jianguo Sun }

%%\note{
%%  ~~further notes~~
%%}

\seealso{
\code{\link{gLRT}}, \code{\link{gLRT1}}, \code{\link{gLRT3}},  \code{\link{gLRT4}}, \code{\link{ScoreTest}}  
}
\examples{
data(cosmesis)
gLRT2(cosmesis, rho=0, gamma=1, inf=100)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{generalized logrank test}
%\keyword{score test}
