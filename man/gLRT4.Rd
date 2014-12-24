\name{gLRT4}
\alias{gLRT4}
\title{
Conduct a generalized logrank test for interval-censored data
}
\description{
Function \code{gLRT4} conducts a \eqn{k}(=2)-sample test for interval-censored survival data. The test is based on Zhao, Duan, Zhao, and Sun (2013). The null hypothesis is that the two survival functions of the failure time are the same, and the alternative hypothesis is that the two functions are not the same. 
}
\usage{
gLRT4(A, k = 2, rho = 0, gamma = 0, EMstep = TRUE, ICMstep = TRUE, 
tol = 1e-06, maxiter = 1000, inf = Inf)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{an \eqn{n} by 3 data matrix with the censoring interval of the format \eqn{(L, R]} in columns 1 & 2 and treatmentment indicator ranging from 0 to \eqn{k-1} in column 3. No exact observations (i.e., \eqn{L = R}) are allowed.}
  \item{k}{number of treatments. Only \eqn{k = 2} is allowed.}
  \item{rho}{non-negative parameter in [0, 1] of the link function used for calculating the test statistics. It is the same as \eqn{b} in Zhao, Duan, Zhao, and Sun (2013). The default is 0.}
  \item{gamma}{non-negative parameter in [0, 1] of the link function used for calculating the test statistics. It is the same as \eqn{c} in Zhao, Duan, Zhao, and Sun (2013). The default is 0.}
  \item{EMstep}{a boolean variable indicating whether to take an EM step in the iteration when estimating the common distribution function. The default is TRUE.}
  \item{ICMstep}{a boolean variable indicating whether to take an ICM step in the iteration when estimating the common distribution function. The default is TRUE.}
  \item{tol}{the maximal \eqn{L_1} distance between successive estimates before stopping iteration when estimating the common distribution function. The default is 1.0e-6.}
  \item{maxiter}{the maximal number of iterations to perform before stopping when estimating the common distribution function. The default is 1000.}
  \item{inf}{value used in data for infinity. The default is Inf.}
}
\details{
The NPMLE of the distribution function for each treatment group is computed by function \code{\link{ModifiedEMICM}}.

The f-test in \code{gLRT4} is based on an F distribution with degrees of freedom 1 and 1.

The link function used in \code{gLRT4} is \eqn{\xi(x) = x log(x) x^\rho (1 - x)^\gamma = x log(x) x^b (1 - x)^c. }
}
\value{
The function returns an object containing the following components: 
\item{method}{test procedure used}
\item{u}{the test statistic} 
\item{v}{\code{NA} is returned}
\item{fstat}{the f-test statistic}
\item{df}{the numerator and denominator degrees of freedom of the f-test, i.e., (1, 1)}
\item{p}{p-value of the f-test}
}
\references{
X. Zhao, R. Duan, Q. Zhao, and J. Sun (2013), "A New Class of Generalized Log Rank Tests for Interval-censored Failure Time Data", Computational Statistics and Data Analysis. 60: 123-131.
}
\author{ Qiang Zhao and Jianguo Sun }

%%\note{
%%  ~~further notes~~
%%}

\seealso{
\code{\link{gLRT}}, \code{\link{gLRT1}},  \code{\link{gLRT2}}, \code{\link{gLRT3}}, \code{\link{ScoreTest}}  
}
\examples{
data(cmv)
cmvBlood = cmv[,c(2,3,6)]
cmvUrine = cmv[, 4:6]
gLRT4(cmvBlood)
gLRT4(cmvUrine, rho=1, gamma=1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{generalized logrank test}
%\keyword{score test}
