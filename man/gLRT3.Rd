\name{gLRT3}
\alias{gLRT3}
\title{
Conduct a generalized logrank test for interval-censored data
}
\description{
Function \code{gLRT3} conducts a k(>=2)-sample test for interval-censored survival data. The test is based on Zhao, Zhao, Sun, and Kim (2008). The null hypothesis is that all k survival functions of the failure time are the same, and the alternative hypothesis is that not all functions are the same. 
}
\usage{
gLRT3(A, k = 2, rho = 0, gamma = 0, EMstep = TRUE, ICMstep = TRUE, 
tol = 1e-07, maxiter = 1000, inf = Inf)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{an n by 3 data matrix with the censoring interval of the format (L, R] in columns 1 & 2 and treatmentment indicator ranging from 0 to k-1 in column 3. }
  \item{k}{number of treatments. The default is 2.}
  \item{rho}{non-negative parameter of the link function used for calculating the test statistics. The default is 0.}
  \item{gamma}{non-negative parameter of the link function used for calculating the test statistics. The default is 0.}
  \item{EMstep}{a boolean variable indicating whether to take an EM step in the iteration when estimating the common distribution function. The default is TRUE.}
  \item{ICMstep}{a boolean variable indicating whether to take an ICM step in the iteration when estimating the common distribution function. The default is TRUE.}
  \item{tol}{the maximal L1 distance between successive estimates before stopping iteration when estimating the common distribution function. The default is 1.0e-7.}
  \item{maxiter}{the maximal number of iterations to perform before stopping when estimating the common distribution function. The default is 1000.}
  \item{inf}{value used in data for infinity. The default is Inf.}
}
\details{
Under the null hypothesis, the NPMLE of the common distribution function is computed by function \code{\link{ModifiedEMICM}}.

Censoring interval for each observation take the form (Li, Ri]. For exact observations, Li = Ri.

The chi-square test in \code{gLRT3} has either k or k - 1 degrees of freedom depending on the existence and proportion of exact observations in each treatment. See Zhao, Zhao, Sun, and Kim (2008) for more details.

The link function used in \code{gLRT3} is \eqn{\xi(x) = x log(x) x^\rho (1 - x)^\gamma. }
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
X. Zhao, Q. Zhao, J. Sun, Q. and J. S. Kim (2008), "Generalized Log-rank Tests for Partly Interval-Censored Failure Time Data", Biometrical Journal, 50 (3): 375-385.
}
\author{ Qiang Zhao and Jianguo Sun }

%%\note{
%%  ~~further notes~~
%%}

\seealso{
\code{\link{gLRT}}, \code{\link{gLRT1}},  \code{\link{gLRT2}}, \code{\link{ScoreTest}}  
}
\examples{
data(cosmesis)
gLRT3(cosmesis, rho=1, inf=100)

data(diabetes)
gLRT3(diabetes, gamma=0)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{generalized logrank test}
%\keyword{score test}
