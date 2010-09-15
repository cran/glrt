\name{gLRT}
\alias{gLRT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Do one of the generalized logrank tests or a score test for interval-censored data
}
\description{
The \code{gLRT} function conducts four k(>=2)-sample tests for interval-censored survival data. Three of them are different nonparametric generalized logrank tests, and the other is a score test under a proportional hazards model. They are two-sided tests. The null hypothesis is that all k survival functions of the failure time are identical, and the alternative hypothesis hypothesis is that not all survival functions are the same. This function calls one of functions ( \code{\link{gLRT1}},  \code{\link{gLRT2}},  \code{\link{gLRT3}},  \code{\link{ScoreTest}}) based on the method specified.  
}
\usage{
gLRT(A, k = 2, method = c("glrt1", "glrt2", "glrt3", "score"), 
M = 50, rho = 0, gamma = 0, EMstep = TRUE, ICMstep = TRUE, tol = 1e-07, 
maxiter = 1000, inf = Inf)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{an n by 3 data matrix with the censoring interval of the format (L, R] in columns 1 & 2 and treatmentment indicator ranging from 0 to k-1 in column 3. }
  \item{k}{number of treatments. The default is 2.}
  \item{method}{a character string specifying the test to be performed: "glrt1", "glrt2", "glrt3", and "score".}
  \item{M}{number of multiple imputations used in estimating the covariance in function  \code{\link{gLRT1}}. The default is 50. }
  \item{rho}{non-negative parameter of the link function used for calculating the test statistics in \code{\link{gLRT2}} and  \code{\link{gLRT3}}. The default is 0.}
  \item{gamma}{non-negative parameter of the link function used for calculating the test statistics in \code{\link{gLRT2}} and  \code{\link{gLRT3}}. The default is 0.}
  \item{EMstep}{a boolean variable indicating whether to take an EM step in the iteration when estimating the common distribution function. The default is TRUE.}
  \item{ICMstep}{a boolean variable indicating whether to take an ICM step in the iteration when estimating the common distribution function. The default is TRUE.}
  \item{tol}{the maximal L1 distance between successive estimates before stopping iteration when estimating the common distribution function. The default is 1.0e-7.}
  \item{maxiter}{the maximal number of iterations to perform before stopping when estimating the common distribution function. The default is 1000.}
  \item{inf}{value used in data for infinity. The default is Inf.}
}

\details{
Under the null hypothesis, the NPMLE of the common distribution function is computed by function \code{\link{ModifiedEMICM}}.When \code{method="glrt1"} is selected, \code{\link{gLRT1}}is called to perform the test proposed by Zhao and Sun (2004). When \code{method="glrt2"} is selected, \code{\link{gLRT2}}is called to perform the test proposed by Sun, Zhao, and Zhao (2005). When \code{method="glrt3"} is selected, \code{\link{gLRT3}}is called to perform the test proposed by Zhao, Zhao, Sun, and Kim (2008). When \code{method="score"} is selected, \code{\link{ScoreTest}}is called to perform a score test under a proportional hazards model proposed by Finkelstein (1986).   

Censoring interval for each observation take the form (Li, Ri]. For exact observations, Li = Ri.

Exact observations are not allowed in \code{\link{gLRT2}}. If no exactly observations exist, \code{\link{gLRT3}} reduces to \code{\link{gLRT2}} in terms of chi-square statistic and p-value. 

The link function used in \code{\link{gLRT2}} and  \code{\link{gLRT3}} is \eqn{\xi(x) = x log(x) x^\rho (1 - x)^\gamma. }
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

J. Sun, Q. Zhao, and X. Zhao (2005), "Generalized Log-rank Test for Interval-Censored Data", Scandinavian Journal of Statistics, 32: 45-57.

X. Zhao, Q. Zhao, J. Sun, Q. and J. S. Kim (2008), "Generalized Log-rank Tests for Partly Interval-Censored Failure Time Data", Biometrical Journal, 50 (3): 375-385.

Finkelstein, DM (1986), "A Proportional Hazards Model for Interval-censored Failure Time Data", Biometrics, 42: 845-854.
}
\author{ Qiang Zhao and Jianguo Sun }

%%\note{
%%  ~~further notes~~
%%}

\seealso{
\code{\link{gLRT1}},  \code{\link{gLRT2}},  \code{\link{gLRT3}},  \code{\link{ScoreTest}}  
}
\examples{
data(cosmesis)
gLRT(cosmesis, method="glrt1", M=20, inf=100)
gLRT(cosmesis, method="glrt2", rho=1, inf=100)

data(diabetes)
gLRT(diabetes, method="glrt3", gamma=1)
gLRT(diabetes, method="score")

# 3-sample test
data(cosmesis)
cosmesis[80:94, 3] = 2
gLRT(cosmesis, k=3, method="glrt3", rho=0, gamma=0, inf=100)
}
