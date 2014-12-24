\name{ScoreTest}
\alias{ScoreTest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Do a score test under the proportional hazards model for interval-censored data
}
\description{
The \code{ScoreTest} function conducts a \eqn{k}(>=2)-sample test for interval-censored survival data. The test is based on Finkelstein (1986). The null hypothesis is that all \eqn{k} survival functions of the failure time are the same, and the alternative hypothesis is that not all functions are the same. It is not referred as gLRT4 as in Zhao (2012).
}
\usage{
ScoreTest(A, k = 2, EMstep = TRUE, ICMstep = TRUE, tol = 1e-06, 
maxiter = 1000, inf = Inf)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{an \eqn{n} by 3 data matrix with the censoring interval of the format \eqn{(L, R]} in columns 1 & 2 and treatmentment indicator ranging from 0 to \eqn{k-1} in column 3. }
  \item{k}{number of treatments. The default is 2.}
  \item{EMstep}{a boolean variable indicating whether to take an EM step in the iteration when estimating the common distribution function. The default is TRUE.}
  \item{ICMstep}{a boolean variable indicating whether to take an ICM step in the iteration when estimating the common distribution function. The default is TRUE.}
  \item{tol}{the maximal \eqn{L_1} distance between successive estimates before stopping iteration when estimating the common distribution function. The default is 1.0e-6.}
  \item{maxiter}{the maximal number of iterations to perform before stopping when estimating the common distribution function. The default is 1000.}
  \item{inf}{value used in data for infinity. The default is Inf.}
}
\details{
Under the null hypothesis that \eqn{k} survival functions are the same, the NPMLE of the common survival function is computed by function \code{\link{ModifiedEMICM}}.

The chi-square test in \code{ScoreTest} has \eqn{k-1} degrees of freedom. 
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
Finkelstein, DM (1986), "A Proportional Hazards Model for Interval-censored Failure Time Data", Biometrics, 42: 845-854.

Q. Zhao (2012), "gLRT - A New R Package for Analyzing Interval-censored Survival Data", Interval-Censored Time-to-Event Data: Methods and Applications, CRC Press, 377-396.
}
\author{ Qiang Zhao and Jianguo Sun }

%%\note{
%%  ~~further notes~~
%%}

\seealso{
\code{\link{ModifiedEMICM}},  \code{\link{gLRT}},  \code{\link{gLRT1}},  \code{\link{gLRT2}},  \code{\link{gLRT3}}, \code{\link{gLRT4}}  
}
\examples{
data(cosmesis)
ScoreTest(cosmesis, inf=100)

data(diabetes)
ScoreTest(diabetes)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{generalized logrank test}
%\keyword{score test}
