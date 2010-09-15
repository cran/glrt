\name{ModifiedEMICM}
\alias{ModifiedEMICM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimate the distribution function using the hybrid EM-ICM approach
}
\description{
A modified version of function \code{\link{EMICM}} in package \code{Icens} by incorporating function \code{\link{Aintmap}} in package \code{interval}. By defult, the function provides an NPMLE for the distribution function of the survival time. 
}
\usage{
ModifiedEMICM(A, EMstep = TRUE, ICMstep = TRUE, keepiter = FALSE, 
tol = 1e-07, maxiter = 1000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{an n by 2 matrix with containing the end points of censoring intervals of the format (Li, Ri]. }
  \item{EMstep}{a boolean variable indicating whether to take an EM step in the iteration when estimating the common distribution function. The default is TRUE.}
  \item{ICMstep}{a boolean variable indicating whether to take an ICM step in the iteration when estimating the common distribution function. The default is TRUE.}
  \item{keepiter}{TRUE/FALSE determining whether to keep the iteration states. }
  \item{tol}{the maximal L1 distance between successive estimates before stopping iteration when estimating the common distribution function. The default is 1.0e-7.}
  \item{maxiter}{the maximal number of iterations to perform before stopping when estimating the common distribution function. The default is 1000.}
}
\details{
After incorporating function \code{Aintmap}, function \code{ModifiedEMICM} often produces \code{intmap} with smaller size than function \code{\link{EMICM}}, especially when exact observations (Li = Ri) exist. In addition, object \code{ppairs} is returned for later use in computing the test statistics in functions \code{\link{gLRT1}}, \code{\link{gLRT2}}, \code{\link{gLRT3}}, and \code{\link{ScoreTest}}. Also, a bug was identified in using \code{\link{EMICM}} when ICMstep=F is specified. The problem is fixed by calling \code{\link{ModifiedEMICMmac}}, a modified version of function \code{EMICMmac} from package \code{Icens}.

Either EM, ICM, or both steps can be taken in the estimation. When \code{ICMstep = FALSE}, the function computes a self-consistent estimate, the same results as obtained from function \code{icfit} in package \code{interval}. 
}
\value{
An object containing the following components:
 \item{pf}{Estimated probabilities}
 \item{sigma}{NPMLE/self-consistant estimate of the distribution function
}
 \item{weights}{the diagonal of the likelihood functions's second derivative}
 \item{lastchange}{a vector of differences between the last two iterations}
 \item{numiter}{number of iterations performed}
 \item{iter}{only present if \code{keepiter} is true; state of sigma during the iteration}
 \item{intmap}{the real representation associated with the probabilities reported in \code{pf}}
 \item{startend}{the indices for Li and Ri identifying the end points in \code{intmap} where a subject is at risk. }
}
\references{
Function \code{\link{EMICM}} by Alain Vandal and Robert Gentleman .

J. A. Wellner and Y. Zhan (1997), "A hybrid algorithm for computation of the nonparametric maximum likelihood estimator from censored data", JASA. 
}
\author{
Qiang Zhao and Jianguo Sun
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{EMICM}}, \code{\link{Aintmap}}, \code{\link{EMICMmac}}, \code{\link{ModifiedEMICMmac}}
}
\examples{
data(diabetes)
ModifiedEMICM(diabetes[,1:2])

data(cosmesis)
ModifiedEMICM(cosmesis[,1:2])
}
