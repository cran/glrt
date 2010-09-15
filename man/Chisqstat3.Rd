\name{Chisqstat3}
\alias{Chisqstat3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute the chi-square test statistic and degrees of freedom for test gLRT3
}
\description{
This function computes the chi-square test statistic and degrees of freedom for test \code{gLRT3}. The degree of freedom depends on whether there are exact observations and proportions of exact observations in each treatment.
}
\usage{
Chisqstat3(U, V, counts)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{U}{
k-vector of the test statistic in \code{gLRT3}, where k is number of treatments.
}
  \item{V}{
estimated k by k covariance matrix of \code{U} 
}
  \item{counts}{
 a k by 2 matrix containing number of exact and non-exact observations for each treatment
}
}
\value{
  \item{chisq }{chi-square test statistic}
  \item{df}{degrees of freedom used. }
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

%\examples{
%}
