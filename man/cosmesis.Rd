\name{cosmesis}
\alias{cosmesis}
\docType{data}
\title{
Interval-censored data for time taken until cosmetic deterioration of breast cosmesis
}
\description{
The cosmesis data frame has 95 rows and 3 columns. 
}
\usage{
data(cosmesis)
}
\format{
  A data frame with the following 3 variables.
  \describe{
    \item{\code{L}}{left end point of the cosmetic deterioration interval}
    \item{\code{R}}{right end point of the cosmetic deterioration interval}
    \item{\code{Trt}}{treatment indicator. It is zero for those that received radiotherapy}
  }
}
%\details{
%}
\source{
Borrowed from package \code{Icens} by Alain Vandal and Robert Gentleman.
}
\references{
A semiparametric model for regression analysis of interval-censored failure time data, D. M. Finkelstein and R. A. Wolfe, 1985, Biometrics. 
}
%\examples{
%data(cosmesis)
%}
