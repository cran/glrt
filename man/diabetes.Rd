\name{diabetes}
\alias{diabetes}
\docType{data}
\title{
Interval-censored data for time from onset of diabetes to the onset of diabetic nephronpathy
}
\description{
The \code{diabetes} data frame is based on a study conducted at the Steno Memorial Hospital in Denmark from 1933-1984. The data involve 731 patients (454 males and 277 females) and have 595 exact, 136 interval-censored, and no right-censored observations.  
}
\usage{data(diabetes)}
\format{
  A data frame with 731 observations on the following 3 variables.
  \describe{
    \item{\code{left}}{left end point of an censoring interval}
    \item{\code{right}}{left end point of an censoring interval}
    \item{\code{gender}}{gender indicator (1 for male and 0 for female)}
  }
}
\details{
The survival time is time from onset of diabetes to the onset of diabetic nephronpathy, a major complication of Type I diabetes. 
}
\source{
Kindly provided by Professor P. K. Andersen.
}
\references{
X. Zhao, Q. Zhao, J. Sun, Q. and J. S. Kim (2008), "Generalized Log-rank Tests for Partly Interval-Censored Failure Time Data", Biometrical Journal, 50 (3): 375-385.
}
\examples{
data(diabetes)
}
