\name{cmv}
\alias{cmv}
\docType{data}
\title{
Interval-censored data for times to cytomegalovirus (CMV) shedding in blood and urine
}
\description{
The \code{cmv} data frame is based on an AIDS clinic trial on HIV-infected individuals. The data involve 204 patients who are classified into two groups based on whether patient's CD4 count is below 75 cells per \eqn{\mu l} or not. Among the 204 patients,  111 of them had a CD4 count below 75. 
}
\usage{
data(cmv)
}
\format{
  A data frame with 204 observations on the following 6 variables.
  \describe{
    \item{\code{patient}}{patient id}
    \item{\code{lb}}{left end point of an censoring interval for time to CMV shedding in blood}
    \item{\code{rb}}{right end point of an censoring interval for time to CMV shedding in blood}
    \item{\code{lu}}{left end point of an censoring interval for time to CMV shedding in urine}
    \item{\code{ru}}{right end point of an censoring interval for time to CMV shedding in urine}
    \item{\code{cd4ind}}{group indicator if patient's baseline CD4 count is below 75 (1 for below 75 and 0 otherwise)}
  }
}
\details{
The survival times are time to the presence of CMV in blood and ullrine, respectively. The censoring intervals are in the format of \eqn{(l_b, r_b]} and \eqn{(l_u, r_u]}. For CMV in blood, there are 7 left-censored, 23 interval-censored, and 174 right-censored, but no exact observations. For CMV in urine, there are 49 left-censored, 67 interval-censored, and 88 right-censored, but no exact observations.  
}
\source{
J. Sun (2006), "The Statistical Analysis of Interval-censored Failure Time Data", Springer.
}
\references{
W. B. Goggins and D. M. Finkelstein (2000), "A proportional hazards model for multivariate interval-censored failure time data", Biometrics, 56: 940-943.
}
%\examples{
%data(cmv)
%}
