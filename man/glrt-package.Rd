\name{glrt-package}
\alias{glrt-package}
\alias{glrt}
\docType{package}
\title{Generalized logrank tests for interval-censored failure time data}
\description{Functions to conduct three nonparametric generalized logrank tests and a score test under a proportional hazards model}
\details{
\tabular{ll}{
Package: \tab glrt\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2010-09-15\cr
License: \tab GPL (>=2)\cr
LazyLoad: \tab yes\cr
}
The main function is \code{\link{gLRT}}, in which a method is selected. Depending on the method specified, one of the functions (\code{\link{gLRT1}}, \code{\link{gLRT2}}, \code{\link{gLRT3}}, \code{\link{ScoreTest}}) is called to perform a generalized logrank test or a scor test. Before computing the test statistic in each test, function  \code{\link{ModifiedEMICM}} is called to obtain an NPMLE of the common survival function under the null hypothesis that all treatments have the same survival function.
}
\author{
Qiang Zhao and Jianguo Sun

Maintainer: Qiang Zhao <qiang.zhao@txstate.edu>
}
\references{
Q. Zhao and J. Sun (2004), "Generalized Log-rank Test for Mixed-Censored Failure Time Data", Statistics in Medicine, 23: 1621-1629.

J. Sun, Q. Zhao, and X. Zhao (2005), "Generalized Log-rank Test for Interval-Censored Data", Scandinavian Journal of Statistics, 32: 45-57.

X. Zhao, Q. Zhao, J. Sun, Q. and J. S. Kim (2008), "Generalized Log-rank Tests for Partly Interval-Censored Failure Time Data", Biometrical Journal, 50 (3): 375-385.

Finkelstein, DM (1986), "A Proportional Hazards Model for Interval-censored Failure Time Data", Biometrics, 42: 845-854.
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
