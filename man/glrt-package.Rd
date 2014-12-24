\name{glrt-package}
\alias{glrt-package}
\alias{glrt}
\docType{package}
\title{Generalized logrank tests for interval-censored failure time data}
\description{Functions to conduct four nonparametric generalized logrank tests and a score test under a proportional hazards model}
\details{
\tabular{ll}{
Package: \tab glrt\cr
Type: \tab Package\cr
Version: \tab 2.0\cr
Date: \tab 2014-12-24\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
The main function is \code{\link{gLRT}}, in which a method is selected. Depending on the method specified, one of the five functions (\code{\link{gLRT1}}, \code{\link{gLRT2}}, \code{\link{gLRT3}}, \code{\link{gLRT4}}, \code{\link{ScoreTest}}) is called to perform a generalized logrank test or a scor test. Before computing the test statistic in each test, function  \code{\link{ModifiedEMICM}} is called to compute an NPMLE of the common survival function under the null hypothesis that all treatments have the same survival function except for in \code{gLRT4}, where \code{ModifiedEMICM} is used to estimate the survival function of each treatment group.
}
\author{
Qiang Zhao and Jianguo Sun

Maintainer: Qiang Zhao <qiang.zhao@txstate.edu>
}
\references{
Q. Zhao and J. Sun (2004), "Generalized Log-rank Test for Mixed-Censored Failure Time Data", Statistics in Medicine, 23: 1621-1629.

J. Sun, Q. Zhao, and X. Zhao (2005), "Generalized Log-rank Test for Interval-Censored Data", Scandinavian Journal of Statistics, 32: 45-57.

X. Zhao, Q. Zhao, J. Sun, Q. and J. S. Kim (2008), "Generalized Log-rank Tests for Partly Interval-Censored Failure Time Data", Biometrical Journal, 50 (3): 375-385.

X. Zhao, R. Duan, Q. Zhao, and J. Sun (2013), "A New Class of Generalized Log Rank Tests for Interval-censored Failure Time Data", Computational Statistics and Data Analysis. 60: 123-131.

Finkelstein, DM (1986), "A Proportional Hazards Model for Interval-censored Failure Time Data", Biometrics, 42: 845-854.

Q. Zhao (2012), "gLRT - A New R Package for Analyzing Interval-censored Survival Data", Interval-Censored Time-to-Event Data: Methods and Applications, CRC Press, 377-396.
}
\seealso{
\code{\link{gLRT}}, \code{\link{gLRT1}}, \code{\link{gLRT2}},  \code{\link{gLRT3}}, \code{\link{gLRT4}}, \code{\link{ScoreTest}}, \code{\link{ModifiedEMICM}} 
}

