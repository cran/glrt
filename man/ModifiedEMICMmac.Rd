\name{ModifiedEMICMmac}
\alias{ModifiedEMICMmac}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
An interval function in package "Icens"
}
\description{
A modified version of \code{EMICMmac} in package \code{Icens} to avoid the error when calling function \code{EMICM} when specifying ICMstep = FALSE. The error is caused by changing the type of an undeclared vector \code{weights}. 
}
%\usage{
%ModifiedEMICMmac(A, EMstep = TRUE, ICMstep = TRUE, keepiter = FALSE, tol = 1e-07, tolbis = 1e-07, maxiter = 1000)
%}
%- maybe also 'usage' for other objects documented here.
%\arguments{
%  \item{A}{
%%     ~~Describe \code{A} here~~
%}
%  \item{EMstep}{
%%     ~~Describe \code{EMstep} here~~
%}
%  \item{ICMstep}{
%%     ~~Describe \code{ICMstep} here~~
%}
%  \item{keepiter}{
%%     ~~Describe \code{keepiter} here~~
%}
%  \item{tol}{
%%     ~~Describe \code{tol} here~~
%}
%  \item{tolbis}{
%%     ~~Describe \code{tolbis} here~~
%}
%  \item{maxiter}{
%%     ~~Describe \code{maxiter} here~~
%}
%}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\author{Qiang Zhao and Jianguo Sun
}
\note{
It is used in \code{\link{ModifiedEMICM}} and not to be called by the user.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{ModifiedEMICM}}
}
%\examples{
%}
