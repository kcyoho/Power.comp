\name{fried.anov.comp}
\alias{fried.anov.comp}
\title{Comparison of the Friedman Rank Sum and ANOVA tests}
\description{
fried.anova.comp performs ANOVA and Friedman rank sum tests on unreplicated  complete block data, and returns the test statistic, p-value, and whether or not the null hypothesis is rejected based on a provided alpha.
}
\usage{fried.anov.comp(X, response = NULL, treatment = NULL, block = NULL, alpha = 0.05)}

\arguments{
  \item{X}{A data frame or matrix. Data must contain separate columns for the "response", "treatment", and "block" variables, and must represent an unreplicated complete block design.}

\item{response}{The column of the matrix or data frame X containing the dependent variable. Must be numeric. Not required if "response" is already a column name in the data set. }

\item{treatment}{The column of the matrix or data frame X containing the treatment variable. Not required if "treatment" is already a column name in the data set.}

\item{block}{The column of the matrix or data frame X containing the block variable. Not required if "block" is already a column name in the data set.}

\item{alpha}{A numeric object giving the desired significance level of the test. Default = 0.05}
}
\details{
fried.anov.comp is intended to demonstrate how the traditional ANOVA test compares to the Friedman Rank Sum test a nonparametric alternative for unreplicated block designs), particularly when the ANOVA assumption of normality is violated.
}
\value{Returns a matrix containing the test statistic, p-value, and decision of the ANOVA and Friedman's Rank Sum tests.}

\author{Kelie Yoho}

\seealso{
\code{\link{data_type}}
\code{\link{power.comp}} }

\examples{
## Randomly generated data from the cauchy distribution for an unreplicated block design.
## k = 4 treatments and b = 6 blocks.
## Scale is assumed to be the same across all treatments.
X <- data.frame(response = c(rcauchy(6,0,3.25), rcauchy(6,8,3.25),
                            rcauchy(6,5,3.25), rcauchy(6,11,3.25)),
                      treatment = factor(c(rep("trt1", times = 6),
                                        rep("trt2", times = 6),
                                        rep("trt3", times = 6),
                                        rep("trt4", times = 6))),
                      block = factor(rep(1:6)))

X

fried.anov.comp(X, alpha = 0.05 )

## the data_type function can be used to generate data for k= 3 treatments

sim.dat <- data_type("gumbel", effect = 7, SD = 5)

sim.dat

fried.anov.comp(sim.dat, alpha = 0.01)

## Column names may be other than "response", "treatment", and "block"

O.ex <- data.frame(y = c(rnorm(5,0,2), rnorm(5,3,2), rnorm(5,5,2)),
                trt = factor(c(rep("trt1", times = 5),
                            rep("trt2", times = 5),
                            rep("trt3", times = 5))),
                bl = factor(rep(1:5)))

O.ex

fried.anov.comp(O.ex, response = O.ex[,1], treatment = O.ex[,2], block = O.ex[,3])

fried.anov.comp(O.ex, response = O.ex$y, treatment = O.ex$trt, block = O.ex$bl)
}
