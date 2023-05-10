\name{power.comp}

\alias{power.comp}

\title{Friedman Rank Sum and ANOVA Power Comparison for unreplicated RCB designs}

\description{Graphs the relative powers of ANOVA and Friedman tests for an unreplicated randomized complete block design with k = 3 treatments and b = 4 blocks.}

\usage{power.comp(dist, n_sims= 25L, effect, sd =  10, alpha = 0.05, print.power = FALSE)}

\arguments{
\item{dist}{The data generating distribution. Options are "normal", "cauchy", or "gumbel".}

\item{n_sims}{An integer object representing the number of times to repeat the data generation process for each effect size. Larger values are associated with more precise estimates of power, but require more computing power. Default = 25}

\item{effect}{A numeric vector containing the range of treatment effect values from which to generate data. Corresponds to the domain of the output graph.}

\item{sd}{A numeric object giving the desired standard deviation (for normal) or scale (for gumbel and cauchy) of the data generating distributions. sd is assumed to be consistent across all treatment groups. Default = 10}

\item{alpha}{A numeric object giving the desired significance level of the test. Default = 0.05}

\item{print.power}{Optional command to print the dataframe of power values from which the graph is produced. Takes values TRUE and FALSE. Default = FALSE}}

\details{power.comp simulates new data for each effect size and runs Friedman and ANOVA tests on the data; this is repeated n_sims number of times. It then sums the total number of null hypothesis rejections for a given effect size and divides by n_sims to calculate power.}}

\value{Returns a graph displaying a trendline of the average powers (y-axis) of the ANOVA and Friedman's Rank Sum tests across the specified range of effect sizes (x-axis), assuming the provided distribution, standard deviation, and significance level.}

\references{
Zimmerman, D. W., & Zumbo, B. D. (1993). Relative Power of the Wilcoxon Test, the Friedman Test, and Repeated-Measures ANOVA on Ranks. The Journal of Experimental Education, 62(1), 75–86. http://www.jstor.org/stable/20152399}

\author{Kelie Yoho, Anna Hawse}

\seealso{
\code{\link{data_type}}
\code{\link{fried.anov.comp}} }

\examples{
## Data simulation and hypothesis testing are included within the function.

power.comp(dist = "cauchy", n_sims = 50L, effect = 1:30, print.power = TRUE)

power.comp("normal", effect = seq(1,20,by = 2), sd = 4, alpha = 0.01)
}

