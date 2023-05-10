\name{data_type}
\alias{data_type}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Data generation for block design}
\description{The data_type() function is used to take set distribution type and parameter values and return a data frame in a block design suitable for statistical analysis.}

\usage{
data_type(type, effect, N=4, Mean1= NULL, Mean2= NULL,
Mean3= NULL, SD = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{type}{The desired distribution type for the generated data. Types include normal, gumbel, or cauchy. Data type must be in quotations to be accepted.}
  \item{effect}{The numeric difference between the treatment groups. This function is set to have 3 treatments in 4 blocks. Effect sets the numeric difference in means between the treatments. The function is set to have the mean of treatment group 2 set to 0, with the means of groups 1 and 3 correspondingly set to the positive and negative values of the effect.}
  \item{N}{N is the sample size of the treatment groups. It is set to a default of 4.}
  \item{Mean1, Mean2, Mean3}{The means of the treatment groups. If Mean1 is set to an number, the function will use the set means for the treatment groups instead of using the effect. The function cannot have both a set Mean1 and a set effect size.}
  \item{SD}{The standard deviation of all the groups. It is set to a default of 1.}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
data_type returns data frame. The output has the following properties:
  \item{}{The data frame has column headers of “response”, “treatment” and “block”}
  \item{}{The data frame has 3N rows: data from the three treatment groups is all in one row.}
  \item{}{The column “response” contains the generated data}
  \item{}{The column “treatment” contains the treatment group: trt1, trt2, and trt3}
  \item{}{The column “block” contains the block that the data was assigned: 1, 2, 3, or 4.}

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Anna Hawse
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data_type("normal", 5)
data_type("cauchy", Mean1 = 0, Mean2 = 50, Mean3 = 100)
data_type("gumbel", effect = 10, N = 10, SD = 5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
