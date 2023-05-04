# Function 1
#  Goal: Takes as input the distribution type (Normal, Cauchy, or Gumbel)
# and their parameter values and returns a data frame for the analysis.
# Please see rnorm, rcauchy, to generate data.

#load packages needed
#install.packages("evd")
library(evd) #for the 'rgumbel' function

#function where you say "data_type("distribution type", N, Mean1, Mean2, Mean3, SD)
#the distribution type must be in ""

data_type <- function(type, effect, N=4, Mean1= NULL, Mean2= NULL, Mean3= NULL,
                      SD = 1){
  if (is.null(Mean1)){
    Mean1 = effect
    Mean2= 0
    Mean3=-effect}
  if(type == "normal"){
    group1 = rnorm(n= N, mean= Mean1, sd = SD)
    group2 = rnorm(n= N, mean= Mean2, sd = SD)
    group3 = rnorm(n= N, mean= Mean3, sd = SD)}
  else if(type == "cauchy"){
    group1 = rcauchy(n= N, location = Mean1, scale = SD)
    group2 = rcauchy(n= N, location = Mean2, scale = SD)
    group3 = rcauchy(n= N, location = Mean3, scale = SD)}
  else if(type == "gumbel"){
    group1 = rgumbel(n= N, loc = Mean1, scale = SD)
    group2 = rgumbel(n= N, loc = Mean2, scale = SD)
    group3 = rgumbel(n= N, loc = Mean3, scale = SD)}
  X = matrix(c(group1, group2, group3),
             nrow=N,
             byrow = FALSE,
             dimnames = list(1:N,
                             c("Trt1","Trt2","Trt3")))
  df <-data.frame(response = c(X[,1], X[,2], X[,3]), treatment = factor(c(rep("trt1", times = N),
                                                                          rep("trt2", times = N),
                                                                          rep("trt3", times = N))),
                  block = factor(rep(1:N)))
  return(df)
}
