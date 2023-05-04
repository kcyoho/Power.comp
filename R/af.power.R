#Function 3: A wrapper function that runs Function 1 and Function 2 a large number of times for a sequence of sample sizes and produces a plot with power curves for the ANOVA and Friedman's test versions, comparing the two tests.

#install.packages(c("ggplot2", "cowplot","tidyr"))

library(tidyr)
library(ggplot2)
library(cowplot)

power.comp <- function(dist, n_sims = 25L, effect, sd = 10, alpha = 0.05, print.power = FALSE){
  #some error messages for input
  if (n_sims <= 0) { stop("Input must be greater than 0")}
  if (!is.integer(n_sims)) {stop("Input must be an integer")}
  if (!is.numeric(effect)) {stop("Input must be numeric")}

  power <- as.data.frame (matrix(0, nrow = length(effect), ncol =3)) #creates an empty matrix to store power info
  colnames(power) <- c("Effect size", "ANOVA power", "Friedman's power")

  for (sims in 1:n_sims) {# the "sims" loop repeats the data generation and testing process for each sample size n_sims number of times
    count = 0 #used in the i loop -- put inside sims loop so that count will reset after each iteration of the i loop

    for(i in seq_along(effect)){ #the "i" loop will run through all provided effect sizes one time, tracking whether or not the tests rejected the null
      count <- count + 1 # tracks which element of the effect object the loop is running -- passed along to track the row in the dataframe "power"
      X <- data_type(type = dist, effect = i, SD = sd)
      fa.comp <- fried.anov.comp(X, alpha)
      power[count,1] <- effect[count] # assigns effect size to column 1
      power[count,2] <- if (fa.comp[3,1] == "REJECT"){power[count,2]+1} else {power[count,2]+0} #counts ANOVA rejections
      power[count,3] <- if (fa.comp[3,2] == "REJECT"){power[count,3] +1} else {power[count,3]+0} #counts Friedman rejections
    }
  }
  power[, 2:3] <- power[, 2:3]/n_sims #averages power for each sample size across simulations
  power =  pivot_longer(data = power, !`Effect size`,
                        names_to = "Test", values_to = "power") #adjusts the data frame to be tidy--better for ggplot
  #graphs data
  power_plot = ggplot(data = power, aes(x=`Effect size`, y= `power`, group = `Test`))+
    geom_smooth(se = FALSE, aes(color=`Test`))+
    theme_cowplot(font_size = 10)+
    labs(title = "Comparison of Power between ANOVA and Friedman's Tests")
  if (print.power == TRUE){return(list(power, power_plot))}
  else {return(power_plot)}
}
