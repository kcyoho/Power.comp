#Function 2 : Takes as input the data frame produced by Function 1 and returns whether we reject the null hypothesis or not by ANOVA and Friedman's test. k = 3 treatments

fried.anov.comp <- function(X, response = NULL, treatment = NULL, block = NULL, alpha = 0.05) {
  #some error messages for input
  if (alpha < 0) { stop("Input must be positive")}
  if (!is.numeric(alpha)) { stop("Input must be numeric")}

  #runs the tests
  fried <-friedman.test(formula = response ~ treatment| block, data = X)
  anov <-anova(lm(response ~ block + treatment, data = X))

  #creates a matrix filled with results for return
  results <- matrix(nrow= 3, ncol=2,
                    c(anov[2,4], anov[2,5],  if (anov[2,5] < alpha){"REJECT"} else {"FAIL TO REJECT"},
                      fried$statistic, fried$p.value, if (fried$p.value < alpha){"REJECT"} else {"FAIL TO REJECT"}))
  colnames(results) <- c("ANOVA", "Friedman's")
  rownames(results)<- c("statistic", "p-value", "Conclusion")
  return(results)
}

