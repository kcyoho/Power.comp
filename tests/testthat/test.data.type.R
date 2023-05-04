context("Testing my_function()")

test_that("data_type returns the correct format", {
  # Set up
  x <- 1:10

  # Test
  result <- data_type("normal", 5)

  # Check
  expect_equal(ncol(result), 3)
})
