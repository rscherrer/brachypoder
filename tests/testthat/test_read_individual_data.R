root <- system.file("extdata", "sim-example", package = "brachypoder")

test_that("Read individual data", {

  data <- read_individual_data(root)
  expect_equal(ncol(data), 6)
  expect_true(all(names(data) == c("time", "deme", "patch", "x", "y", "z")))

})
