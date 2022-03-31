root <- system.file("extdata", "sim-example", package = "brachypoder")

test_that("Read trait mean data", {

  data <- read_trait_mean_data(root)
  expect_equal(ncol(data), 6)
  expect_true(all(names(data) == c("time", "deme", "patch", "x", "y", "z")))

})
