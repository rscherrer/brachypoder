root <- system.file("extdata", "sim-example", package = "brachypoder")

test_that("Read patch size data", {

  data <- read_patch_size_data(root)
  expect_equal(ncol(data), 4)
  expect_true(all(names(data) == c("time", "deme", "patch", "n")))

})
