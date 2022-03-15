root <- system.file("extdata", "sim-example", package = "brachypoder")

test_that("Read binary data into a vector", {

  data <- read_binary(paste0(root, "/time.dat"))
  expect_true(length(data) > 0)
  expect_true(is.numeric(data))

})

