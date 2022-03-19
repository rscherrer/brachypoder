root <- system.file("extdata", "sim-example", package = "brachypoder")

test_that("", {

  pars <- read_parameters(root)
  expect_true(is.list(pars))
  expect_true("pgood" %in% names(pars))

})
