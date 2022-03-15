root <- system.file("extdata", "sim-example", package = "brachypoder")

test_that("Read one variable into a tibble", {

  data <- read_data(root, variables = "time")
  expect_equal(ncol(data), 1)
  expect_true("tbl" %in% class(data))

})

test_that("Read two variables", {

  data <- read_data(root, variables = c("time", "popsize"))
  expect_equal(ncol(data), 2)

})

test_that("Read two variables and split one", {

  data <- read_data(root, variables = c("time", "patchsizes"), ncols = c(1, 10))
  expect_equal(ncol(data), 11)

})

test_that("Read multiple variables and split some", {

  variables <- c("time", "popsize", "demesizes", "patchsizes")
  data <- read_data(root, variables = variables, ncols = c(1, 1, 5, 10))
  expect_equal(ncol(data), 17)

})

test_that("Read multiple variables and duplicate one", {

  variables <- c("time", "demesizes")
  data <- read_data(root, variables = variables, ncols = c(-5, 1))
  expect_equal(ncol(data), 2)

})

test_that("Read multiple variables and duplicate several", {

  variables <- c("time", "popsize", "demesizes")
  data <- read_data(root, variables = variables, ncols = c(-5, -5, 1))
  expect_equal(ncol(data), 3)

})

test_that("Duplicate different elements different numbers of times", {

  n_time_points <- length(read_binary(paste0(root, "/time.dat")))
  ncols <- list(rep(2, n_time_points))
  data <- read_data(root, "time", ncols = ncols)
  expect_equal(nrow(data), 2 * n_time_points)

})
