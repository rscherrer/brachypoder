test_that("Duplicate with indices", {

  output <- index_duplicate(c("x", "y", "z"), n = 1:3)
  expect_equal(output, c("x", "y1", "y2", "z1", "z2", "z3"))

})
