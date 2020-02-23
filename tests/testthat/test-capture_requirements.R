context("capture_requirements")


testthat::test_that("errors", {
  testthat::expect_error(
    capture_requirements(write = "TRUE"),
    "The argument 'write' accepts only boolean, TRUE or FALSE"
  )
  testthat::expect_error(
    capture_requirements(write = "FALSE"),
    "The argument 'write' accepts only boolean, TRUE or FALSE"
  )
  testthat::expect_error(
    capture_requirements(write = "lalala"),
    "The argument 'write' accepts only boolean, TRUE or FALSE"
  )
  testthat::expect_error(
    capture_requirements(write = 1),
    "The argument 'write' accepts only boolean, TRUE or FALSE"
  )
})
