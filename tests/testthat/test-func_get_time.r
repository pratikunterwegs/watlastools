context("check time between patches\n")
testthat::test_that("time bw patches works", {

  # make some test data
  testdata <- data.table::data.table(
    t1 = c(1, 2, 5, 7),
    t2 = c(2, 4, 6, 10)
  )

  # get time between patches
  time_bw_patch <- watlastools::wat_time_bw_patches(testdata,
    t_start = "t1",
    t_end = "t2"
  )

  # do test for expected values
  testthat::expect_equal(time_bw_patch, c(NA, 0, 1, 1))

  # do test if testdata is too few
  testdata_small <- data.table::data.table(
    t1 = 1,
    t2 = 2
  )
  time_bw_small <- watlastools::wat_time_bw_patches(testdata_small,
    t_start = "t1",
    t_end = "t2"
  )
  testthat::expect_equal(time_bw_small, NA_real_)
})
