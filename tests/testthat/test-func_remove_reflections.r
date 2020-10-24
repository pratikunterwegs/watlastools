context("removing reflections")
testthat::test_that("reflections are removed", {

# make test positions
  test_data <- data.table::fread("../testdata/data_errors.csv")

  # get speeds
  test_data[, `:=`(
    in_speed = watlastools::wat_get_speed(test_data),
    angle = watlastools::wat_turning_angle(test_data)
  )]

  # remove NA speeds
  test_data <- stats::na.omit(test_data, cols = c("in_speed", "angle"))

  # remove outliers
  test_data <- test_data[in_speed < 0.024, ]

  # remove reflections
  test_output <- watlastools::wat_remove_reflections(test_data,
    point_angle_cutoff = 10,
    reflection_speed_cutoff = 0.024
  )

  # do tests
  # should return fewer elements than nrows in df
  testthat::expect_lte(nrow(test_output), nrow(test_data))

  # no extreme speeds should remain
  testthat::expect_lte(max(test_output$speed, na.rm = TRUE), 0.024)

  # a test for change in x and y
  # make test positions
  x_test <- seq_len(250)

  y_test <- c(rep(0, 100), rep(20, 50), rep(0, 100)) + stats::runif(250)
  test_data <- data.table::data.table(
    x = x_test,
    y = y_test,
    time = seq_len(250) * 3
  )

  # get speeds
  test_data[, `:=`(
    speed = watlastools::wat_get_speed(test_data),
    angle = watlastools::wat_turning_angle(test_data)
  )]

  test_output <- watlastools::wat_remove_reflections(test_data,
    reflection_speed_cutoff = 5,
    point_angle_cutoff = 5
  )

  # do tests
  # should return fewer elements than nrows in df
  testthat::expect_lte(nrow(test_output), nrow(test_data))

  # should remove points with x and y coordinates less than 5
  testthat::expect_lt(max(test_output$y), 5)
})
