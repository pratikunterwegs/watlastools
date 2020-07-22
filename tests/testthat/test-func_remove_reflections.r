context("removing reflections")
testthat::test_that("reflections are removed", {

  # make test positions
  x_good <- stats::runif(100)
  x_bad <- stats::runif(50) + 50

  y_good <- x_good + rev(x_good)
  y_bad <- x_bad + rev(x_bad)

  test_df <- data.table::data.table(x = c(x_good, x_bad, x_good),
                                    y = c(y_good, y_bad, y_good),
                                    time = seq_len(250) * 3)

  # get speeds
  test_df[, `:=`(speed = wat_get_speed(test_df),
                 angle = wat_turning_angle(test_df))]

  test_output <- wat_remove_reflections(test_df,
                                        reflection_speed_cutoff = 10,
                                        point_angle_cutoff = 5)

  # do tests
  # should return fewer elements than nrows in df
  testthat::expect_lte(nrow(test_output), nrow(test_df))

  # should remove points with x and y coordinates less than 5
  testthat::expect_lt(max(test_output$x), 5)

  # a test for change in x and y
  # make test positions
  x_test <- seq_len(250)

  y_test <- c(rep(0, 100), rep(20, 50), rep(0, 100)) + stats::runif(250)
  test_df <- data.table::data.table(x = x_test,
                                    y = y_test,
                                    time = seq_len(250) * 3)

  # get speeds
  test_df[, `:=`(speed = wat_get_speed(test_df),
                 angle = wat_turning_angle(test_df))]

  test_output <- wat_remove_reflections(test_df,
                                        reflection_speed_cutoff = 5,
                                        point_angle_cutoff = 5)

  # do tests
  # should return fewer elements than nrows in df
  testthat::expect_lte(nrow(test_output), nrow(test_df))

  # should remove points with x and y coordinates less than 5
  testthat::expect_lt(max(test_output$y), 5)

})
