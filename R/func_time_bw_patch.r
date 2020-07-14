#' Get time between patches.
#'
#' @param data Dataframe containing at least the variables indicating patch
#' start and end time.
#' @param t1 The departure time from a hypothetical first patch.
#' @param t2 The arrival time at a hypothetical second patch.
#'
#' @return A vector of times in seconds between patches.
#' @export
wat_time_bw_patches <- function(data,
                                t1 = "time_end",
                                t2 = "time_start") {

  # check the patches for time
  watlastools:::wat_check_data(data,
                               names_expected = c(t1, t2))

  # check there is more than one patch
  assertthat::assert_that(nrow(data) > 1,
                          msg = "time_bw_patches: not enough patches")

  # order by time start
  data.table::setorder(data, t1)

  # get t2 - t1
  t2 <- data[[t2]][seq_len(nrow(data) - 1)]
  t1 <- data[[t2]][-1]

  time_between <- c(NA, t2 - t1)

  assertthat::assert_that(length(time_between) == nrow(data),
  msg = "time_bw_patches: times between don't match number of patches")

  return(time_between)

}