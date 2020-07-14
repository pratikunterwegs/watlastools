#' Get time between patches.
#'
#' @param data Dataframe containing at least the variables indicating patch
#' start and end time.
#' @param t_end The departure time from a hypothetical first patch.
#' @param t_start The arrival time at a hypothetical second patch.
#'
#' @return A vector of times in seconds between patches.
#' @export
wat_time_bw_patches <- function(data,
                                t_end = "time_end",
                                t_start = "time_start") {

  # check the patches for time
  watlastools:::wat_check_data(data,
                               names_expected = c(t_end, t_start))

  # order by time start
  data.table::setorderv(data, t_start)

  # handle good data case
  if (nrow(data) > 1) {
     # get t2 - t1
     time1 <- data[[t_end]][seq_len(nrow(data) - 1)]
     time2 <- data[[t_start]][-1]

     time_between <- c(NA, time2 - time1)
  } else if (nrow(data) == 1) {
     time_between <- NA_complex_
  } else {
     time_between <- NA_complex_
  }
  

  assertthat::assert_that(length(time_between) == nrow(data),
  msg = "time_bw_patches: times between don't match number of patches")

  return(time_between)

}