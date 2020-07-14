
#' Fix extreme speeds and reflections.
#'
#' @param data A dataframe or similar, containing the columns \code{x},
#' \code{y}, and \code{time}.
#' @param speed_cutoff A speed cutoff in metres per second, above which to
#' remove both point locations as well as reflections.
#'
#' @return
#' @export
wat_fix_speeds <- function(data,
                           x = "x",
                           y = "y",
                           time = "time",
                           speed_cutoff) {

  # check the data
  watlastools:::wat_check_data(data,
                               names_expected = c(x, y, time))

  # set order in time
  data.table::setorder(data, time)

  # get speeds
  speeds <- wat_get_speed(data)

  # remove extreme speeds
  data <- data[speeds < speed_cutoff, ]

  # deal with reflections
  # start with recalculating speeds
  speeds <- wat_get_speed(data)

  # set the first position to Inf
  speeds[1] <- Inf

  # reflections are cumulative sums of speed changes
  reflections <- cumsum(speeds > speed_cutoff)

  # remove reflections divisible by 2
  data <- data[reflections %% 2 != 0, ]

  return(data)
}
