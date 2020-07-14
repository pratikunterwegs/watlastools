#' Count data reflections.
#'
#' @param data The data in which to count so-called reflections.
#' @param speed_cutoff The speed cutoff in metres per second by which to
#' determine whether positions are reflected or not.
#'
#' @return A named list with two elements \code{n_reflections} and
#' \code{p_reflected}, the first indicating how many times there are
#' reflecions in the data, and the second indicating the proportion of fixes
#' that have been reflected.
#'
#' @export
wat_count_reflections <- function(data,
                                  x = "x",
                                  y = "y",
                                  time = "time",
                                  speed_cutoff = 0.5) {

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

  # get max value
  n_reflections <- length(unique(reflections[reflections %% 2 == 0]))

  p_reflected <- nrow(data[reflections %% 2 == 0, ]) / nrow(data)

  return(list(n_reflections = n_reflections,
              p_reflected = p_reflected))

}