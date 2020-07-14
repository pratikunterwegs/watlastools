
#' Calculate instantaenous speed.
#'
#' @param data A dataframe or similar which must have the columns
#' specified by \code{x}, \code{y}, and \code{time}.
#' @param x The x coordinate.
#' @param y The y coordinate.
#' @param time The timestamp in seconds since the UNIX epoch.
#'
#' @return A vector of numerics representing speed.
#' The first position is assigned a speed of NA.
#' @export
wat_get_speed <- function(data,
                          x = "x",
                          y = "y",
                          time = "time") {

  wat_check_data(data,
                 names_expected = c(x, y, time))

  # set order in time
  data.table::setorder(data, time)

  # get distance
  distance <- watlastools::wat_simple_dist(data,
                                           x, y)

  # get time
  time <- c(NA, diff(data[[time]]))

  # simple speed
  speed <- distance / time

  return(speed)

}

#' Get the angle (relative bearing) between points.
#'
#' @param data A dataframe or similar which must have the columns
#' specified by \code{x}, \code{y}, and \code{time}.
#' @param x The x coordinate.
#' @param y The y coordinate.
#' @param time The timestamp in seconds since the UNIX epoch.
#' @return A vector of relative bearings in degrees. Negative degrees indicate 'left' turns.
#' @export
wat_get_angle <- function(data,
                          x = "x",
                          y = "y",
                          time = "time") {

  # check for column names
  wat_check_data(data,
                 names_expected = c(x, y, time))

  # set order in time
  if (!is.data.table(data)) {
    setDT(data)
  }
  setorderv(data, time)

  # handle good data case
  if (nrow(data) > 1) {
    x1 <- data[[x]][seq_len(nrow(data) - 1)]
    x2 <- data[[x]][-1]

    y1 <- data[[y]][seq_len(nrow(data) - 1)]
    y2 <- data[[y]][-1]

    # get angle in degrees
    angle <- c(NA, atan2(y1 - y2, x1 - x2)) * 180 / pi
  } else if (nrow(data) == 1) {
    angle <- NA_complex_
  }
  return(angle)
}

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
  wat_check_data(data,
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

#' Count data reflections.
#'
#' @param data The data in which to count so-called reflections.
#' @param speed_cutoff The speed cutoff in metres per second by which to
#' determine whether positions are reflected or not.
#' @param x The X coordinate.
#' @param y The Y coordinate.
#' @param time The timestamp, preferably in seconds since UNIX epoch.
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
  wat_check_data(data,
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

#' Fix extreme speeds and reflections.
#'
#' @param data A dataframe or similar, containing the columns \code{x},
#' \code{y}, and \code{time}.
#' @param speed_cutoff A speed cutoff in metres per second, above which to
#' remove both point locations as well as reflections.
#' @param x The X coordinate.
#' @param y The Y coordinate.
#' @param time The timestamp, preferably in seconds since the UNIX epoch.
#'
#' @return A dataframe with extreme speeds and reflections removed.
#' @export
wat_fix_speeds <- function(data,
                           x = "x",
                           y = "y",
                           time = "time",
                           speed_cutoff) {

  # check the data
  wat_check_data(data,
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
