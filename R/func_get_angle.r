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
    angle <- NA_real_
  }
  return(angle)
}