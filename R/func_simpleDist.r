#' A fast way to calculate euclidean distances between successive points.
#'
#' @param x A column name in a data.frame object that contains the numeric
#' X or longitude coordinate for position data.
#' @param y A column name in a data.frame object that contains the numeric
#' Y or latitude coordinate for position data.
#' @param data A dataframe object of or extending the class data.frame,
#' which must contain at least two coordinate columns for the X and Y
#' coordinates.
#' @param time Time column to order by.
#'
#' @return Returns a vector of distances between consecutive points.
#' @export
#'
wat_simple_dist <- function(data,
                            x = "x",
                            y = "y",
                            time = "time") {

  #check for basic assumptions
  assertthat::assert_that(is.data.frame(data),
                          is.character(x),
                          is.character(y),
                          is.character(time),
                          msg = "simpleDist: some data assumptions are not met")

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

    # get dist
    dist <- c(NA, sqrt(((x2 - x1) ^ 2) + ((y2 - y1) ^ 2)))
  } else if (nrow(data) == 1) {
    dist <- NA_real_
  }
  return(dist)
}

#### a function for patch end to patch start distances ####
