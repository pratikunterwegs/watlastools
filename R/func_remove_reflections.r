#' Remove reflected positions.
#'
#' @author Pratik R. Gupte
#' @param data A dataframe or similar which has previously been cleaned.
#' @param x The name of the X coordinate column.
#' @param y The name of the Y coordinate column.
#' @param time The name of the timestamp column.
#' @param adjacency_speed The speed (in m/s) between two 'good' ends of a
#' reflection.
#' @param point_angle_cutoff The turning angle (in degrees) above which
#' high instantaneous speeds are considered an anomaly rather than fast transit.
#' @param reflection_speed_cutoff The speed (in m/s) above which an anomaly is
#' detected when combined with a high turning angle.
#'
#' @return A dataframe with reflections removed.
#' @export
wat_remove_reflections <- function(data,
                                   x = "x",
                                   y = "y",
                                   time = "time",
                                   adjacency_speed = 1,
                                   point_angle_cutoff = 45,
                                   reflection_speed_cutoff = 40) {

  setorderv(data, time)

  # get speed and angle
  data[, `:=`(speed = watlastools::wat_get_speed(data),
              angle = watlastools::wat_turning_angle(data))]

  # prepare a vector of rows to discard
  vec_discard <- integer()

  # identify the last point before an anomaly
  anchor_point <- which(data$speed >=
                          reflection_speed_cutoff &
                          data$angle >= point_angle_cutoff)[1] - 1

  # message
  message(glue::glue("first anchor at {anchor_point}"))

  while (anchor_point < nrow(data) - 1) {

    # all subsequent points are suspect
    suspect_point <- anchor_point + 1

    # set an anchor point
    x_anchor <- data[anchor_point, ][[x]]
    y_anchor <- data[anchor_point, ][[y]]
    time_anchor <- data[anchor_point, ][[time]]

    # get the speed between anchor and all subsequent points
    anchor_distance <- sqrt((x_anchor -
                               data[suspect_point:nrow(data), ][[x]])^2 +
                              (y_anchor -
                                 data[suspect_point:nrow(data), ][[y]])^2)
    anchor_tdiff <- data[suspect_point:nrow(data), ][[time]] - time_anchor
    anchor_speed <- anchor_distance / anchor_tdiff

    # identify where the reflection ends
    # the reflection ends with the last point to have an extreme speed
    reflection_end <- anchor_point + which(anchor_speed <=
                                             adjacency_speed)[1] + 1
    # when reflections do not end remove all subsequent data
    # this is more likely in smaller subsets
    if (is.na(reflection_end)) {
       reflection_end <- nrow(data)
    }

    # message ref end
    message(glue::glue("reflection ends {reflection_end}"))

    # identify rows to remove
    # may be excessive but works
    vec_discard <- c(vec_discard, seq(anchor_point, reflection_end))

    # set the next anchor
    next_anchor <- which(data$speed[reflection_end:nrow(data)] >=
                           reflection_speed_cutoff &
                           data$angle[reflection_end:nrow(data)] >=
                           point_angle_cutoff)[1] - 1

    if (is.na(next_anchor)) {
      break ()
    } else {
      anchor_point <- reflection_end + next_anchor
      # check for errors in order
      assertthat::assert_that(anchor_point > reflection_end,
                              msg = glue::glue("anchor point {anchor_point} is \\
                            before reflection end {reflection_end}"))
      # message
      message(glue::glue("next anchor is {anchor_point}"))
    }
  }

  # return the rows to be kept
  vec_keep <- setdiff(seq_len(nrow(data)), vec_discard)

  return(data[vec_keep, ])

}
