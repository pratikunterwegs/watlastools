#' A wrapper function to enable use of func_cleanData for data accessed from the NIOZ WATLAS server for files with multiple individuals.
#'
#' @param somedata A dataframe object returned by getData. Must contain the columns "X", "Y", "SD", "NBS", "TAG", "TIME"; these are the X coordinate, Y coordinate, standard deviation in measurement, number of ATLAS towers that received the signal, the tag number, and the numeric time, in milliseconds from 1970-01-01.
#' @param sd_threshold A threshold value above which rows are removed.
#' @param nbs_min The minimum number of base stations (ATLAS towers) that received tag signal.
#' @param moving_window The size of the moving window for the running median calculation.
#' @param speed_cutoff The maximum speed in kilometres per hour allowed in the raw data. Points with straight line displacement speeds calculated as above this are removed.
#' @param filter_speed Logical specifiying whether to use the speed filter or not.
#'
#' @return A datatable class object (extends data.frame) which has the additional columns posID and ts, which is TIME converted to human readable POSIXct format.
#' @export
#'
#'
#'

wat_clean_wrap <- function(somedata, ...){

  somedata <- as.data.table(somedata)

  # determine whether one or multiple individuals.
  if(length(unique(somedata$TAG)) == 1){

    data <- somedata[,wat_clean_data(.SD,...)]
  }

  if(length(unique(somedata$TAG)) > 1){

    data <- somedata[,wat_clean_data(cbind(TAG,.SD),...), by = TAG]
  }

  return(data)

}

# ends here
