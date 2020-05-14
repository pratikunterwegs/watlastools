#' A function to clean data accessed from the NIOZ WATLAS server. Allows filtering on standard deviation, and the number of receivers that detected the tag. Applies a moving-window median filter, whose size can be specified.
#'
#' @param somedata A dataframe object returned by getData. Must contain the columns "X", "Y", "SD", "NBS", "TAG", "TIME"; these are the X coordinate, Y coordinate, standard deviation in measurement, number of ATLAS towers that received the signal, the tag number, and the numeric time, in milliseconds from 1970-01-01.
#' @param sd_threshold A threshold value above which rows are removed.
#' @param nbs_min The minimum number of base stations (ATLAS towers) that received tag signal.
#' @param moving_window The size of the moving window for the running median calculation.
#' @param var_lim The threshold value for both VARX and VARY, above which are removed. Note that these are specific outputs for raw localisations extracted from the ATLAS system.
#' @param max_tDiff The maximum temporal gap between datapoints for which median smoothing will occur.
#' @param speed_cutoff The maximum speed in kilometres per hour allowed in the raw data. Points with straight line displacement speeds calculated as above this are removed.
#' @param filter_speed Logical specifiying whether to use the speed filter or not.
#'
#' @return A datatable class object (extends data.frame) which has the additional columns posID and ts, which is TIME converted to human readable POSIXct format.
#' @export
#'
wat_clean_data <- function(somedata,
                           max_tDiff=30,
                           moving_window = 3,
                           var_lim = 2500,
                           nbs_min = 0,
                           sd_threshold = 100,
                           filter_speed = TRUE,
                           speed_cutoff = 150)
{
  SD <- NBS <- TIME <- TAG <- X <- Y <- NULL #sets vectors to NULL
  posID <- ts <- X_raw <- Y_raw <- VARX <- VARY <- COVXY <- NULL
  sld <- sld_speed <- NULL

  # check parameter types and assumptions
  {
    assertthat::assert_that("data.frame" %in% class(somedata),
                            msg = "cleanData: not a dataframe object!") #assert that is like an if statement but breaks function if not met
    {
      dfnames <- colnames(somedata)
      namesReq <- c("X", "Y", "SD", "NBS", "TAG", "TIME", "VARX", "VARY", "COVXY")
      # include asserts checking for required columns
      purrr::walk(namesReq, function(nr) {
        assertthat::assert_that(nr %in% dfnames,
                                msg = glue::glue("cleanData: {nr} is required but missing from data!"))
      })
    }
    # check args positive
    assertthat::assert_that(min(c(moving_window)) > 1,
                            msg = "cleanData: moving window not > 1") #alerts user to errors in function parameters
    assertthat::assert_that(min(c(nbs_min)) >= 0,
                            msg = "cleanData: NBS min not positive")
    assertthat::assert_that(min(c(speed_cutoff)) >= 0,
                            msg = "cleanData: speed cutoff not positive")
  }
  # convert to data.table
  {
    # convert both to DT if not
    if (data.table::is.data.table(somedata) != TRUE) {
      setDT(somedata)
    }
  }
  # delete positions with approximated standard deviations above SD_threshold,
  # and below minimum number of base stations (NBS_min), VARX and VARY values
  # Ensure that badly estimated locations are removed.
  somedata <- somedata[SD < sd_threshold &
                         NBS >= nbs_min &
                         VARX < var_lim &
                         VARY < var_lim,
                       ]

  # tag prefix
  prefix_num <- 31001000000

  # begin processing if there is data
  if (nrow(somedata) > 1) { # Shouldn't this be >= 1?
    #somedata[,.SD[order(TIME)], by = id] # not tested this but its probably a good checker.
    # add position id and change time to seconds
    somedata[, `:=`(posID = 1:nrow(somedata),#can use this to determine original position of point, useful for debugging
                    TIME = as.numeric(TIME)/1e3, #time from atlas is given in milliseconds from 1970, change to seconds.
                    TAG = as.numeric(TAG) - prefix_num, # remove prefix from tag column
                    X_raw = X, #create columns for original x and y values
                    Y_raw = Y)]

    if(filter_speed == TRUE){
      # filter for insane speeds if asked
      somedata[, `:=`(sld, wat_simple_dist(somedata, "X", "Y"))] #calculate distance
      somedata[, `:=`(tdiff, TIME - shift(TIME))]######### get time difs between rows to calculate groups later.
      somedata[, `:=`(sld_speed, sld/c(NA, as.numeric(diff(TIME))))] #calculate speed

      while(nrow(somedata[sld_speed > (speed_cutoff/3.6),])>0){ #While there are any points that are over the speed cut off limit, remove them, recalculate speeds without these points. The next parts may also remove points and therefore speeds should be checked before moving forward.

        somedata <- somedata[sld_speed <= (speed_cutoff/3.6), ] #remove speeds over the cut off then recalculate
        somedata[, `:=`(sld, wat_simple_dist(somedata, "X", "Y"))] # recalculate distance
        somedata[, `:=`(sld_speed, sld/c(NA, as.numeric(diff(TIME))))] # recalculate speed
        somedata[, `:=`(ts, as.POSIXct(TIME, tz = "CET", origin = "1970-01-01"))] #create a human-readable timestamp column

        #ensure each time group is within max_tDiff of each other (e.g. 3 rows should be 9 seconds)
        somedata[, `:=`(tdiff, TIME - shift(TIME, fill= first(TIME)))]# get time difs between rows to calculate groups later.
        somedata[, `:=`(tdOver, as.integer(tdiff > max_tDiff))] # find values that are > maximum difference value
        somedata[, `:=`(tGroup, cumsum(tdOver) + 1)] #create groups
        somedata[, `:=`(n, .N), by = tGroup]# how many points are in each section, can't really make a median with very few points

        somedata[n >= 3] # remove sections with few points before median is calculated then retest speed filters just in case

        somedata[, `:=`(sld, wat_simple_dist(somedata, "X", "Y"))] # recalculate distance
        somedata[, `:=`(sld_speed, sld/c(NA, as.numeric(diff(TIME))))] # recalculate speed

      }
    }

    # median filter
    # no longer includes reversed smoothing. Phase shifts should no longer occur due to grouping by minimum time differences.

    somedata[, `:=`(x.med,
                    lapply(.SD, function(z) {stats::runmed(z, moving_window)})),
             .SDcols = "X", by = tGroup]

    somedata[, `:=`(y.med,
                    lapply(.SD, function(z) {stats::runmed(z, moving_window)})),
             .SDcols = "Y", by = tGroup]

    ## postprocess (clean) data
    somedata <- somedata[, .(TAG, posID, TIME, ts, X_raw, Y_raw, NBS, VARX, VARY, COVXY, x.med, y.med, SD, sld, sld_speed,n)]

    # rename columns to appropriate final names
    setnames(somedata,
             old = c("x.med", "y.med", "TAG", "TIME", "sld", "sld_speed"),
             new = c("x", "y", "id", "time", "dist", "speed"))

  }else{
    somedata <- data.table::data.table(matrix(NA, nrow = 0, ncol = 16))
    colnames(somedata) <- c("id", "posID", "time", "ts",
                            "X_raw", "Y_raw", "NBS", "VARX", "VARY", "COVXY",
                            "x", "y","SD","dist","speed","n")
  }

  assertthat::assert_that("data.frame" %in% class(somedata),
                          msg = "cleanData: cleanded data is not a dataframe object!")

  return(somedata)
}

# ends here
