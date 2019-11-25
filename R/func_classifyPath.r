#' classifyPath
#'
#' @param somedata A data frame which must have a column for the residence time at each point.
#' @param resTimeLimit A numeric giving the time limit in minutes against which residence time is compared.
#'
#' @return A data.frame extension object, which retains only points classified as residence points if residence time is below \code{resTimeLimit} over \code{travelSeg} points.
#' @import data.table
#' @export
#'

funcClassifyPath <- function(somedata,
                             resTimeLimit = 2) {
  # check somedata is a data.frame and has a resTime column
  {
    assertthat::assert_that("data.frame" %in% class(somedata),
                            msg = "classifypath: not a dataframe object!")

    assertthat::assert_that("resTime" %in% names(somedata),
                            msg = "classifypath: data has no residence time column")
    assertthat::assert_that(min(c(resTimeLimit)) > 1,
                            msg = "funcClassifyPath: function arguments are not positive")
  }

  # make datatable to use functions
  if (is.data.table(somedata) != TRUE) {
    setDT(somedata)
  }

  # handle global variable issues
  resTime <- resTimeBool <- rollResTime <- NULL
  time <- timediff <- type <- x <- y <- npoints <- NULL

  # sort by time
  data.table::setorder(somedata, time)

  # check this has worked
  {
    assertthat::assert_that(min(diff(somedata$time)) >= 0,
                            msg = "data for segmentation is not ordered by time")
  }

  # prep to assign sequence to res patches
  # to each id.tide combination
  # remove NA vals in resTime
  somedata <- somedata[!is.na(resTime),]

  # drop NAs in rolling residence time evaluation
  # essentially the first and last elements will be dropped
  somedata <- somedata[resTime >= resTimeLimit, ]

  # print message if dataframe has few rows
  {
    if (nrow(somedata) < 5) {
      print(glue::glue('\n\n...segmented dataframe has < 5 rows\n\n'))
    }
  }

  return(somedata)

}

# ends here
