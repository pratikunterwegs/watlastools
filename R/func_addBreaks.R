wat_add_breaks <- function (df,
                              time_interval = 600,
                              dist_interval = 1000) {

  if (is.data.table(df) != TRUE) {
    setDT(df)
  }
  df[, `:=`(distance, wat_simple_dist(df, "x", "y"))] #calculate distance
  df[, `:=`(tdiff, time - shift(time, fill = first(time)))]######### get time difs between rows to calculate groups later.
  df[, `:=`(tdOver, as.integer(distance > dist_interval | tdiff > time_interval))] # find values that are > maximum difference value
  df[is.na(tdOver), tdOver := 0] # change first value to 0 (it was NA because no distance to calculate. This can be simpler in future versions)
  df[, `:=`(ID, paste(id, "-", cumsum(tdOver) + 1)), by = id] #create groups that are within the same time periods that do not have large jumps in between points.
  df[, `:=`(n, .N), by = ID] #Count number of rows in each group

  return(df)

}
