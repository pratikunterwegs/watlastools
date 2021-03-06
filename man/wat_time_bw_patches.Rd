% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/func_time_bw_patch.r
\name{wat_time_bw_patches}
\alias{wat_time_bw_patches}
\title{Get time between patches.}
\usage{
wat_time_bw_patches(data, t_end = "time_end", t_start = "time_start")
}
\arguments{
\item{data}{Dataframe containing at least the variables indicating patch
start and end time.}

\item{t_end}{The departure time from a hypothetical first patch.}

\item{t_start}{The arrival time at a hypothetical second patch.}
}
\value{
A vector of times in seconds between patches.
}
\description{
Get time between patches.
}
