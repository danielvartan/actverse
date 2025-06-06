% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_epoch.R
\name{find_epoch}
\alias{find_epoch}
\alias{find_epoch.numeric}
\alias{find_epoch.hms}
\alias{find_epoch.tbl_df}
\title{Find distinct epochs in a series}
\usage{
find_epoch(x, threshold = 0.9)

\method{find_epoch}{numeric}(x, threshold = 0.9)

\method{find_epoch}{hms}(x, threshold = 0.9)

\method{find_epoch}{tbl_df}(x, threshold = 0.9)
}
\arguments{
\item{x}{Any \code{\link[base:vector]{atomic}} vector, provided that the function
has a method for handling it.}

\item{threshold}{(optional) A number, from \code{0} to \code{1}, indicating the minimum
proportion that an epoch must have to be considered valid. \code{threshold = 1}
means that the regularity of the time series must be strict (i.e., have
just 1 periodicity) (default: \code{0.9}).}
}
\value{
A \code{\link[=list]{list}} object with the following elements:
\itemize{
\item \code{best_match}: A number indicating the epoch/periodicity above the
\code{threshold} with greater prevalence in seconds. If none is find, \code{best_match}
value will be equal as \code{as.numeric(NA)}.
\item \code{prevalence}: a \code{\link[tibble:tibble]{tibble}} listing the unique
epochs/periodicities found in \code{data} along with its proportions.
}
}
\description{
\code{find_epoch()} identifies the distinct epochs or periodicities present in an
object and returns them along with the most prevalent epoch that meets a
specified threshold.
}
\details{
In rare cases where multiple periodicities have the same highest prevalence
above the threshold, \code{best_match} will return only one of those values.
}
\examples{
data <-
  dplyr::tibble(
    index = c(
      as.POSIXct(
        seq(60, 5400, by = 60),
        origin = lubridate::origin
      ),
      as.POSIXct(
        seq(5430, 5490, by = 30),
        origin = lubridate::origin
      ),
      as.POSIXct(
        seq(5505, 5520, by = 15),
        origin = lubridate::origin
      ),
      as.POSIXct(
        seq(5530, 5540, by = 10),
        origin = lubridate::origin
      ),
      as.POSIXct(
        seq(5545, 5555, by = 5),
        origin = lubridate::origin
      )
    ),
    x = seq_along(timestamp)
  ) |>
  tsibble::tsibble(index = index)

find_epoch(data, 0.8)

seq(1, 100, by = 5) |> find_epoch(threshold = 0.8)
}
\seealso{
Other utility functions: 
\code{\link{aggregate_index}()},
\code{\link{get_raw_data}()}
}
\concept{utility functions}
