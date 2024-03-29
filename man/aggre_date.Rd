% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aggre_date.R
\name{aggre_date}
\alias{aggre_date}
\title{Format datetimes for easier aggregating.}
\usage{
aggre_date(x, by)
}
\arguments{
\item{x}{a POSIXct vector of dates.}

\item{by}{Level of aggregation (minute, hour, day, week, month, or year).}
}
\value{
A POSIXct vector of dates.
}
\description{
Mutate datetimes for easier aggregation.
}
