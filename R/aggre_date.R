#' Format datetimes for easier aggregating.
#'
#' @description Mutate datetimes for easier aggregation.
#' @param x a POSIXct vector of dates.
#' @param by Level of aggregation (minute, hour, day, week, month, or year).
#' @return A POSIXct vector of dates.
#'
#' @export
aggre_date <- function(x, by) {
    if (by == "minute") {
        out <- strptime(format(x, "%Y-%m-%dT%H:%M:00"), "%Y-%m-%dT%H:%M:%S", tz = lubridate::tz(x))
    } else if (by == "hour") {
        out <- strptime(format(x, "%Y-%m-%dT%H:00:00"), "%Y-%m-%dT%H:%M:%S", tz = lubridate::tz(x))
    } else if (by == "day") {
        out <- strptime(format(x, "%Y-%m-%dT00:00:00"), "%Y-%m-%dT%H:%M:%S", tz = lubridate::tz(x))
    } else if (by == "week") {
        out <- strptime(format(x, "%Y-W%U-1"), "%Y-W%U-%w", tz = lubridate::tz(x))
    } else if (by == "month") {
        out <- strptime(format(x, "%Y-%m-01T00:00:00"), "%Y-%m-%dT%H:%M:%S", tz = lubridate::tz(x))
    } else if (by == "year") {
        out <- strptime(format(x, "%Y-01-01T00:00:00"), "%Y-%m-%dT%H:%M:%S", tz = lubridate::tz(x))
    }
    return(as.POSIXct(out))
}
