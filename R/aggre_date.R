#' Format datetimes for easier aggregating.
#'
#' @description Mutate datetimes for easier aggregation.
#' @param x a POSIXct vector of dates.
#' @param by Level of aggregation (minute, hour, day, week, month, or year).
#' @return A POSIXct vector of dates.
#'
#' @export
aggre_date <- function(x, by) {
    if (by == "minute" | by == "minutes") {
        out <- strptime(format(x, "%Y-%m-%dT%H:%M:00"), "%Y-%m-%dT%H:%M:%S", tz = lubridate::tz(x))
    } else if (by == "hour" | by == "hours") {
        out <- strptime(format(x, "%Y-%m-%dT%H:00:00"), "%Y-%m-%dT%H:%M:%S", tz = lubridate::tz(x))
    } else if (by == "day" | by == "days") {
        out <- strptime(format(x, "%Y-%m-%dT00:00:00"), "%Y-%m-%dT%H:%M:%S", tz = lubridate::tz(x))
    } else if (by == "week" | by == "weeks") {
        out <- strptime(format(x, "%Y-W%U-1"), "%Y-W%U-%w", tz = lubridate::tz(x))
    } else if (by == "month" | by == "months") {
        out <- strptime(format(x, "%Y-%m-01T00:00:00"), "%Y-%m-%dT%H:%M:%S", tz = lubridate::tz(x))
    } else if (by == "year" | by == "years") {
        out <- strptime(format(x, "%Y-01-01T00:00:00"), "%Y-%m-%dT%H:%M:%S", tz = lubridate::tz(x))
    } else {
        print("`by` needs to be minute(s), hour(s), day(s), week(s), month(s), or year(s)")
        return(NA)
    }
    return(as.POSIXct(out))
}
