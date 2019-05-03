#' Calculate mode.
#'
#' @description Generic function for mode of a sample.
#' @param x An R object. Currently there are methods for numeric objects.
#' @param na.rm A logical value indicating whether NA values should be stripped before the computation proceeds.
#' @return The mode of \code{x} or a list of the modes of \code{x}.
#' @examples
#' modus(c(1,1,1,2,2,3,NA), na.rm = TRUE)
#'
#' @export
modus <- function(x, na.rm=FALSE) {
    if (na.rm == FALSE) {
        na <- 'a'
    } else {
        na <- 'n'
    }
    return_obj <- sort(table(x, useNA = na), decreasing = TRUE)
    return_obj <- names(return_obj[return_obj == return_obj[1]])
    return_obj <- tryCatch({as.numeric(return_obj)}, error = function(e) {return_obj}, warning = function(w) {return_obj})
    return(return_obj)
}
