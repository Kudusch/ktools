#' Faster \code{prop.table(table())}
#'
#' @description Wrapper for \code{table()} and \code{prop.table()}. Returns APA compliant percentage values from table data.
#' @param object An object to tabulate.
#' @param percent Return rounded percentage.
#' @param ... Other arguments passed to \link[base]{table}.
#' @return \code{ptable()} returns a contingency table, an object of class "table", an array of integer values. See \link[base]{table}.
#' @examples
#' ptable(c(1,1,1,2,2,3), percent = FALSE, useNA = 'always')
#'
#' @export
ptable <- function(object, percent=TRUE, ...) {
    if (percent == FALSE) {
        return_obj <- prop.table(table(object, ...))
    } else {
        return_obj <- round(prop.table(table(object, ...))*100, 2)
    }
    names(dimnames(return_obj)) <- deparse(substitute(object))
    return(return_obj)
}
