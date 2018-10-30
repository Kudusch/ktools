#' Faster prop.table(table())
#'
#' @param object An object to tabulate.
#' @param percent Return rounded percentage.
#' @param ... Other arguments passed to \link[base]{table}.
#' @examples
#' p_table(df)
p_table <- function(object, percent=TRUE, ...) {
    if (percent == FALSE) {
        return_obj <- prop.table(table(object, ...))
    } else {
        return_obj <- round(prop.table(table(object, ...)*100), 2)
    }
    names(dimnames(return_obj)) <- deparse(substitute(object))
    return(return_obj)
}
