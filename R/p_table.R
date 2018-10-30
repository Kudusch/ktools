#' Faster prop.table(table())
#'
#' @param object An object to tabulate.
#' @param ... Other arguments passed to \link[base]{table}.
#' @examples
#' p_table(df)
p_table <- function(object, ...) {
    return(prop.table(table(object, ...)))
}
