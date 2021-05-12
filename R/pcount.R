pcount_ <- function(tbl, x, percent, name, ...) {
    if (name == "n") {
        percent.name <- "p"
    } else {
        percent.name <- paste0(name, "_percent")
    }
    tbl <- tbl %>%
        count((!!sym(x)), name=name, ...) %>%
        mutate(!!percent.name := (!!sym(name))/sum(!!sym(name)))
    if (percent) {
        tbl[percent.name] <- round(tbl[percent.name]*100, 2)
    }
    tbl
}

#' Wrapper for \code{count()} with percentages
#'
#' @description Shortcut for count(x) %>% mutate(p = n/sum(n)). Returns APA compliant percentage values from table data.
#' @param tbl Table
#' @param x The variable to count and calculate the percentage on.
#' @param percent logical. Return rounded percentage.
#' @param name character. The name of the new column in the output. If omitted, it will default to n.
#' @param ... Other arguments passed to \link[dplyr]{count}.
#' @return \code{pcount()} returns the same as \code{count()} with an aditional percentage column. See \link[dplyr]{count}.
#'
#' @examples
#'\dontrun{
#' dat <- tibble(group = rep(1:5, each = 2),
#'               letter = c("a", "b",
#'                          "a", "c",
#'                          "a", "c",
#'                          "b", "e",
#'                          "b", "f"))
#'
#' pcount(dat, letter)
#' pcount(dat, letter, sort = TRUE)
#' pcount(dat, letter, name = "obs")
#'}
#'
#' @export
pcount <- function(tbl, x, percent=TRUE, name="n", ...) {
    pcount_(tbl, substitute(x), percent, name, ...)
}

