col_name_ <- function(x, default = stop("Please supply column name", call. = FALSE))
{
    if (is.character(x))
        return(x)
    if (identical(x, quote(expr = )))
        return(default)
    if (is.name(x))
        return(as.character(x))
    if (is.null(x))
        return(x)
    stop("Invalid column specification", call. = FALSE)
}

count_pairwise_ <- function(tbl, item, feature, sort, diag, upper) {
    tbl <- tbl %>%
        dplyr::group_by((!!sym(feature))) %>%
        dplyr::group_map(~ expand.grid(.[[item]], .[[item]])) %>%
        do.call(rbind, .) %>%
        as.data.frame() %>%
        dplyr::group_by_all() %>%
        dplyr::count() %>%
        dplyr::rename(item1 = 1,
                      item2 = 2) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(item1 = as.character(item1),
               item2 = as.character(item2),
               n = as.numeric(n))

    if (diag == FALSE) {
        tbl <- tbl %>% filter(item1 != item2)
    }

    if (upper == FALSE) {
        tbl <- tbl %>%
            dplyr::mutate(item1_sort = ifelse(item1 < item2, item1, item2),
                   item2_sort = ifelse(item1 < item2, item2, item1)) %>%
            dplyr::distinct(item1_sort, item2_sort, .keep_all = TRUE) %>%
            dplyr::select(-item1_sort, -item2_sort)
    }

    if (sort == TRUE) {
        tbl %>%
            dplyr::arrange(-n)
    } else {
        tbl
    }
}

#' Count pairs of items within a group
#'
#' Count the number of times each pair of items appear together within a group
#' defined by "feature." For example, this could count the number of times
#' two words appear within documents).
#'
#' @param tbl Table
#' @param item Item to count pairs of; will end up in \code{item1} and
#' \code{item2} columns
#' @param feature Column within which to count pairs
#' \code{item2} columns
#' @param sort Sort the output by \code{n}?
#' @param diag Count.
#' @param upper Remove duplicates.
#'
#' @examples
#'\dontrun{
#' library(dplyr)
#' dat <- tibble(group = rep(1:5, each = 2),
#'               letter = c("a", "b",
#'                          "a", "c",
#'                          "a", "c",
#'                          "b", "e",
#'                          "b", "f"))
#'
#' # count the number of times two letters appear together
#' count_pairwise(dat, letter, group)
#' count_pairwise(dat, letter, group, sort = TRUE)
#' count_pairwise(dat, letter, group, sort = TRUE, diag = TRUE)
#'}
#'
#' @export
count_pairwise <- function(tbl, item, feature, sort=FALSE, diag=FALSE, upper=TRUE) {
    count_pairwise_(tbl,
                    col_name_(substitute(item)),
                    col_name_(substitute(feature)),
                    sort,
                    diag,
                    upper)
}
