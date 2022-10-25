nonzero_ <- function(x){
    ## function to get a two-column matrix containing the indices of the
    ### non-zero elements in a "generalMatrix" class matrix
    stopifnot(inherits(x, "generalMatrix"))
    if (all(x@p == 0)) {
        return(matrix(
            0,
            nrow=0,
            ncol=2,
            dimnames=list(character(0), c("item1","item2"))))
    }
    res <- cbind(x@i+1, rep(seq(dim(x)[2]), diff(x@p)))
    colnames(res) <- c("item1", "item2")
    res <- res[x@x != 0, , drop = FALSE]
    return(res)
}

pairwise_count_ <- function(tbl, item, feature, sort, diag, upper) {
    feature <- as.character(feature)
    item <- as.character(item)

    values <- tbl[[item]]
    values_unique <- unique(values)
    tbl[[item]] <- match(values, values_unique)

    pairs_sparse <- tidytext::cast_sparse(tbl, (!!dplyr::sym(item)), (!!dplyr::sym(feature)))
    res <- Matrix::tcrossprod(pairs_sparse)
    res <- as(res, "generalMatrix")
    pairs_counted <- nonzero_(res) |>
        tibble::as_tibble() |>
        dplyr::mutate(item1 = values_unique[item1],
                      item2 = values_unique[item2],
                      n = res@x)
    if (!upper) {
        pairs_counted <- dplyr::filter(pairs_counted, item1 <= item2)
    }
    if (!diag) {
        pairs_counted <- dplyr::filter(pairs_counted, item1 != item2)
    }
    if (sort == TRUE) {
        pairs_counted <- pairs_counted |> dplyr::arrange(-n)
    }
    pairs_counted
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
pairwise_count <- function(tbl, item, feature, sort=FALSE, diag=FALSE, upper=TRUE) {
    pairwise_count_(tbl,
                    substitute(item),
                    substitute(feature),
                    sort,
                    diag,
                    upper)
}
