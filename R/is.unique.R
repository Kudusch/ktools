#' Does a list contain duplicates.
#'
#' @description Check if \code{length(x) == length(unique(x))} and optionally print table.
#' @param list The list to check for uniqueness.
#' @param boolOnly Return only a boolean.
#' @return If \code{boolOnly == FALSE} then only \code{TRUE}/\code{FALSE} is returned, else print table with length of \code{list} and \code{unique(list)}.
#' @examples
#'\dontrun{
#' is.unique(list)
#' # [1] TRUE
#'
#' is.unique(x, boolOnly = F)
#' # complete   unique    delta
#' # 11       10        1
#' # [1] FALSE
#'}
#'
#' @export
is.unique <- function(list, boolOnly=TRUE) {
    if (boolOnly == TRUE) {
        return(length(list)==length(unique(list)))
    } else {
        return_obj <- as.table((c(length(list), length(unique(list)), length(list)-length(unique(list)))))
        names(return_obj) <- c("complete", "unique", "delta")
        print(return_obj)
        return(length(list)==length(unique(list)))
    }
}
