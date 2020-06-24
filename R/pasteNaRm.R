#' Alternative to `paste()`, where NAs are remove before conversion to character strings
#'
#' @description Behaves as `paste()` would, but removes NAs before converting its arguments (via as.character) to character strings
#' @param ... See `paste()` for arguments.
#' @param sep See `paste()` for arguments.
#' @param collapse See `paste()` for arguments.
#' @return See `paste()` for return values.
#' @examples
#'\dontrun{
#' s <- c("a", "b", NA, "d")
#' paste(s, collapse = "")
#' # [1] "abNAd"
#'
#' pasteNArm(s, collapse = "")
#' # [1] "Abd"
#'}
#'
#' @export
pasteNArm <- function(..., sep="", collapse = "") {
    l <- list(...)
    l <- lapply(l, function(x) {x[is.na(x)] <- ""; x})
    return(do.call("paste", c(l, sep = sep, collapse = collapse)))
}
