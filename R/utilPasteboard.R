#' Write To The Pasteboard In macOS
#'
#' This function uses the pbcopy utility to write to the pasteboard.
#'
#' @param s An character string to write to the pasteboard.
#' @examples
#'\dontrun{
#' writePasteboard("Hello!")
#'}
#'
#' @export
writePasteboard <- function(s) {
    s <- tryCatch({
        as.character(s)
    }, error = function(error_condition) {
        stop("Input not treatable as string")
    })
    tryCatch({
        con <- pipe("pbcopy", "w")
        cat(s, file = con)
    }, error = function(error_condition) {
        stop("Pasteboard not writable")
    }, finally={
        close(con)
    })
}
#' Read From The Pasteboard In macOS
#'
#' This function uses the pbpaste utility to read from the pasteboard.
#'
#' @return The string currently in the pasteboard.
#' @examples
#'\dontrun{
#' s <- readPasteboard()
#'}
#' @export
readPasteboard <- function() {
    tryCatch({
        out <- system("pbpaste", intern = TRUE)
        if (length(out) > 1) {
            out <- paste(out, collapse = "\n")
        }
        return(out)
    }, error = function(error_condition) {
        stop("Pasteboard not readable")
    })
}
