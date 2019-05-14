#' Library please! Easier Package Installation
#'
#' @description If the last error was raised due to calling \code{library()} on a missing package.
#' @param package The package to install.
#' @examples
#'\dontrun{
#' library(parallel)
#' # Error in library(parallel) : there is no package called ‘parallel’
#'
#' library_pls(parallel)
#' # Install package parallel?
#' # (y/n) y
#' # Installing package …
#'}
#'
#' @importFrom utils install.packages
#'
#' @export
library_pls <- function(package) {
    if(interactive()) {
        package_name <- deparse(substitute(package))
        last_error <- .traceback()
        if (!is.null(last_error)) {
            if (last_error[[1]] == "stop(packageNotFoundError(package, lib.loc, sys.call()))") {
                cat(paste("Install package ", package_name, "?\n", sep = ""))
                if (readline(prompt="(y/n) ") == "y") {
                    install.packages(package_name)
                    library(package_name, character.only = TRUE, verbose = TRUE)
                }
            } else {
                message("Nothing to do!")
            }
        } else {
            message("Nothing to do!")
        }
    } else {
        message(.traceback())
    }
}
