#' Library please! Easier Package Installation
#'
#' @description If the last error was raised due to calling \code{library()} on a missing package, install from CRAN and load the package.
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
    package_name <- deparse(substitute(package))
    if (!package_name %in% row.names(installed.packages())) {
        cat(paste("Install package ", package_name, "?\n", sep = ""))
        if (readline(prompt="(y/n) ") == "y") {
            install.packages(package_name)
            library(package_name, character.only = TRUE, verbose = TRUE)
        }
    } else {
        library(package_name, character.only = TRUE, verbose = TRUE)
    }
}
