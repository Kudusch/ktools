#' Save RDS w/ pigz support
#'
#' This function uses pigz to save RDS files.
#' pigz is a fully functional replacement for gzip that exploits
#' multiple processors and multiple cores to the hilt when compressing data.
#'
#' pigz needs to be installed for this function to work.
#'
#' @param object An object to save.
#' @param file A location to save to.
#' @param threads Number of threads to use. Defaults to \code{parallel::detectCores()-1}.
#' @param compression_level Compression level used by pigz (1–9).
#' @examples
#'\dontrun{
#' saveRDS.gz(temp, file = 'temp.RDS')
#'}
#'
#' @export
saveRDS.gz <- function(object,file,threads=(parallel::detectCores())-1,compression_level=6) {
    con <- pipe(paste0("pigz -c",compression_level," -p",threads," > ",shQuote(file)),"wb")
    saveRDS(object, file = con)
    close(con)
}
#' Read RDS w/ pigz support
#'
#' This function uses pigz to read RDS files.
#' pigz is a fully functional replacement for gzip that exploits
#' multiple processors and multiple cores to the hilt when compressing data.
#'
#' pigz needs to be installed for this function to work.
#'
#' @param file A location to read from.
#' @param threads Number of threads to use. Defaults to \code{parallel::detectCores()-1}.
#' @return The object at \code{file}.
#' @examples
#'\dontrun{
#' temp <- readRDS.gz('temp.RDS')
#'}
#' @export
readRDS.gz <- function(file,threads=(parallel::detectCores())-1) {
    con <- pipe(paste0("pigz -d -c -p",threads," ",shQuote(file)))
    object <- base::readRDS(file = con)
    close(con)
    return(object)
}
