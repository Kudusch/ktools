#' Save RDS w/ pigz support
#'
#' @param object An object to save.
#' @param file A location to save to.
#' @examples
#' saveRDS.gz(temp, file = "temp.RDS)
saveRDS.gz <- function(object,file,threads=(parallel::detectCores())-1,compression_level=6) {
    con <- pipe(paste0("pigz -c",compression_level," -p",threads," > ",file),"wb")
    saveRDS(object, file = con)
    close(con)
}
#' Read RDS w/ pigz support
#'
#' @param file A location to read from.
#' @return The object at \code{file}.
#' @examples
#' temp <- readRDS.gz('temp.RDS')
readRDS.gz <- function(file,threads=(parallel::detectCores())-1) {
    con <- pipe(paste0("pigz -d -c -p",threads," ",file))
    object <- base::readRDS(file = con)
    close(con)
    return(object)
}
