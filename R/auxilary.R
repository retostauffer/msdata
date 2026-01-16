
#' Show HTTP Error Status and Terminate
#'
#' This function is called whenever \code{httr::GET} returns an
#' http status code out of the \code{200} range (success).
#' Shows \code{\link[httr]{http_status}} code information alongside
#' with additional messages returned by the API (if any).
#'
#' @param scode numeric, http status code.
#' @param xtra \code{NULL} or named list with additional information.
#'
#' @return No return, will terminate R.
#'
#' @author Reto Stauffer
# Show http_status message if possible.
show_http_status_and_terminate <- function(scode, xtra = NULL) {

    stopifnot(is.numeric(scode), length(scode) == 1)
    if (scode %/% 100 == 2) return(NULL)
    cat('---\n')

    info <- tryCatch(http_status(scode),
                     error = function(x) NULL)

    # Depending on the status code these are somewhat redundant
    if (!is.null(xtra))
        xtra <- paste(c("  status returned by API:",
                      sprintf("    %-20s %s", sprintf("%s:", names(xtra)), xtra)),
                      collapse = "\n")
    if (!is.null(info))
        info <- paste(c("  http_status description:",
                      sprintf("    %-20s %s", sprintf("%s:", names(info)), info)),
                      collapse = "\n")

    if (is.null(info) & is.null(xtra)) {
        stop("HTTP request error: server returned status code ", scode)
    } else {
        stop(paste("HTTP request error", xtra, info, sep = "\n"))
    }
}
