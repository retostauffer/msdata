



#'
#'
#' @param station character of length 1, name or abbreviation of the station
#'        (can be a regular expression; case sensitive). Must match one single station
#'        station for which data is requested.
#' @param dir `NULL` or path to a directory for caching (forwarded
#'        to e.g., [sg_download_asset()], RETO TODO.
#' @param verbose logical. If `TRUE` some messages will be thrown.
#' @param \dots only for development purposes.
#'
#' @export
#' @author Reto
sg_stationdata_mswiss <- function(station, dir = NULL, verbose = FALSE, ...) {

    verbose <- as.logical(verbose[[1]])
    stopifnot(
        "argument 'station' must be character of length 1" =
            is.character(station) && length(station) == 1L &&
            nchar(station) > 0L,
        "argument 'verbose' must evaluate to `TRUE` or `FALSE`" =
            isTRUE(verbose) || isFALSE(verbose)
    )

    # By default the resource id is 'ch.meteoschweiz.ogd-smn'. Can
    # be overwritten by providing `id = ...` via the dots argument
    # (for development purposes only).
    args <- list(...)
    id <- if (!is.null(args$id)) as.character(args$id[[1]]) else "ch.meteoschweiz.ogd-smn"

    # Getting items
    items <- tryCatch(sg_items(id), error = function(e) e)
    if (!is.data.frame(items))
        stop("Problems downloading resource items for id \"", id, "\":\n", items)

    # Trying to identify the station
    items <- subset(items, grepl(station, title))
    if (nrow(items) == 0L) {
        stop("No station found matching station = \"", station, "\".")
    } else if (nrow(items) > 1L) {
        stop("Found multiple stations matching station = \"", station,
             "\". Adjust the 'station' argument so it matches one specific station only.\n",
             "Currently matches: ", paste(items$title, collapse = ", "), ".")
    }

    assets <- as.data.frame(items$assets)

    if (verbose)
        message("Processing station ", items$title, " (id ", items$id, ").",
                "Number of assets: ", nrow(assets))


    return(assets)

}
