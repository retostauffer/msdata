

items_extract_stations <- function(x) {
    stopifnot(x$type == "FeatureCollection",
              is.list(x$features) && length(x$features) > 0L)

    f <- x$features

    # Extracting coordinates, expecting point geometries only
    tmp <- sapply(f, function(x) x$geometry$type)
    stopifnot("not all geometries are Point geometries" = all(tmp == "Point"))

    # Extract and prepare coordinates (data.frame)
    coord <- setNames(as.data.frame(t(sapply(f, function(x) x$geometry$coordinates))), c("lon", "lat"))

    # Extracting additional properties
    d <- data.frame(id = sapply(f, function(x) x$id),
                    name = sapply(f, function(x) x$properties$title))
    d <- cbind(coord, d)
    return(d)
}

#' Get Request Helper Function
#'
#' Auxilary function to send GET requests with additional error handling.
#'
#' @param url character, URL to send the request to. Can contain
#'        get parameters in the URL.
#' @param query `NULL` (default) or a list with get parameters to be sent
#'        with the request. Warning: If the URL contains get requests and
#'        `query` is not `NULL`, the parameters in the URL (`url`) will
#'        be overwritten!
#'
#' @details We are expecting a JSON response from the API, thus
#' we expect that the request content is decoded to a list by the
#' httr package. If we do not get a proper HTTP status code, or
#' the extracted content is not a list, this function will throw
#' an error.
#'
#' @return List with the decoded information.
#'
#' @author Reto
#' @importFrom httr GET content show_http_status_and_terminate
get_request <- function(url, query = NULL, ...) {
    message("Sending request to ", url)
    # Sending query = NULL would kill (remove) the parameters specified in URL
    req <- if (is.null(query)) GET(url, ...) else GET(url, query = query, ...)

    if (!status_code(req) %/% 100 == 2) {
        # Trying to read the response and see if the API answered
        # with an error message (error details). If so, that will be
        # shown, else a more generic error will be displayed.
        tmp <- tryCatch(content(req), error = function(x) NULL)
        show_http_status_and_terminate(status_code(req), tmp)
    }

    # Extracting content
    res <- content(req)
    if (!is.list(res))
        stop("Result of http request no list (not JSON)")

    return(res)
}

# Auxilary function to extract the 'next' link from a previous
# items request. If not found (i.e., no additional next batch
# to be processed) NULL is returned, else the URL for the next
# get request.
get_link_next <- function(x) {
    stopifnot(
        "argument 'x' expected to be list" = is.list(x),
        "expected list 'x' to contain $links" = !is.null(x$links)
    )
    x   <- x$links
    idx <- which(sapply(x, function(k) k$rel == "next"))
    return(if (!length(idx) == 1L) NULL else x[[idx]]$href)
}

#' Available Automated Weather Stations
#'
#' Retrieving all available automatic weather stations.
#'
#' @param url `NULL` or an URL TODO: If we need to retrieve
#'        data from other collections we may better go for a 'collection'
#'        argument, for now only tested `ch.meteoschweiz.ogd-smn` which is
#'        specified internally.
#'
#' @return A simple feature data frame with station name, id, and its
#' geographical location. Unfortunately, we do not get altitude information
#' or information about the period a specific station provides information.
#' TODO: Currently ignoring 'assets' which contain the CSV file names
#' containing information about temporal resolution and type of data.
#' We will need to extract that.
#' See <https://opendatadocs.meteoswiss.ch/general/download>.
#'
#' @export
#' @author Reto
#'
#' @importFrom stats setNames
#' @importFrom sf st_as_sfc st_crs
ms_stations <- function(url = NULL) {

    stopifnot(
        "argument 'url' must be NULL or character of length 1" = 
            is.null(url) || (is.character(url) && length(url) == 1L)
    )
    if (is.null(url))
        url <- paste0("https://data.geo.admin.ch/",
                      "api/stac/v1/collections/ch.meteoschweiz.ogd-smn/items")

    # List to store result
    res <- list()

    # Sending initial request
    tmp <- get_request(url, query = list(limit = 100L))
    res[[1L]] <- items_extract_stations(tmp)

    # Extracting link for the next batch request
    nxt <- get_link_next(tmp)

    # As long as we get a 'next' link there is an additional
    # batch to download.
    while (is.character(nxt)) {
        # Sending request to the API
        tmp <- get_request(nxt)

        # Extract and store station information
        res[[length(res) + 1L]] <- items_extract_stations(tmp)
        nxt <- get_link_next(tmp)
    }

    return(st_as_sf(do.call(rbind, res), coords = c("lon", "lat"), crs = st_crs(4326)))
}

#' @export
ms_assets <- function(url = NULL) {

    stopifnot(
        "argument 'url' must be NULL or character of length 1" = 
            is.null(url) || (is.character(url) && length(url) == 1L)
    )

    if (is.null(url))
        url <- paste0("https://data.geo.admin.ch/",
                      "api/stac/v1/collections/ch.meteoschweiz.ogd-smn/assets")

    tmp <- get_request(url)
    return(tmp)

}
