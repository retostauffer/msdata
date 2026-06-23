
# Helper function to extract station information from an item
#' @importFrom dplyr bind_rows
items_extract_features <- function(x) {
    stopifnot(x$type == "FeatureCollection",
              is.list(x$features) && length(x$features) > 0L)

    # Extracting features only
    x <- x$features

    # Extracting coordinates, expecting point geometries only
    tmp <- sapply(x, function(x) x$geometry$type)
    stopifnot("not all geometries are Point geometries" = all(tmp == "Point"))

    # Extract and prepare coordinates (data.frame)
    coord <- setNames(as.data.frame(t(sapply(x, function(y) y$geometry$coordinates))), c("lon", "lat"))

    # Search for atomic elements; the rest will be excluded by default
    atom <- sapply(x[[1]], is.atomic); names(atom)[atom]
    fn <- function(x) return(c(x[atom], x$properties))
    d <- as.data.frame(bind_rows(lapply(x, fn)))
    d <- cbind(coord, d)
    d <- autoconvert_datetime(d)
    d$assets <- assets(lapply(x, function(y) y$assets))

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
#' @param paging logical, defaults to `FALSE`. If `TRUE` we will send
#'        an initial request is sent to the `url` which returns a
#'        maximum of 100 entries. It is then checked if a 'next'
#'        link is included in the response used to request the
#'        next batch of up to 100 entries until finished.
#' @param limit integer, defaults to `100L`. Number of entries to
#'        be requested if `paging = TRUE`.
#' @param verbose logical, defaults to `FALSE`. If set `TRUE` the
#'        some messages are printed.
#' @param \dots forwarded to the `httr::GET()` function.
#'
#' @details We are expecting a JSON response from the API, thus
#' we expect that the request content is decoded to a list by the
#' httr package. IF `paging = FALSE` this list is returned.
#' If `paging = TRUE` a list of lists is returned, where each
#' element in the main list contains the results of one API call
#' following the 'next' links (see argument `paging`).
#'
#' If we do not get a proper HTTP status code, or
#' the extracted content is not a list, this function will throw
#' an error.
#'
#' @return List with the decoded information.
#'
#' @author Reto
#' @importFrom httr GET content status_code
get_request <- function(url, query = NULL, paging = FALSE, limit = 100L, verbose = FALSE, ...) {

    limit <- as.integer(limit)[1L]
    stopifnot(
        "argument 'paging' must be TRUE or FALSE" = isTRUE(paging) || isFALSE(paging),
        "argument 'limit' must be a single positive integer" =
            is.integer(limit) && length(limit) == 1L && !is.na(limit) && limit > 0L,
        "argument 'verbose' must be TRUE or FALSE" = isTRUE(verbose) || isFALSE(verbose)
    )

    downloadfn <- function(url, query) {
        # Scopes 'verbose'!
        if (verbose) message("Sending get request to ", url) # nocov

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
        if (!is.list(res)) stop("Result of http request no list (not JSON)") # nocov
        return(res)
    }

    # No paging expected/required we simply do cone request
    # and return the result we get (the 'raw' encoded list)
    if (!paging) return(downloadfn(url, query = NULL))

    # Else we have to send an initial request first and check
    # if we get a 'next' link which allows us to request the
    # next page (or block) of results (using the default
    # limit = 100L).

    # Initial request, check for 'next' link. `get_link_next`
    # returns a character (url) if we have to do another
    # request (while loop), else `NULL`.
    tmp <- downloadfn(url, query = list(limit = limit))
    link_next <- get_link_next(tmp)

    # Store initial result as a list
    res <- list(tmp)

    # As long as we get a 'next' link there is an additional
    # batch to download.
    while (is.character(link_next)) {
        # Sending request to the API
        tmp       <- downloadfn(link_next, query = NULL)
        link_next <- get_link_next(tmp)
        res[[length(res) + 1L]] <- tmp
    }

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


#' Collections
#'
#' Requesting a list of all available collections provided via the API.
#'
#' @param pattern `NULL` or a pattern used return a subset of all
#'        available collections, applied to the collection ID.
#' @param verbose logical, defaults to `FALSE`. If set `TRUE` the
#'        some messages are printed.
#' @param raw logical, defaults to `FALSE` (see Return).
#'
#' @return Returns a tbl data frame with the available collections.
#' If `pattern = NULL` all collections found are returned. If
#' a `pattern` is specified, all collections where the collection ID
#' (`id`) matches the pattern are returned. If no IDs match the
#' pattern, a warning is thrown and `NULL` is returned.
#'
#' If `raw = TRUE` is specified, `pattern` is ignored and the raw
#' data as retrieved by the API are returned as a list of lists.
#' Each element of the list corresponds to one API call containing
#' a set of lists corresponding to the different collections returned.
#'
#' @examples
#' \dontrun{
#' ## Fetch all available collections
#' collections <- sg_collections()
#'
#' ## Extract collections by MeteoSchweiz
#' subset(res, grepl("meteoschweiz", id))
#' }
#'
#' @export
#' @author Reto
#'
#' @importFrom dplyr bind_rows
sg_collections <- function(pattern = NULL, verbose = FALSE, raw = FALSE) {

    stopifnot(
        "argument 'pattern' must be NULL or a single non-empty character" =
            is.null(pattern) ||
            (is.character(pattern) && length(pattern) == 1L && nchar(pattern) > 0L),
        "argument 'raw' must be TRUE or FALSE" = isTRUE(raw) || isFALSE(raw)
    )

    if (verbose) message("Retrieving collections") # nocov

    # Downloading the data from the API. Each time the API returns up to
    # 100 items, paging = TRUE calls the API until all items are fetched.
    url <- sg_api_url("collections")
    res <- get_request(url, paging = TRUE, verbose = verbose)

    extractfun <- function(x) {
        stopifnot(is.list(x))
        take <- c("id", "title", "description", "license", "created", "updated")
        res <- x[take[take %in% names(x)]]
        if (!is.null(x[[c("summaries", "proj:epsg")]]))
            res$crs <- x[[c("summaries", "proj:epsg")]][[1]]
        return(res)
    }

    if (raw) return(do.call(c, res))

    if (verbose) message("Preparing data frame for the return") # nocov
    res <- lapply(res, function(x) bind_rows(lapply(x$collections, extractfun)))
    res <- bind_rows(res)
    if (!is.null(pattern)) {
        idx <- grep(pattern, res$id)
        if (length(idx) == 0L) {
            warning("No collection ids matching ", pattern, ", returning NULL.")
            return(NULL)
        }
        res <- res[idx, ]
    }

    # Converting columns containing ISO 8601 datetime information
    # from character to POSIXct
    return(autoconvert_datetime(res))
}

#' Collection Items
#'
#' Retrieving all items of a specific collection.
#'
#' @param id character, ID of the collection for which to retrieve
#'        the items (see [sg_collections()]).
#' @param verbose logical, defaults to `FALSE`. If set `TRUE` the
#'        some messages are printed.
#' @param raw logical, defaults to `FALSE` (see Return).
#'
#' @return
#' If `raw = TRUE` a list of lists is returned with the
#' raw (json decoded) result from the API calls. Each element in
#' the list corresponds to one API call (paging).
#'
#' Else (default) the function tries to auto-detect the format.
#' As an example, if the returned data are of type "FeatureCollection"s
#' The data is converted to a simple feature data frame which is
#' then returned. If now only "FeatureCollection" is implemented,
#' if the returned data is of a different type the raw list
#' is returned (TODO).
#'
#' @examples
#' \dontrun{
#' ## Retrieving items for a collection w/ features
#' CID <- "ch.meteoschweiz.ogd-smn"
#' items <- sg_items(CID)
#'
#' ## Visualizing the features
#' library("ggplot2")
#' ggplot(st_transform(items, crs = st_crs(2056))) +
#'     geom_sf(col = "lightgreen") + geom_sf_text(aes(label = id))
#' }
#'
#' @export
#' @author Reto
#'
#' @importFrom stats setNames
#' @importFrom sf st_as_sf st_crs
sg_items <- function(id, verbose = FALSE, raw = FALSE) {

    stopifnot(
        "argument 'id' must be character of length 1" = 
            is.character(id) && length(id) == 1L && nchar(id) > 0L,
        "argument 'raw' must be TRUE or FALSE" = isTRUE(raw) || isFALSE(raw)
    )

    # Generate expected API end point
    url <- sg_api_url("collections", id, "items")

    if (verbose) message("Retrieving items") # nocov

    # Downloading the data from the API. Each time the API returns up to
    # 100 items, paging = TRUE calls the API until all items are fetched.
    res <- get_request(url, paging = TRUE, verbose = verbose)

    # RAW results requested? Job done ...
    if (raw) return(res)

    # Else we try to prepare the data in an R object. First, ensure
    # that the data all match by type. TODO: Not sure if there is a
    # situation where it can happen to get different types of data
    # from one single API end point; but leaving it in here for now.
    type <- sapply(res, function(x) x$type)
    if (!all(type == type[[1]]))
        stop("Not all item type identical, got ", paste(unique(type), collapse = ", "))
    type <- type[[1]] # Our type

    # Feature type? Create simple features data.frame
    if (type == "FeatureCollection") {
        # Extracting features
        res <- do.call(rbind, lapply(res, items_extract_features))
        # Convert data frame to simple features data frame
        # TODO: Assuming 4326 here for now
        res <- st_as_sf(res, coords = c("lon", "lat"), crs = st_crs(4326))
    }

    return(res)
}

#' Collection Assets
#'
#' Retrieving all assets of a specific collection.
#'
#' @param id character, ID of the collection for which to retrieve
#'        the items (see [sg_collections()]).
#' @param verbose logical, defaults to `FALSE`. If set `TRUE` the
#'        some messages are printed.
#' @param raw logical, defaults to `FALSE` (see Return).
#'
#' @return A tbl data frame containing the asset details is returned.
#'
#' If `raw = TRUE` a list of lists is returned with the raw (json decoded)
#' result from the API calls. Each element in the list corresponds to one API
#' call (paging).
#'
#' @export
#' @author Reto
#'
#' @importFrom dplyr bind_rows
sg_assets <- function(id, verbose = FALSE, raw = FALSE) {

    stopifnot(
        "argument 'id' must be character of length 1" = 
            is.character(id) && length(id) == 1L && nchar(id) > 0L,
        "argument 'raw' must be TRUE or FALSE" = isTRUE(raw) || isFALSE(raw)
    )

    # Generate expected API end point
    url <- sg_api_url("collections", id, "assets")

    if (verbose) message("Retrieving items") # nocov

    # Downloading the data from the API. Each time the API returns up to
    # 100 items, paging = TRUE calls the API until all items are fetched.
    res <- get_request(url, paging = TRUE, verbose = verbose)

    # RAW results requested? Job done ...
    if (raw) return(res)

    # Extracting asset details and return as data.frame
    fn <- function(x) {
        atom <- which(sapply(x, is.atomic))
        return(x[atom])
    }
    res <- bind_rows(lapply(res, function(x) lapply(x$assets, fn)))
    names(res) <- gsub("\\:", "_", names(res))

    return(autoconvert_datetime(res))
}

##  #' Data Inventory
##  #'
##  #' Downloading data inventory of a specific data set collection.
##  #'
##  #' @param id character, ID of the collection for which to retrieve
##  #'        the items (see [sg_collections()]).
##  #' @param verbose logical, defaults to `FALSE`. If set `TRUE` the
##  #'        some messages are printed.
##  #'
##  #' @return If `raw = TRUE` a list of lists is returned with the
##  #' raw (json decoded) result from the API calls. Each element in
##  #' the list corresponds to one API call (paging).
##  #'
##  #' @export
##  #' @author Reto
##  #'
##  #' @importFrom stats setNames
##  #' @importFrom utils read.csv2
##  sg_inventory <- function(id, verbose = FALSE, raw = FALSE) {
##
##      stopifnot(
##          "argument 'id' must be character of length 1" =
##              is.character(id) && length(id) == 1L && nchar(id) > 0L,
##          "argument 'raw' must be TRUE or FALSE" = isTRUE(raw) || isFALSE(raw)
##      )
##
##      # Generate expected API end point
##      url <- sg_api_url("collections", id, "assets")
##
##      if (verbose) message("Retrieving items") # nocov
##
##      # Downloading the data from the API. Each time the API returns up to
##      # 100 items, paging = TRUE calls the API until all items are fetched.
##      res <- get_request(url, paging = TRUE, verbose = verbose)
##
##      # RAW results requested? Job done ...
##      if (raw) return(res)
##
##      # Else we try to prepare the data in an R object
##      type <- sapply(res, function(x) x$type)
##      if (!all(type == type[[1]])) {
##          # TODO: Is that possible?
##          stop("Not all item type identical, got ", paste(unique(type), collapse = ", "))
##      }
##      type <- type[[1]] # Our type
##
##      # Feature type? Create simple features data.frame
##      if (type == "FeatureCollection") {
##          # Extracting features
##          res <- do.call(rbind, lapply(res, items_extract_features))
##      }
##
##      # Convert data frame to simple features data frame
##      return(st_as_sf(res, coords = c("lon", "lat"), crs = st_crs(4326)))
##  }

