
#' Generate Geoportal API
#'
#' @param x if `NULL` (default) the base URL is returned.
#'        If character, the base URL is extended. If `x` is
#'        a vector of length > 1L the elements will be collapsed
#'        using `/` to generate a valid URL.
#' @param version integer, the API version (defaults to 1L).
#'
#' @return Character of length one with the URL of the API end point.
#'
#' @details On loading the package sets the option `"swissgeo.apiurl"`
#' with the main entry point to the API. This is used inside this
#' function (i.e., by the entire package) to talk to the API.
#'
#' This would allow users to change the end-point (or version)
#' without updating the package, e.g., for testing.
#'
#' @examples
#' ## Base URL
#' sg_api_url()
#' ## Base URL with different API version
#' sg_api_url(version = 2L)
#'
#' ## Extending the base URL to point to a specific API endpoint
#' sg_api_url("ch.meteoschweiz.ogd-smn")
#' sg_api_url("/ch.meteoschweiz.ogd-smn")
#' sg_api_url(c("ch.meteoschweiz.ogd-smn", "items"))
#'
#' ## Changing default API URL. Possible, but not a standard use-case.
#' hold_apiurl <- getOption("swissgeo.apiurl") # kept for resetting
#' options(swissgeo.apiurl = "https://some.new.domain/api/stac/v5")
#' sg_api_url()
#'
#' ## Setting back to defaults
#' options(swissgeo.apiurl = hold_apiurl)
#' sg_api_url()
#'
#' @author Reto
#' @export
sg_api_url <- function(x = NULL, version = 1L) {
    version <- as.integer(version)[1L]
    stopifnot(
        "argument 'x' must be NULL or character vector, no empty strings allowed" =
            is.null(x) || (is.character(x) && all(nchar(x) > 0L)),
        "argument 'version' must be a positive integer" =
            is.integer(version) && version > 0L
    )

    apiurl <- getOption("swissgeo.apiurl")
    if (is.null(x)) return(apiurl)

    # Else extending the URL
    if (!substr(x[[1]], 0, 1) == "/") x[[1]] <- paste0("/", x[[1]])
    return(paste0(apiurl, paste(x, collapse = "/")))
}


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


#' Converting Datetime Columns
#'
#' Takes a data frame as input and tries to identify columns
#' containing datetime information.
#'
#' @param x data frame.
#'
#' @return Data frame with POSIXct variables if any variable
#' containing datetime information was detected.
#'
#' @details Data received by the API or read via the files
#' provided regularey contain date or datetime information
#' as character strings.
#'
#' This function takes a data frame and tries to identify
#' variables/columns containing date and time information
#' to coerce the information into Date or POSIXct objects.
#' Currently, the following formats are checked currently:
#'
#' * `2026-01-16T17:34:34(...)Z`: converted to POSIXct
#' * `16.01.2026`: converted to Date
#' * `2026-01-16`: converted to Date
#' * `16.01.2026 12:03`: converted to POSIXct w/ time zone Europe/Zuerich
#' * `16.01.2026 00:00`: converted to Date
#'
#' @examples
#' \dontrun{
#' autoconvert_datetime(data.frame(a = 1, b = "2026-01-16T12:34:56.23435Z"))
#' autoconvert_datetime(data.frame(a = 1, b = "2026-01-16T12:34:56Z"))
#' autoconvert_datetime(data.frame(a = 1, b = "16.01.2026 12:34"))
#' autoconvert_datetime(data.frame(a = 1, b = "16.01.2026 00:00"))
#' autoconvert_datetime(data.frame(a = 1, b = "16.01.2026"))
#' autoconvert_datetime(data.frame(a = 1, b = "2026-01-16"))
#' }
#'
#' @author Reto
#' @importFrom parsedate parse_iso_8601
autoconvert_datetime <- function(x) {
    stopifnot(is.data.frame(x))
    if (nrow(x) == 0L) return(x)

    get_idx <- function(x, pattern) {
        tmp <- sapply(x, function(x) grepl(pattern, x) | is.na(x))
        which(if (is.matrix(tmp)) colSums(tmp) == nrow(x) else tmp)
    }

    # Checking for ISO8601 format (Y-m-dTH:M:S..) and convert, if found
    idx <- get_idx(x, "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}")
    for (i in idx) x[[i]] <- parse_iso_8601(x[[i]])

    # Checking for 'german date' and convert, if found.
    idx <- get_idx(x, "^[0-9]{2}.[0-9]{2}.[0-9]{4}$")
    for (i in idx) x[[i]] <- as.Date(x[[i]], format = "%d.%m.%Y")

    # Checking for 'date' and convert, if found.
    idx <- get_idx(x, "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")
    for (i in idx) x[[i]] <- as.Date(x[[i]], format = "%Y-%m-%d")

    # Checking for 'german date and time' and convert, if found.
    idx <- get_idx(x, "^[0-9]{2}.[0-9]{2}.[0-9]{4}\\s+[0-9]{2}:[0-9]{2}")
    for (i in idx) x[[i]] <- as.POSIXct(x[[i]], format = "%d.%m.%Y %H:%M", tz = "Europe/Zurich")

    return(x)
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
        if (verbose) message("Sending get request to ", url)

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
        if (!is.list(res)) stop("Result of http request no list (not JSON)")
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
#' @param verbose logical, defaults to `FALSE`. If set `TRUE` the
#'        some messages are printed.
#' @param raw logical, defaults to `FALSE` (see Return).
#'
#' @return A data frame with a series of properties from the
#' collections (if `raw = FALSE`), else a list of lists with
#' the unformatted return from the API. Each element of the
#' list is the result of one API request (paging).
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
sg_collections <- function(verbose = FALSE, raw = FALSE) {

    stopifnot("argument 'raw' must be TRUE or FALSE" = isTRUE(raw) || isFALSE(raw))

    if (verbose) message("Retrieving collections")

    # Downloading the data from the API. Each time the API returns up to
    # 100 items, paging = TRUE calls the API until all items are fetched.
    url <- sg_api_url("collections")
    res <- get_request(url, paging = TRUE, verbose = verbose)

    extractfun <- function(x) {
        stopifnot(is.list(x))
        take <- c("id", "title", "description", "license",
                  "created", "updated")
        res <- x[take[take %in% names(x)]]
        if (!is.null(x[[c("summaries", "proj:epsg")]]))
            res$crs <- x[[c("summaries", "proj:epsg")]][[1]]
        return(res)
    }

    if (raw) return(res)

    if (verbose) message("Preparing data frame for the return")
    res <- lapply(res, function(x) bind_rows(lapply(x$collections, extractfun)))
    res <- as.data.frame(bind_rows(res))

    # Converting columns containing ISO 8601 datetime information
    # from character to POSIXct
    res <- autoconvert_datetime(res)
    return(res)
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
#' @return If `raw = TRUE` a list of lists is returned with the
#' raw (json decoded) result from the API calls. Each element in
#' the list corresponds to one API call (paging).
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
    url <- sg_api_url(c("collections", id, "items"))

    if (verbose) message("Retrieving items")

    # Downloading the data from the API. Each time the API returns up to
    # 100 items, paging = TRUE calls the API until all items are fetched.
    res <- get_request(url, paging = TRUE, verbose = verbose)

    # RAW results requested? Job done ...
    if (raw) return(res)

    # Else we try to prepare the data in an R object
    type <- sapply(res, function(x) x$type)
    if (!all(type == type[[1]])) {
        # TODO: Is that possible?
        stop("Not all item type identical, got ", paste(unique(type), collapse = ", "))
    }
    type <- type[[1]] # Our type

    # Feature type? Create simple features data.frame
    if (type == "FeatureCollection") {
        # Extracting features
        res <- do.call(rbind, lapply(res, items_extract_features))
    }

    # Convert data frame to simple features data frame
    return(st_as_sf(res, coords = c("lon", "lat"), crs = st_crs(4326)))
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
#' @return If `raw = TRUE` a list of lists is returned with the
#' raw (json decoded) result from the API calls. Each element in
#' the list corresponds to one API call (paging).
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
    url <- sg_api_url(c("collections", id, "assets"))
    print(url)

    if (verbose) message("Retrieving items")

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
    return(as.data.frame(autoconvert_datetime(res)))
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
##      url <- sg_api_url(c("collections", id, "assets"))
##
##      if (verbose) message("Retrieving items")
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

