

#' Download (Cache) and Import Assets/Data Sets
#'
#' @param x data frame (with one row) or list containing
#'        at least the following variables: id, type, href, file_checksum.
#' @param dir character, name/path to a directory to cache the
#'        data sets (see 'Details' for more information).
#' @param language character, one of `"all"` (default), `"en"`,
#'        `"de"`, `"fr"`, or `"it"`. If set different to `"all"`
#'        columns ending in `_<lang>` not matching the requested
#'        language will be removed.
#' @param verbose logical, defaults to `FALSE`. If set `TRUE` the
#'        some messages are printed.
#'
#' @details Assets (data sets) provided via geo.admin.ch come
#' with a file checksum which can be used to check if a file
#' changed. We use this to cache files.
#'
#' If a directory is specified (`dir`) it will be checked if that file has
#' already been downloaded and stored in the directory. In this situation, the
#' local file is used rather than downloading the data. If not, the asset will
#' be downloaded and stored in the directory specified so that it can be
#' potentially used again next time.
#'
#' Please note that the the size of the directory can grow
#' fast, especially if this mechanic is used when downloading/accessing
#' data sets that rapidly change (i.e., most recent data) as
#' a new file will be created every time the checksum changes.
#'
#' @author Reto
#' @export
#'
#' @importFrom httr GET write_disk status_code
#' @importFrom utils read.csv2
#' @importFrom dplyr as_tibble
sg_download_asset <- function(x, dir = NULL, language = c("all", "en", "de", "fr", "it"), verbose = FALSE) {

    # Sanity checks
    stopifnot(
        "argument 'x' must be a data frame or list" = is.list(x),
        "argument 'dir' must be NULL or single character" =
            is.null(dir) || (is.character(dir) && length(dir) == 1L),
        "argument 'verbose' must be logical TRUE or FALSE" =
            isTRUE(verbose) || isFALSE(verbose)
    )
    x <- as.list(x)
    language <- match.arg(language)

    # Check if directory exists
    if (!is.null(dir) && !dir.exists(dir)) stop("directory \"", dir, "\" not found")

    # Check that we have all required variables in our data set
    expected <- c("id", "type", "href", "file_checksum")
    if (!all(expected %in% names(x)))
        stop("missing at least one variable in 'x' (", paste(expected, collapse = ", "))
    # Must all be of length 1
    x <- lapply(x[expected], as.character)
    stopifnot(
        "all vectors in 'x' must be of length 1L" = all(sapply(x, length) == 1L),
        "empty characters in 'x' not allowed" = all(sapply(x, nchar) > 0L)
    )

    # Define local file name (NULL if not used)
    x$local_file <- if (is.null(dir)) {
        NULL
    } else {
        file.path(dir, sprintf("%s_%s", x$file_checksum, x$id))
    }

    # Shall we download and store locally?
    if (!is.null(x$local_file) && !file.exists(x$local_file)) {
        if (verbose) message("Downloading ", x$id, " and store to ", x$local_file)
        req <- GET(x$href, write_disk(x$local_file))
        show_http_status_and_terminate(status_code(req), xtra = x)
    } else if (file.exists(x$local_file)) {
        if (verbose) message("Using ", x$local_file)
    }

    # Importing file
    if (x$type == "text/csv") {
        tmp <- if (!is.null(x$local_file) && !file.exists(x$local_file)) x$local_file else x$href
        res <- as_tibble(read.csv2(tmp, dec = ".", fileEncoding = "Latin1",
                                   na.strings = c("NA", "")))
        res <- autoconvert_datetime(res)

        # Removing unquired language columns
        res <- remove_language_cols(res, language)
    }

    return(res)
}


#' Removing Non-required Language-specific Variables
#'
#' @param x data frame to be checked and potentially modified.
#' @param lang character, languages to keep. If `"all"` no
#'        modifications are done. Else trying to remove
#'        all columns not matching the requested `lang`.
#'
#' @return A data frame, potentially modified by removing
#' all columns not matching the requested language.
#'
#' @author Reto
remove_language_cols <- function(x, lang) {
    if (lang == "all") return(x)

    # Else we remove columns/variables ending in:
    languages <- c("en", "de", "fr", "it")
    remove <- languages[!languages == lang]

    # Columns to remove
    pattern <- sprintf(".*_(%s)$", paste(remove, collapse = "|"))
    keep    <- !grepl(pattern, names(x))

    return(x[, keep])
}




