

#' Assets
#'
#' Each item can provide multiple assets. An asset specifies the type of data
#' (e.g., historical, recent) as well as the temporal resolution of the data
#' and the data period (if suitable). The `sg_assets` class is used to handle
#' these assets within the package.
#'
#' @param x list of named lists containing the asset details.
#'
#' @return An asset data frame with the name of the asset and all provided
#' additional information.
#'
#' @author Reto
#'
#' @importFrom dplyr bind_rows
assets <- function(x) {
    stopifnot(
        "argument 'x' must be an unnamed list (of assets)" =
            is.list(x) && is.null(names(x)),
        "elements in 'x' must be named lists" = 
            all(lapply(x, function(y) is.list(y) && !is.null(names(y))))
    )

    fn <- function(x) cbind(data.frame(name = names(x)), bind_rows(x))
    return(structure(lapply(x, fn), class = "assets"))
}


#' @param x object of class 'assets'.
#' @exportS3Method format assets
#' @rdname assets
#' @author Reto
format.assets <- function(x, ...) {
    sprintf("assets: %d", sapply(x, function(y) length(y[[1]])))
}

#' @exportS3Method print assets
#' @rdname assets
#' @author Reto
print.assets <- function(x, ...) {
    print(sprintf("assets: %d", as.numeric(x)))
    invisible(x)
}

#' @param i elements to extract or replace. Numeric values coerced
#'        to integer identifying the `i`th element(s) for subsetting.
#'
#' @exportS3Method `[` assets
#' @rdname assets
#' @author Reto
`[.assets` <- function(x, i, ...) {
    out <- NextMethod()
    return(structure(out, class = class(x)))
}

#' @exportS3Method as.double assets
#' @rdname assets
#' @author Reto
as.double.assets <- function(x, ...) sapply(x, function(y) length(y[[1]]))

#' @exportS3Method names assets
#' @rdname assets
#' @author Reto
names.assets <- function(x) {
    res <- lapply(x, function(y) y$name)
    return(if (length(res) == 1L) res[[1]] else res)
}

#' @param row.names `NULL` (default) or a character vector giving the row names for
#'        the data frame. Only used if `x` is of length 1L.
#' @param optional currently ignored.
#' @param \dots currently ignored.
#'
#' @exportS3Method as.data.frame assets
#' @rdname assets
#' @author Reto
as.data.frame.assets <- function(x, row.names = NULL, optional = NA, ...) {
    res <- lapply(x, function(y) structure(y, class = "data.frame"))
    res <- if (length(res) == 1L) res[[1]] else res
    if (!is.null(row.names) && is.data.frame(res))
        rownames(res) <- row.names
    return(res)
}
