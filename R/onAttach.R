

.onAttach <- function(libname, pkgname) {
    # Specify default API version; this would allow
    # to overwrite the version without updating
    # the package for testing. Same with the API base URL.

    # Currently version 1 (v1); can be overruled by overwriting
    # the option using `options(swissgeo.apiurl = ...)`.
    apiurl <- sprintf("https://data.geo.admin.ch/api/stac/v%d", 1L)
    options("swissgeo.apiurl" = apiurl)

    packageStartupMessage("
        TODO: Startup message with some information to geo.admin.ch and
        how to cite the data if used, though that is tricky as it is
        different for different data sets and must be found somewhere
        on the data providers home page.
    ")
}
