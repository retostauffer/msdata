

.onLoad <- function(libname, pkgname) {
    # Specify default API version; this would allow
    # to overwrite the version without updating
    # the package for testing. Same with the API base URL.

    # Currently version 1 (v1); can be overruled by overwriting
    # the option using `options(msdata.apiurl = ...)`.
    apiurl <- sprintf("https://data.geo.admin.ch/api/stac/v%d", 1L)
    options("msdata.apiurl" = apiurl)

    message("   TODO: On load message to the API and how to cite")
}
