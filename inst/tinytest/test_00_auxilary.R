# -------------------------------------------------------
# Checking auxilary functions/helper functions
# -------------------------------------------------------

if (interactive()) { library("tinytest"); library("swissgeo") }


# -------------------------------------------------------
# HTTP error handler function
# -------------------------------------------------------

# First at all, check that the function (not exported) exists
expect_true(is.function(swissgeo:::show_http_status_and_terminate),
            info = "Hidden function swissgeo:::show_http_status_and_terminate exists")

# Incorrect use/sanity checks
expect_error(swissgeo:::show_http_status_and_terminate("foo"),
             info = "Argument 'scode' expected to be numeric")
expect_error(swissgeo:::show_http_status_and_terminate(1:3),
             info = "Argument 'scode' expected to be of length 1L")
expect_error(swissgeo:::show_http_status_and_terminate(404, xtra = "foo"),
             info = "Argument 'xtra' expected to be NULL or a named list > 1L")
expect_error(swissgeo:::show_http_status_and_terminate(404, xtra = list(1, 2, 3)),
             info = "Argument 'xtra' expected to be NULL or a named list > 1L")
expect_error(swissgeo:::show_http_status_and_terminate(404, xtra = list()),
             info = "Argument 'xtra' expected to be NULL or a named list > 1L")


# If the status code is in the 200 range, the function
# simply returns NULL (no problem found)
expect_null(swissgeo:::show_http_status_and_terminate(200),
            info = "Returning NULL, successful request (200).")
expect_null(swissgeo:::show_http_status_and_terminate(200.0),
            info = "Returning NULL, successful request (200.0; numeric).")
expect_null(swissgeo:::show_http_status_and_terminate(202),
            info = "Returning NULL, successful request (202).")
expect_null(swissgeo:::show_http_status_and_terminate(299),
            info = "Returning NULL, successful request (299).")

# Testing some common errors
expect_error(swissgeo:::show_http_status_and_terminate(404),
             info = "HTTP response error 404")
expect_error(swissgeo:::show_http_status_and_terminate(503),
             info = "HTTP response error 503")

# Custom error for status codes not catched by the httr package
expect_error(swissgeo:::show_http_status_and_terminate(6020),
             pattern = "HTTP request error\\: server returned status code 6020",
             info = "HTTP response error (unknown/non-standard; 6020)")

# Custom message (xtra)
expect_null(swissgeo:::show_http_status_and_terminate(200, xtra = "foo"),
            info = "Returning NULL, successful request (200); argument 'xtra' has no effect.")


# Custom additional information via xtra (named list)
expect_error(swissgeo:::show_http_status_and_terminate(404, xtra = list(foo = "bar")),
             pattern = "foo\\:\\s+bar",
             info = "Error with custom message expected.")
expect_error(swissgeo:::show_http_status_and_terminate(404, xtra = list(foo = "bar", test = 12345)),
             pattern = "foo\\:\\s+bar.*test\\:\\s+12345",
             info = "Error with custom message expected.")
