# These functions were created to be used with the 'mockr' package.
# Sort by type or alphabetical order.

cluster_map <- function(cl = NULL, fun, ..., MoreArgs = NULL, RECYCLE = TRUE,
                        SIMPLIFY = FALSE, USE.NAMES = TRUE,
                        .scheduling = c("static", "dynamic")) {
    parallel::clusterMap(
        cl = cl, fun = fun, ..., MoreArgs = MoreArgs, RECYCLE = RECYCLE,
        SIMPLIFY = SIMPLIFY, USE.NAMES = USE.NAMES,
        .scheduling = .scheduling
    )
}

make_cluster <- function(spec, ...) {
    require_pkg("parallel")

    parallel::makeCluster(spec = spec, ...)
}

stop_cluster <- function(cl = NULL) {
    require_pkg("parallel")

    parallel::stopCluster(cl = cl)
}

curl_download <- function(url, destfile, quiet = TRUE, mode = "wb",
                          handle = curl::new_handle()) {
    require_pkg("curl")

    curl::curl_download(
        url = url, destfile = destfile, quiet = quiet, mode = mode,
        handle = handle
    )
}

curl_fetch_memory <- function(url, handle = curl::new_handle()) {
    require_pkg("curl")

    curl::curl_fetch_memory(url = url, handle = handle)
}

has_internet <- function(...) {
    require_pkg("curl")

    curl::has_internet()
}

from_json <- function(txt, simplifyVector = TRUE,
                      simplifyDataFrame = simplifyVector,
                      simplifyMatrix = simplifyVector, flatten = FALSE,
                      ...) {
    require_pkg("jsonlite")

    jsonlite::fromJSON(txt, simplifyVector = TRUE,
                       simplifyDataFrame = simplifyVector,
                       simplifyMatrix = simplifyVector, flatten = FALSE,
                       ...)
}

read_json <- function(path, simplifyVector = FALSE, ...) {
    require_pkg("jsonlite")

    jsonlite::read_json(path, simplifyVector = FALSE, ...)
}

raw_to_char <- function(x, multiple = FALSE) {
    rawToChar(x = x, multiple = multiple)
}

is_interactive <- function(...) {
    interactive()
}

require_namespace <- function(x, ..., quietly = TRUE) {
    requireNamespace(x, ..., quietly = quietly)
}
