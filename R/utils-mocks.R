assert_internet <- function(...) prettycheck::assert_internet()

cluster_map <- function(
  cl = NULL,
  fun,
  ...,
  MoreArgs = NULL, #nolint
  RECYCLE = TRUE, #nolint
  SIMPLIFY = FALSE, #nolint
  USE.NAMES = TRUE, #nolint
  .scheduling = c("static", "dynamic")
) {
  parallel::clusterMap(
    cl = cl,
    fun = fun, ...,
    MoreArgs = MoreArgs,
    RECYCLE = RECYCLE,
    SIMPLIFY = SIMPLIFY,
    USE.NAMES = USE.NAMES,
    .scheduling = .scheduling
  )
}

curl_download <- function(
  url,
  destfile,
  quiet = TRUE,
  mode = "wb",
  handle = curl::new_handle()
) {
  curl::curl_download(
    url = url,
    destfile = destfile,
    quiet = quiet,
    mode = mode,
    handle = handle
  )
}

curl_fetch_memory <- function(url, handle = curl::new_handle()) {
  curl::curl_fetch_memory(url = url, handle = handle)
}

from_json <- function(
  txt,
  simplifyVector = TRUE, #nolint
  simplifyDataFrame = simplifyVector, #nolint
  simplifyMatrix = simplifyVector, #nolint
  flatten = FALSE,
  ...
) {
  jsonlite::fromJSON(txt,
    simplifyVector = TRUE,
    simplifyDataFrame = simplifyVector,
    simplifyMatrix = simplifyVector,
    flatten = FALSE,
    ...
  )
}

make_cluster <- function(spec, ...) parallel::makeCluster(spec = spec, ...)
path_abs <- function(...) fs::path_abs(...)

raw_to_char <- function(x, multiple = FALSE) {
  rawToChar(x = x, multiple = multiple)
}

read_json <- function(path, simplifyVector = FALSE, ...) {  #nolint
  jsonlite::read_json(path, simplifyVector = FALSE, ...)
}

stop_cluster <- function(cl = NULL) parallel::stopCluster(cl = cl)
