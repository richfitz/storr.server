content_type_binary <- function() {
  httr::content_type("application/octet-stream")
}

from_json <- function(x, empty=list()) {
  ret <- jsonlite::fromJSON(x)
  if (identical(ret, list())) {
    ret <- empty
  }
  ret
}

client_base_url <- function(host, port) {
  if (!(is.character(host) && length(host) == 1L && !is.na(host))) {
    stop("host must be a scalar non-NA character string")
  }
  if (!(is.numeric(port) && length(port) == 1L && !is.na(port))) {
    stop("port must be a scalar non-NA character string")
  }
  sprintf("http://%s:%d", host, port)
}

storr_client_method <- function(base_url, endpoint, path=NULL,
                                method=httr::GET, ...,
                                error=TRUE,
                                content=TRUE,
                                empty=NULL) {
  if (is.null(path)) {
    path <- file.path(base_url, endpoint)
  } else {
    path <- file.path(base_url, endpoint, paste(path, collapse="/"))
  }
  r <- method(path, ...)
  if (error) {
    httr::stop_for_status(r)
  }
  if (content) {
    type <- httr::headers(r)[["content-type"]]
    if (type == "application/octet-stream") {
      res <- unserialize(httr::content(r, "raw"))
    } else {
      ## NOTE: The encoding line here is required to avoid printing a
      ## message in recent (development) httr.
      res <- httr::content(r, "text", encoding="UTF-8")
      if (type == "application/json") {
        res <- from_json(res, empty)
      }
    }
    res
  } else {
    r
  }
}

storr_client_unsupported <- function(method) {
  stop(sprintf("%s is unsupported with storr_client"), method)
}
