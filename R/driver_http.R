##' Storr http client and driver.
##'
##' @title Storr http client
##' @param host Host of the storr server
##' @param port Port of the storr server
##' @param mangle_key As with \code{readRDS}; mangle the keys so that
##'   URL/filename safe characters are used.  Uses \code{storr::encode64}.
##' @param default_namespace Default namespace (see
##'   \code{\link{storr}}).
##' @export
storr_http <- function(host="localhost", port=8888, mangle_key=FALSE,
                       default_namespace="objects") {
  storr::storr(driver_http(host, port, mangle_key), default_namespace)
}

##' @export
##' @rdname storr_http
##' @importFrom R6 R6Class
driver_http <- function(host="localhost", port=8888, mangle_key=FALSE) {
  .R6_driver_http$new(host, port, mangle_key)
}

.R6_driver_http <- R6::R6Class(
  "driver_http",

  public=list(
    base_url=NULL,
    mangle_key=NULL,
    initialize=function(host, port, mangle_key) {
      self$base_url <- client_base_url(host, port)
      self$mangle_key <- mangle_key
    },

    type=function() {
      "http"
    },

    destroy=function() {
      storr_client_method(self$base_url, "admin", "restart", method=httr::POST)
    },

    get_hash=function(key, namespace) {
      storr_client_method(self$base_url, "driver/hash",
                          self$name_key(key, namespace))
    },
    set_hash=function(key, namespace, hash) {
      storr_client_method(self$base_url, "driver/hash",
                          self$name_key(key, namespace),
                          body=hash,
                          content_type_plain(),
                          method=httr::POST,
                          empty=NULL)
      invisible(NULL)
    },
    get_object=function(hash) {
      storr_client_method(self$base_url, "driver/object", hash)
    },
    set_object=function(hash, value) {
      storr_client_method(self$base_url, "driver/object", hash,
                          body=serialize(value, NULL),
                          content_type_binary(),
                          method=httr::POST,
                          empty=NULL)
    },

    exists_key=function(key, namespace) {
      storr_client_method(self$base_url, "driver/exists",
                          self$name_key(key, namespace))
    },
    exists_hash=function(hash) {
      storr_client_method(self$base_url, "driver/exists", hash)
    },

    del_key=function(key, namespace) {
      storr_client_method(self$base_url, "driver/hash",
                          self$name_key(key, namespace),
                          method=httr::DELETE)
    },
    del_hash=function(hash) {
      storr_client_method(self$base_url, "driver/object", hash,
                          method=httr::DELETE)
    },

    list_hashes=function() {
      storr_client_method(self$base_url, "driver/list_hashes",
                          empty=character())
    },
    list_keys=function(namespace) {
      ret <- storr_client_method(self$base_url, "driver/list", namespace,
                                 empty=character())
      if (self$mangle_key) storr::decode64(ret, TRUE) else ret
    },
    list_namespaces=function() {
      storr_client_method(self$base_url, "driver/list",
                          empty=character())
    },

    ping=function() {
      storr_client_method(self$base_url, "admin", "ping")
    },
    version=function() {
      storr_client_method(self$base_url, "admin", "version")
    },
    name_key=function(key, namespace) {
      if (self$mangle_key) {
        key <- storr::encode64(key)
      }
      c(namespace, key)
    }
  ))

content_type_plain <- function() {
  httr::content_type("text/plain")
}

##' @export
##' @rdname storr_http
##' @param ... Arguments passed to \code{driver_http}
storr_server_available <- function(...) {
  !inherits(try(driver_http(...)$ping(), silent=TRUE), "try-error")
}
