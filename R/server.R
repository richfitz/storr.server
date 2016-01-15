##' Create a storr server, powered by HTTP.
##'
##' This is not fast; expect throughput on the order of 100-200
##' requests per second.
##'
##' The API is not currently documented, though it will be once I'm
##' sure I've got this right.
##'
##' @title Run a storr server
##'
##' @param st A storr object to serve.  The storr can be empty to
##'   start (e.g., a new call to \code{storr::storr_environment()}
##' @param host A string that is a valid IPv4 address, or "0.0.0.0" to
##'   listen on all IP addresses (which is the default)
##' @param port Port number to listened on.
##' @param verbose Print information about
##' @param loop Run the server in a loop (using
##'   \code{httpuv::runServer}).  If \code{FALSE}, you will have to
##'   handle requests yourself with \code{httpuv::service}.
##' @export
storr_server <- function(st, host="0.0.0.0", port=8888, verbose=TRUE,
                         loop=TRUE) {
  app <- storr_server_app(st, verbose)
  base_url <- client_base_url(host, port)
  report_url <- sub("0.0.0.0", "127.0.0.1", base_url, fixed=TRUE)
  storr_server_log(verbose, paste0("Starting server on ", report_url))
  if (loop) {
    httpuv::runServer(host, port, app)
  } else {
    httpuv::startServer(host, port, app)
  }
}

## TODO: This could be heaps nicer by using one of the microframeworks
## like jug or prairie (previously dull).  However, swapping this out
## should be fairly straightforward so I'll continue as I am for now.
##
## TODO: too many storr_ prefixes on local functions.  Replace
## storr_server -> server and storr_client -> client on non-exported
## functions.
##
## TODO: Serve a help page (from inst/) when requesting index.
storr_server_app <- function(st, verbose) {
  if (!inherits(st, "storr")) {
    stop("Expected a storr")
  }
  storr_server_log(verbose, "Serving from a %s storr", st$driver$type())
  state <- new.env(parent=emptyenv())
  list(
    call=function(req) {
      dat <- parse_req(req)
      storr_server_log(verbose, "%s %s/%s",
                       crayon::red(dat$verb),
                       crayon::yellow(dat$endpoint),
                       crayon::cyan(paste(dat$path, collapse="/")))
      execute <- switch(
        dat$endpoint,
        ## driver:
        "driver/hash"=storr_server_endpoint_driver_hash,
        "driver/object"=storr_server_endpoint_driver_object,
        "driver/exists"=storr_server_endpoint_driver_exists,
        "driver/list"=storr_server_endpoint_driver_list,
        "driver/list_hashes"=storr_server_endpoint_driver_list_hashes,
        ## server:
        "keys"=storr_server_endpoint_keys,
        "namespaces"=storr_server_endpoint_namespaces,
        "hashes"=storr_server_endpoint_hashes,
        "exists"=storr_server_endpoint_exists,
        ## server admin
        "admin"=storr_server_endpoint_admin,
        storr_server_endpoint_unknown)
      ret <- execute(st, dat)
      col <- if (ret$status < 300) crayon::green else crayon::red
      storr_server_log(verbose, col(as.character(ret$status)))
      ret
    })
}

parse_req <- function(req) {
  ## request options are going to to be
  ##    application/json
  ##    application/octet-stream
  ## but parsing that stuff is hard so I am ignoring it for now.
  x <- req$PATH_INFO
  verb <- req$REQUEST_METHOD
  re <- "^/((driver/)?[^/]+)/?(.*)"
  endpoint <- sub(re, "\\1", x)
  driver <- grepl("^driver/", endpoint)
  path <- sub(re, "\\3", x)
  path_split <- strsplit(path, "/", fixed=TRUE)[[1]]
  ret <- list(verb=verb, endpoint=endpoint, driver=driver,
              path=path, path_split=path_split, req=req)
  if (!is.null(req$HTTP_ACCEPT)) {
    ret$accept <- req$HTTP_ACCEPT
  }
  if (!is.null(req$HTTP_CONTENT_TYPE)) {
    ret$content_type <- req$HTTP_CONTENT_TYPE
  }

  if (verb == "POST") {
    con <- req[["rook.input"]]
    if (!is.null(con)) {
      ret$data_raw <- con$read()
      if (length(ret$data_raw) == 0L) { # NullInputStream
        ret$data <- NULL
      } else if (!is.null(ret$content_type)) {
        ## TODO: needs to be checked...
        if (ret$content_type == "application/json") {
          ret$data <- jsonlite::fromJSON(rawToChar(ret$data_raw),
                                         simplifyDataFrame=FALSE)
        } else if (ret$content_type == "text/plain") {
          ret$data <- rawToChar(ret$data_raw)
        } else if (ret$content_type == "application/octet-stream") {
          ret$data <- unserialize(ret$data_raw)
        }
      } else {
        if (any(ret$data_raw == 0L)) {
          ret$data <- unserialize(ret$data_raw)
        } else {
          ret$data <- charToRaw(ret$data_raw)
        }
      }
    }
  }
  ret
}

storr_server_endpoint_keys <- function(st, dat) {
  path <- dat$path_split
  np <- length(path)
  if (dat$verb == "GET") {
    if (np == 2L) {
      type <- storr_server_reply_type(dat$accept)
      tryCatch(
        ## TODO: query string as options to toJSON (lots!)
        storr_server_http_response(st$get(path[[2]], path[[1]]), type=type),
        KeyError=function(e) {
          storr_server_http_response(e$message, 404L)
        })
    } else if (np == 1L) {
      storr_server_http_response(st$list(path),
                                 type="application/json")
    } else {
      storr_server_http_error()
    }
  } else if (dat$verb == "POST") {
    if (np == 2L) {
      storr_server_http_response(st$set(path[[2]], dat$data, path[[1]]),
                                 type="application/json")
    } else {
      storr_server_http_error()
    }
  } else if (dat$verb == "DELETE") {
    if (np == 2L) {
      storr_server_http_response(st$del(path[[2]], path[[1]]),
                                 type="application/json")
    } else if (np == 1L) {
      storr_server_http_response(st$clear(path[[1]]),
                                 type="application/json")
    } else {
      storr_server_http_error()
    }
  } else {
    storr_server_http_error()
  }
}

storr_server_endpoint_exists <- function(st, dat) {
  path <- dat$path_split
  np <- length(path)
  if (dat$verb == "GET") {
    if (np == 2L) {
      storr_server_http_response(st$exists(path[[2]], path[[1]]))
    } else if (np == 1L) {
      storr_server_http_response(st$exists_hash(path[[1]]))
    } else {
      storr_server_http_error()
    }
  } else {
    storr_server_http_error()
  }
}

storr_server_endpoint_namespaces <- function(st, dat) {
  path <- dat$path_split
  np <- length(path)
  if (dat$verb == "GET" && np == 0L) {
    storr_server_http_response(st$list_namespaces())
  } else {
    storr_server_http_error()
  }
}

storr_server_endpoint_hashes <- function(st, dat) {
  path <- dat$path_split
  np <- length(path)
  if (dat$verb == "GET") {
    if (np == 0L) {
      storr_server_http_response(st$list_hashes())
    } else if (np == 2L) {
      storr_server_http_response(st$get_hash(path[[2]], path[[1]]))
    } else if (np == 1L) {
      storr_server_http_response(st$get_value(path[[1]]))
    } else {
      storr_server_http_error()
    }
  } else if (dat$verb == "POST") {
    if (np == 1L) {
      storr_server_http_response(st$set_by_value(dat$data, path[[1]]))
    }
  } else {
    storr_server_http_error()
  }
}

storr_server_endpoint_driver_hash <- function(st, dat) {
  dr <- st$driver
  path <- dat$path_split
  np <- length(path)
  if (np != 2L) {
    return(storr_server_http_error())
  }
  key <- path[[2]]
  namespace <- path[[1]]
  if (dat$verb == "GET") {
    storr_server_http_response(dr$get_hash(key, namespace))
  } else if (dat$verb == "POST") {
    dr$set_hash(key, namespace, dat$data)
    storr_server_http_response(NULL)
  } else if (dat$verb == "DELETE") {
    ## TODO: in storr, del_key -> del_hash
    storr_server_http_response(dr$del_key(key, namespace))
  } else {
    storr_server_http_error()
  }
}

storr_server_endpoint_driver_object <- function(st, dat) {
  dr <- st$driver
  path <- dat$path_split
  np <- length(path)
  if (np != 1L) {
    return(storr_server_http_error())
  }
  hash <- path[[1]]
  if (dat$verb == "GET") {
    type <- storr_server_reply_type(dat$accept)
    storr_server_http_response(dr$get_object(hash), type=type)
  } else if (dat$verb == "POST") {
    dr$set_object(hash, dat$data)
    storr_server_http_response(NULL)
  } else if (dat$verb == "DELETE") {
    ## TODO: in storr, del_hash -> del_object
    storr_server_http_response(dr$del_hash(hash))
  }
}

storr_server_endpoint_driver_exists <- function(st, dat) {
  dr <- st$driver
  path <- dat$path_split
  np <- length(path)
  if (dat$verb == "GET") {
    if (np == 2L) {
      storr_server_http_response(dr$exists_key(path[[2]], path[[1]]))
    } else if (np == 1L) {
      storr_server_http_response(dr$exists_hash(path[[1]]))
    } else {
      storr_server_http_error()
    }
  } else {
    storr_server_http_error()
  }
}

storr_server_endpoint_driver_list <- function(st, dat) {
  dr <- st$driver
  path <- dat$path_split
  np <- length(path)
  if (dat$verb == "GET") {
    if (np == 1L) {
      storr_server_http_response(dr$list_keys(path[[1]]))
    } else if (np == 0L) {
      storr_server_http_response(dr$list_namespaces())
    } else {
      storr_server_http_error()
    }
  } else {
    storr_server_http_error()
  }
}

storr_server_endpoint_driver_list_hashes <- function(st, dat) {
  dr <- st$driver
  path <- dat$path_split
  np <- length(path)
  if (dat$verb == "GET" && np == 0L) {
    storr_server_http_response(dr$list_hashes())
  } else {
    storr_server_http_error()
  }
}

storr_server_endpoint_admin <- function(st, dat) {
  path <- dat$path_split

  if (identical(path, "ping")) {
    storr_server_http_response("pong")
  } else if (identical(path, "version")) {
    storr_server_http_response(
      list(storr=as.character(packageVersion("storr")),
           storr.server=as.character(packageVersion("storr.server"))))
  } else if (identical(path, "restart") && dat$verb == "POST") {
    restart <- getOption("storr.server.admin.restart", NULL)
    if (is.function(restart)) {
      dr <- restart()
      if (identical(class(dr), class(st$driver))) {
        st$driver$destroy()
        st$flush_cache()
        st$driver <- dr
        storr_server_http_response(TRUE)
      } else {
        storr_server_http_error("Server is misconfigured")
      }
    } else {
      storr_server_http_response(FALSE)
    }
  } else {
    storr_server_endpoint_unknown(st, dat)
  }
}

storr_server_endpoint_unknown <- function(st, dat) {
  storr_server_http_error("Unknown endpoint", 404L)
}

storr_server_http_response <- function(body, status=200L,
                                       type="application/json") {
  if (type == "application/json" && !inherits(body, "json")) {
    body <- jsonlite::toJSON(body, auto_unbox=TRUE)
  } else if (type == "application/octet-stream" && !is.raw(body)) {
    body <- serialize(body, NULL)
  }
  list(status=status, headers=list("Content-Type"=type), body=body)
}
storr_server_http_error <- function(message="Invalid request", status=400L,
                                    type="text/plain") {
  storr_server_http_response(message, status, type)
}

## TODO: This is not really enough; assuming cooperative clients
## here.  Basically; we'll reluctantly return json but would
## rather return binary.
storr_server_reply_type <- function(accept) {
  if (!is.null(accept) &&
      grepl("application/json", accept) &&
      !grepl("*/*", accept, fixed=TRUE)) {
    "application/json"
  } else {
    "application/octet-stream"
  }
}

storr_server_log <- function(verbose, fmt, ...) {
  if (verbose) {
    message(sprintf(paste0("[%s] ", fmt),
                    crayon::silver(as.character(Sys.time())), ...))
  }
}
