start_storr_server <- function() {
  outfile <- tempfile()
  cl <- parallel::makeCluster(1L, "PSOCK", outfile=outfile)
  pid <- parallel::clusterEvalQ(cl, Sys.getpid())[[1]]

  parallel:::sendCall(cl[[1L]], "source", list("storr_server", echo=TRUE))
  attr(cl, "outfile") <- outfile
  attr(cl, "pid") <- pid
  cl
}

stop_storr_server <- function(cl) {
  pid_kill(attr(cl, "pid"), 9L)
  close(cl[[1]]$con)
}

skip_if_no_storr_server <- function() {
  if (storr_server_available()) {
    return()
  }
  skip("storr_server not running")
}

## Needed for testing
hash_object <- storr:::hash_object

.driver_create <- driver_http
.driver_name <- "http"
