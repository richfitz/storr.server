context("driver_http")

test_that("basic", {
  skip_if_no_storr_server()
  dr <- driver_http()
  expect_equal(dr$ping(), "pong")
  expect_equal(dr$version(),
               list(storr=as.character(packageVersion("storr")),
                    storr.server=as.character(packageVersion("storr.server"))))
})

message()

test_that("spec", {
  skip_if_no_storr_server()
  driver_http()$destroy()
  storr::test_driver(function() driver_http())
})
