# storr.server

> Storr Server/Client over HTTP

[![Linux Build Status](https://travis-ci.org/richfitz/storr.server.svg?branch=master)](https://travis-ci.org/richfitz/storr.server)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/github/richfitz/storr.server?svg=true)](https://ci.appveyor.com/project/richfitz/storr.server)

Serve (and recieve data from) a storr client over HTTP.

## Why?

Allows setting up a data server between two or more R processes, across networks, between different directories, or (eventually) between R and other languages (e.g. Python) using JSON as a serialisation format.

Most data comes back and forth in plain text or in JSON.  The objects come across as binary data by default but can be sent and recieved as JSON using `jsonlite` to serialise/deserialise.

## How?

Server:

``` r
st <- storr::storr_environment() # or any other storr object
storr.server::storr_server(st)
## [2016-01-14 14:51:35] Serving from a environment storr
## [2016-01-14 14:51:35] Starting server on http://0.0.0.0:8888
```

Client:

``` r
st <- storr.server::storr_http()
st$set("foo", runif(10))
st$get("foo")
##  [1] 0.31811455 0.56751914 0.26933154 0.18951628 0.88103402 0.28187348
##  [7] 0.63482631 0.21600813 0.14539382 0.03464149
```

On server, client requests are logged

``` r
## [2016-01-14 14:53:54] GET driver/exists/06c7429911b9de30dc4dffaba57ce059
## [2016-01-14 14:53:54] 200
## [2016-01-14 14:53:54] POST driver/object/06c7429911b9de30dc4dffaba57ce059
## [2016-01-14 14:53:54] 200
## [2016-01-14 14:53:54] POST driver/hash/objects/foo
## [2016-01-14 14:53:54] 200
## [2016-01-14 14:54:00] GET driver/exists/objects/foo
## [2016-01-14 14:54:00] 200
## [2016-01-14 14:54:00] GET driver/hash/objects/foo
## [2016-01-14 14:54:00] 200
```

## API documentation

...coming...

## Installation

```r
devtools::install_github("richfitz/storr.server")
```

## Usage

```r
library(storr.server)
```

## License

MIT + file LICENSE © [Rich FitzJohn](https://github.com/richfitz).
