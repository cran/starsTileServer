## ---- include = FALSE---------------------------------------------------------
NOT_CRAN <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = NOT_CRAN
)

## ----download, echo=FALSE-----------------------------------------------------
if (!file.exists(tmpGridFile <- "~/ownCloudUva/test.nc")) {
  tmpGridFile <- tempfile(fileext = ".nc")
  download.file("https://surfdrive.surf.nl/files/index.php/s/Z6YoTyzyyAsmgGS/download", tmpGridFile, extra = "-q", method = "wget")
}

## ----asfdf--------------------------------------------------------------------
windDirFun <- function(grd,
                       level = c("875", "900", "925"),
                       time = "2000-04-28 23:00:00") {
  weather <- read_stars(options("tmpGridFile")[[1]], proxy = T)
  st_crs(weather) <- 4326
  bbox <- st_bbox(
    st_transform(
      st_as_sf(grd),
      st_crs(weather)
    )
  )
  levelDim <- which(as.character(st_get_dimension_values(weather, "level")) == level[1])
  timeDim <- which(as.character(st_get_dimension_values(weather, "time")) == time[1])
  u <-
    abind::adrop(st_warp(st_crop(st_as_stars(
      weather["u"] %>%
        slice(level, levelDim) %>%
        slice(time, timeDim)
    ), bbox), grd))
  v <-
    abind::adrop(st_warp(st_crop(st_as_stars(
      weather["v"] %>%
        slice(level, levelDim) %>%
        slice(time, timeDim)
    ), bbox), grd))
  return(sqrt(u^2 + v^2))
}

## ----asdf---------------------------------------------------------------------

colFun <- function(x, alpha = 1, maxColor = 25) {
  cfun <- leaflet::colorNumeric("RdYlBu", domain = c(-as.numeric(maxColor), 0))
  paste0(
    suppressWarnings(cfun(-x)),
    as.character(as.raw(as.numeric(alpha) * 255))
  )
}
attr(colFun, "colorType") <- "numeric"

## ----setup--------------------------------------------------------------------
library(starsTileServer)
require(callr)

rp <- r_bg(args = list(tmpGridFile = tmpGridFile, windDirFun = windDirFun, colFun = colFun), function(tmpGridFile, windDirFun, colFun) {
  require(sf)
  require(stars)
  require(dplyr)
  options(tmpGridFile = tmpGridFile)
  starsTileServer::starsTileServer$new(windDirFun,
    colorFun = colFun
  )$run(port = 5645, docs = T)
})

## ----startupPause, echo=F-----------------------------------------------------
Sys.sleep(35)
stopifnot(rp$is_alive())

## ----plot_map-----------------------------------------------------------------
require(leaflet)
require(leaflet.extras)
require(magrittr)
m <- leaflet() %>%
  addTiles() %>%
  enableTileCaching() %>%
  addTiles(
    "http://127.0.0.1:5645/map/{z}/{x}/{y}?level=900&alpha=.2&time=2000-04-28 23:00:00",
    options = tileOptions(useCache = TRUE, crossOrigin = TRUE)
  ) %>%
  addLegend(pal = readRDS(url("http://127.0.0.1:5645/map/colorfunctionnoalpha")), values = 0:20) %>%
  setView(zoom = 3, lat = 30, lng = 5)

## ----plot_map_image, echo=FALSE-----------------------------------------------
f <- tempfile(fileext = ".png")
mapview::mapshot(m, file = f, delay = 11, vwidth = 700, vheight = 600)
magick::image_read(f)

## ----remove_server------------------------------------------------------------
message(rp$read_error())
message(rp$read_output())
rp$finalize()

