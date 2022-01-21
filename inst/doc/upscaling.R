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
  download.file("https://surfdrive.surf.nl/files/index.php/s/XndqfCYJqkdjcFO/download", tmpGridFile, extra = "-q", method = "wget")
}

## -----------------------------------------------------------------------------
require(callr)
rp_list <- lapply(lapply(as.list(4000+1:2), c, list(tmpGridFile=tmpGridFile)), r_bg, func=function(port, tmpGridFile) {
  # read a stars grid
  weatherData <- stars::read_stars(tmpGridFile, proxy = FALSE, sub = "t")
  names(weatherData) <- "t"
  sf::st_crs(weatherData) <- "+proj=longlat"
  colorFunction <- leaflet::colorNumeric("viridis", c(250, 310))
  colorFunctionWithAlpa <- function(x, alpha = 1) {
    paste0(colorFunction(x), as.character(as.raw(
      as.numeric(alpha) * 255
    )))
  }
  starsTileServer::starsTileServer$new(weatherData, colorFun = colorFunctionWithAlpa)$run(port = port)
})



## -----------------------------------------------------------------------------
require(leaflet)
require(leaflet.extras)
map <- leaflet() %>%
  addTiles() %>%
  enableTileCaching() %>%
  addTiles(
    "http://127.0.0.1:400{s}/map/t/{z}/{x}/{y}?level=900&time=2000-04-27 01:00:00&alpha=0.5",
    options = tileOptions(useCache = TRUE, crossOrigin = TRUE, subdomains = '12')
  ) %>%
  setView(zoom = 3, lat = 30, lng = 30)

## ---- eval=FALSE--------------------------------------------------------------
#  map

## ----plot_map_image, echo=FALSE-----------------------------------------------
mapview::mapshot(map, file = f <- tempfile(fileext = ".png"), delay = 9, vwidth = 500, vheight = 400)
magick::image_read(f)

## -----------------------------------------------------------------------------
lapply(rp_list, function(x)x$read_output())
lapply(rp_list, function(x)x$finalize())


## ---- code=readLines(system.file("compose/Dockerfile", package = "starsTileServer")), eval=F----
#  FROM rocker/geospatial
#  MAINTAINER Bart
#  RUN install2.r -n 5 plumber stars; \
#  	rm -rf /tmp/downloaded_packages
#  RUN R --quiet -e 'install.packages("starsdata", repos = "http://pebesma.staff.ifgi.de", type = "source")'
#  RUN R --quiet   -e "remotes::install_gitlab('bartk/starsTileServer')"
#  EXPOSE 3436
#  COPY script.R script.R
#  RUN R --quiet   -e "source('script.R')"
#  ENTRYPOINT ["R", "--quiet", "-e", "server<-readRDS('server.rds') ;server$run( port=3436, host='0.0.0.0', swagger=T)"]

## ---- code=readLines(system.file("compose/script.R", package = "starsTileServer")), eval=F----
#  require(stars)
#  require(starsTileServer)
#  s5p <- system.file(
#    "sentinel5p/S5P_NRTI_L2__NO2____20180717T120113_20180717T120613_03932_01_010002_20180717T125231.nc",
#    package = "starsdata"
#  )
#  nit <- read_stars(
#    s5p,
#    along = NA,
#    sub = c(
#      "//PRODUCT/SUPPORT_DATA/DETAILED_RESULTS/nitrogendioxide_total_column",
#      "//PRODUCT/SUPPORT_DATA/DETAILED_RESULTS/nitrogendioxide_total_column_precision",
#      "//PRODUCT/SUPPORT_DATA/DETAILED_RESULTS/nitrogendioxide_total_column_precision_kernel",
#      "//PRODUCT/SUPPORT_DATA/DETAILED_RESULTS/number_of_iterations",
#      "//PRODUCT/SUPPORT_DATA/DETAILED_RESULTS/number_of_spectral_points_in_retrieval",
#      "//PRODUCT/SUPPORT_DATA/DETAILED_RESULTS/oxygen_oxygen_dimer_slant_column_density",
#      "//PRODUCT/SUPPORT_DATA/DETAILED_RESULTS/oxygen_oxygen_dimer_slant_column_density_precision",
#      "//PRODUCT/SUPPORT_DATA/DETAILED_RESULTS/ozone_slant_column_density",
#      "//PRODUCT/SUPPORT_DATA/DETAILED_RESULTS/ozone_slant_column_density_precision",
#      "//PRODUCT/SUPPORT_DATA/DETAILED_RESULTS/processing_quality_flags",
#      "//PRODUCT/SUPPORT_DATA/DETAILED_RESULTS/ring_coefficient",
#      "//PRODUCT/SUPPORT_DATA/DETAILED_RESULTS/ring_coefficient_precision"
#    ),
#    curvilinear = c("//PRODUCT/longitude", "//PRODUCT/latitude"),
#    driver = NULL
#  )
#  names(nit) <-
#    sub("//PRODUCT/SUPPORT_DATA/DETAILED_RESULTS/", "", names(nit))
#  for (i in seq(length(names(nit)))) {
#    nit[[i]][nit[[i]] > 9e+36] <- NA
#  }
#  st_crs(nit) <- 4326
#  
#  server <- starsTileServer$new(nit)
#  # we save the server here as there should only be one version (sampling of color scales would otherwise result in differently colored tiles)
#  saveRDS(server, "server.rds")

## -----------------------------------------------------------------------------
system.file("compose/Dockerfile", package = "starsTileServer")
system.file("compose/script.R", package = "starsTileServer")

## ---- eval=F------------------------------------------------------------------
#  require(leaflet)
#  leaflet() %>%
#    addTiles() %>%
#    fitBounds(0, 30, 20, 40) %>%
#    addTiles(urlTemplate = "http://127.0.0.1:6081/map/nitrogendioxide_total_column/{z}/{x}/{y}?alpha=.4")

