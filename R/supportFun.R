#' @importFrom grDevices col2rgb
#' @importFrom png writePNG
#' @importFrom units drop_units
#' @importFrom leaflet colorNumeric
#' @importFrom R6 R6Class
#' @importFrom assertthat assert_that is.count
#' @importFrom sf st_bbox st_transform st_as_sfc st_crs st_join st_drop_geometry st_as_sf
#' @import stars
#' @import plumber
#' @import rlang
NULL

xfun <- function(lon, z) {
  1 / (2 * pi) * 2^z * ((lon * pi / 180) + pi)
}
lonfun <- function(x, z) {
  ((x / (1 / (2 * pi) * 2^z)) - pi) / pi * 180
}
yfun <-
  function(lat, z) {
    1 / (2 * pi) * 2^z * (pi - log(tan(pi / 4 + (lat * pi / 180) / 2)))
  }
latfun <-
  function(y, z) {
    (atan(exp(pi - (y / (
      1 / (2 * pi) * 2^z
    )))) - pi / 4) * 2 * 180 / pi
  }

#' Function to calculate a stars grid based on the x,y,z attributes that represents the coordinates of the map tile
#'
#' @param x The location of the tile in the x dimension
#' @param y The location of the tile in the y dimension
#' @param z The zoom level
#' @param tileSize The size of the tile generally 256 pixels
#'
#' @return A stars grid with all values being zero with the dimensions matching those required for the tile specified
#'
#' @details
#'
#' This function is mostly useful for testing purpose.
#'
#' @export
#'
#' @examples
#' targetGrid(4, 2, 4)
#' targetGrid(4, 2, 4, 128)
targetGrid <- function(x, y, z, tileSize = 256L) {
  assert_that(is.count(x + 1L))
  assert_that(is.count(y + 1L))
  assert_that(is.count(z + 1L))
  assert_that(is.count(tileSize))

  targetExtent <-
    st_bbox(c(
      xmin = lonfun(x, z),
      xmax = lonfun(x + 1, z),
      ymin = latfun(y + 1, z),
      ymax = latfun(y, z)
    ),
    crs = 4326
    )
  st_as_stars(
    nx = tileSize,
    ny = tileSize,
    st_bbox(st_transform(
      st_as_sfc(targetExtent),
      crs = 3857 # web mercator
    ))
  )
}
# Converts a matrix of values to to a png image that it the tile to be returned
matrixToPng <- function(targetMatrix, colFun, ...) {
  t <- ncol(targetMatrix) # assums square matrix
  if (inherits(targetMatrix, "units")) {
    targetMatrix <- drop_units(targetMatrix)
  }
  colorArray <-
    aperm(array(col2rgb(
      ifelse(is.na(targetMatrix), "#00000000", colFun(targetMatrix, ...)),
      alpha = T
    ) / 255,
    dim = c(4, t, t)
    ), c(3, 2, 1))[, , ]
  writePNG(colorArray)
}

# Prevent warning from checks for arguments that are not in the function definition in the following two functions
utils::globalVariables(c("x", "y", "z", "colFun", "attrib", "private"))

handlerFunctionFun <- function() {
  args <- as.list(environment())[-(1:3)]
  # Find the grid for which the values need to be calculated
  targetGrid <- targetGrid(x, y, z, private$tileSize)
  nms <- names(args)[names(args) %in% names(formals(private$grid[["idName"]]))]
  targetGridWithValues <-
    do.call(private$grid[["idName"]], c(list(targetGrid), args[nms]))
  targetMatrix <- (drop(targetGridWithValues[[1]]))
  nms <- names(args)[names(args) %in% names(formals(colFun))]
  do.call("matrixToPng", c(list(targetMatrix, colFun = colFun), args[nms]))
}

handlerFunctionStars <- function() {
  args <- as.list(environment())[-(1:3)]
  targetGrid <- targetGrid(x, y, z, private$tileSize)
  gridDim <- which(names(st_dimensions(private$grid)) %in% attr(st_dimensions(private$grid), "raster")$dimensions)
  s <- lapply(mapply("==", lapply(
    lapply(names(st_dimensions(private$grid))[-(gridDim)], st_get_dimension_values, .x = private$grid),
    as.character
  ), args[names(st_dimensions(private$grid))[-(gridDim)]], SIMPLIFY = F), which)
  subGrid <- eval(expr(private$grid[attrib, , , !!!s]))
  d <- st_dimensions(subGrid) # check for curvilinear is same as in stars
  r <- attr(x, "raster")
  if (
    all(r$dimensions %in%
      names(x)) && isTRUE(attr(d, "raster")$curvilinear)) {
    warning("st_warp fails on curvilinear grids, trying slower fall back method")
    targetGridWithValues <- targetGrid
    a <- st_join(st_transform(st_as_sf(targetGrid,
      as_points = T
    ), st_crs(subGrid)), st_as_sf(subGrid,
      as_points = F
    ))
    targetGridWithValues$values <- (st_drop_geometry(a[, 2])[[1]])
  } else {
    targetGridWithValues <- st_warp(subGrid, targetGrid)
  }
  targetMatrix <- (drop(targetGridWithValues[[1]]))
  nms <- names(args)[names(args) %in% names(formals(colFun))]
  do.call("matrixToPng", c(list(targetMatrix, colFun = colFun), args[nms]))
}
