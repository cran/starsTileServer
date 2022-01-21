
#' A R6 class that extends plumber to function as a tile server
#' @name starsTileServer
#'
#' @description
#' Creates a tile server based on the R6 class `plumber::Plumber`.
#' In can be created both with a `stars` grid as well as a function or list of functions.
#' The main methods are `run` and `new`.
#'
#' @examples
#' m <- matrix(1:20, nrow = 5, ncol = 4)
#' dim(m) <- c(x = 5, y = 4) # named dim
#' (s <- stars::st_as_stars(m))
#' sf::st_crs(s) <- 4326
#' starsTileServer$new(s)
#' # Working directly from a file
#' grid <- system.file("tif/L7_ETMs.tif", package = "stars")
#' starsTileServer$new(grid)
#' \dontrun{
#' starsTileServer$new(s)$run()
#' }
#' @export

starsTileServer <- R6Class(
  "starsTileServer",
  portable = T,
  private = list(
    grid = NULL,
    tileSize = NULL,
    staticParams = list(
      z = list(
        desc = "Zoom level (starts at 0)",
        type = "int",
        required = T
      ),
      x = list(
        desc = "Tile number in x direction (starts at 0)",
        type = "int",
        required = T
      ),
      y = list(
        desc = "Tile number in y direction (starts at 0)",
        type = "int",
        required = T
      )
    ),
    get_params = function() {
      a <-
        lapply(lapply(
          self$get_non_spatial_dimensions(),
          self$get_dimension_values_chr
        ), function(x) {
          if (length(x) == 1) {
            sprintf("[%s]", x)
          } else {
            if (length(x) == 2) {
              sprintf("[%s, %s]", x[1], x[2])
            } else {
              if (length(x) == 3) {
                sprintf("[%s, %s, %s]", x[1], x[2], tail(x, 1))
              } else {
                sprintf("[%s, %s, ..., %s]", x[1], x[2], tail(x, 1))
              }
            }
          }
        })
      if (length(a) != 0) {
        a <-
          lapply(paste0(self$get_non_spatial_dimensions(), ", values: ", a), function(x) {
            list(
              desc = x,
              type = "chr",
              required = F
            )
          })
        names(a) <- self$get_non_spatial_dimensions()
      }
      c(private$staticParams, a)
    }
  ),
  public = list(
    #' @description
    #' This method is used to initialize a new tile server
    #'
    #' @param grid Either a stars grid, a path pointing towards a gridded file, a function or a list of named functions
    #' @param colorFun a color function to use for coloring the map tiles, the function needs to be the same format as [leaflet::colorNumeric()]. It is important to specify a color function as it is important to keep the range of the color scale similar between tiles, therefore the minimum and maximum needs to be fixed. It can also be a list of color functions.
    #' @param tileSize The size of the tile (generally 256 pixels, and not tested with other sizes)
    #' @param ... Arguments passed on to the [plumber::Plumber], most important is the `port` number.
    #'
    #' @details
    #' If `grid` is a function it should take a stars grid as the first argument and return the grid with the same topology with values attached.
    #' Any other arguments to the function will be part of the API and will be passed to the function as characters.
    #'
    #' @return An `starsTileServer` object.
    initialize = function(grid,
                          colorFun = NULL,
                          tileSize = 256,
                          ...) {
      super$initialize(...)
      private$tileSize <- tileSize
      self$filter("cors", function(res) {
        # makes it possible cache tiles in browser
        res$setHeader("Access-Control-Allow-Origin", "*")
        plumber::forward()
      })
      # grid is a single function fall back on the list method
      if (is.function(grid)) {
        grid <- list(map = grid)
      }
      # grid is a list of functions in this case a method for each function is initiated
      if (is.list(grid) && !inherits(grid, "stars")) {
        stopifnot(all(unlist(lapply(
          grid, is.function
        ))))
        stopifnot(!is.null(names(grid)))
        stopifnot(all(names(grid) != ""))
        private$grid <- grid
        if (is.function(colorFun)) {
          colorFun <- rep(list(colorFun), length(names(private$grid)))
          names(colorFun) <- names(grid)
        }
        for (i in names(grid)) {
          stopifnot(i %in% names(colorFun))
          frms <- c(
            formals(grid[[i]])[-1],
            formals(colorFun[[i]])[-1]
          )
          formals(handlerFunctionFun) <-
            c(list(z = 0, x = 0, y = 0), frms)
          extraParams <-
            lapply(paste0(names(frms), ", values: [", unlist(frms), ", ...]"), function(x) {
              list(
                desc = x,
                type = "chr",
                required = F
              )
            })
          names(extraParams) <- names(frms)
          self$add_tile_endpoint(
            prefix = i,
            handlerFun = parse(text = sub(
              "idName", i, deparse(handlerFunctionFun)
            )),
            params = c(private$staticParams, extraParams),
            colorFun = colorFun[names(grid) == i][[1]]
          )
        }
      } else {
        # grid is a stars object add endpoints accordingly
        if (inherits(grid, "stars")) {
          # store the grid in the server as a stars object
          private$grid <- grid
        } else {
          if (is.character(grid)) {
            # if character consider it a path and read it
            stopifnot(file.exists(grid))
            private$grid <- read_stars(grid)
          } else {
            stop(
              "grid should be either, a function, list of functions, stars object or a path readable as a stars object with read_stars."
            )
          }
        }
        #  CRS is needed to locate the grids on the map
        stopifnot(!is.na(sf::st_crs(private$grid)))
        # upto here a grid has been read
        # Add a default color function
        if (is.null(colorFun)) {
          colorFun <-
            lapply(private$grid, function(y) {
              a <- structure(y, dim = NULL)
              rangeForColor <- range(
                sample((a), pmin(length(a), 1000)),
                na.rm = T
              )
              # Remove to prevent large returned objects
              rm(y, a)
              if (inherits(rangeForColor, "units")) {
                rangeForColor <- drop_units(rangeForColor)
              }
              f <- colorNumeric("viridis", rangeForColor)
              ff <- function(x, alpha = 1) {
                paste0(f(x), as.character(as.raw(
                  as.numeric(alpha) * 255
                )))
              }
              attributes(ff) <- attributes(f)
              ff
            })
        }
        # repeat the color function for each attribute of the stars object
        if (is.function(colorFun)) {
          colorFun <-
            rep(list(colorFun), length(names(private$grid)))
        }
        l <-
          lapply(self$get_non_spatial_dimensions(), function(x, g) {
            self$get_dimension_values_chr(x)[1]
          }, g = private$grid)
        names(l) <- self$get_non_spatial_dimensions()
        formals(handlerFunctionStars) <-
          c(list(z = 0, x = 0, y = 0), l, colFormals <- formals(colorFun[[1]])[-1])
        params <- private$get_params()
        if (length(colFormals) != 0) {
          extraParams <-
            lapply(paste0(names(colFormals), ", values: [", unlist(colFormals), ", ...]"), function(x) {
              list(
                desc = x,
                type = "chr",
                required = F
              )
            })
          names(extraParams) <- names(colFormals)
          params <- c(params, extraParams)
        } else {
          extraParams <- list()
        }
        # Loop over stars grid and add plumber end point for all of them
        for (i in names(private$grid)) {
          self$add_tile_endpoint(
            prefix = paste0("map/", i), colorFun = colorFun[[which(names(private$grid) == i)]],
            parse(text = deparse(handlerFunctionStars)), params = params
          )
        }
      }
    },
    #' @description
    #' Add three endpoints to the tile server, to return both the tiles and the color scale used.
    #'
    #' @param prefix the name to be used by the server for this tile server
    #' @param handlerFun The function that handles the api request and returns the grid
    #' @param colorFun The color function to use for example [leaflet::colorNumeric()]
    #' @param params parameters passed on to the `new` method of [plumber::PlumberEndpoint]
    #'
    add_tile_endpoint = function(prefix, handlerFun, colorFun, params) {
      env <- new.env()
      assign("attrib", basename(prefix), env)
      assign("colFun", colorFun, env)
      endpt <-
        plumber::PlumberEndpoint$new(
          "GET",
          paste0("/", prefix, "/<z:int>/<x:int>/<y:int>"),
          handlerFun,
          env,
          serializer = serializer_content_type("image/png"),
          params = params,
          comments = "This provide tiles."
        )
      self$handle(endpoint = endpt)
      endpt <-
        plumber::PlumberEndpoint$new(
          "GET",
          paste0("/", prefix, "/colorfunction"),
          function() {
            colorFun
          },
          new.env(),
          serializer = serializer_rds(),
          params = list(),
          comments = "An API end point to retrieve the color scale used by the server."
        )
      self$handle(endpoint = endpt)
      endpt <-
        plumber::PlumberEndpoint$new(
          "GET",
          paste0("/", prefix, "/colorfunctionnoalpha"),
          function() {
            a <- colorFun
            aa <- function(x) substr(a(x), 1, 7)
            attributes(aa) <- attributes(a)
            aa
          },
          new.env(),
          serializer = serializer_rds(),
          params = list(),
          comments = "An API end point to retrieve the color scale used by the server."
        )
      self$handle(endpoint = endpt)
    },
    #' @description
    #' return the grid used to initialize the function
    get_grid = function() {
      private$grid
    },
    #' @description
    #' return the attributes of the stars grid
    get_attributes = function() {
      names(private$grid)
    },
    #' @description
    #' return the names of the dimensions of the grid
    get_dimensions = function() {
      names(st_dimensions(private$grid))
    },
    #' @description
    #' return the values of a dimension as a character
    #'
    #' @param x the name of the dimension
    get_dimension_values_chr = function(x) {
      as.character(st_get_dimension_values(x, .x = private$grid))
    },
    #' @description
    #' return all non spatial dimensions
    get_non_spatial_dimensions = function() {
      self$get_dimensions()[!self$get_dimensions() %in% attr(st_dimensions(private$grid), "raster")$dimensions]
    }
  ),
  inherit = plumber::Plumber
)
NULL
