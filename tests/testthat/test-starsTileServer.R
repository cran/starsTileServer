test_that("initialization", {
  expect_error(starsTileServer$new(3), "grid should be either")
  expect_error(starsTileServer$new("nonExistingFile"), "file.exists.grid. is not TRUE")
})

test_that("tile server grid on file base", {
  expect_equal(
    class(srv <- starsTileServer$new(
      system.file("tif/L7_ETMs.tif", package = "stars"),
      colorFun = colorNumeric("viridis", c(0, 300))
    )),
    c("starsTileServer", "Plumber", "Hookable", "R6")
  )
  expect_equal(read_stars(system.file("tif/L7_ETMs.tif", package = "stars")), srv$get_grid())
  expect_equal(srv$call(make_req(path = "/map/L7_ETMs.tif/9/206/267"))$status, 200)
  expect_equal(srv$call(make_req(path = "/map/L7_ETMs.tif/9/206/267"))$headers$"Access-Control-Allow-Origin", "*")
  expect_equal(srv$call(make_req(path = "/map/L7_ETMs.tif/9/206/267"))$headers$"Content-Type", "image/png")
})
test_that("tile server grid on file base same as grid input", {
  expect_equal(class(srv <- starsTileServer$new(system.file("tif/L7_ETMs.tif", package = "stars"), colorFun = colorNumeric("viridis", c(0, 300)))), c("starsTileServer", "Plumber", "Hookable", "R6"))
  expect_equal(class(srv2 <-
    starsTileServer$new(read_stars(
      system.file("tif/L7_ETMs.tif", package = "stars")
    ),
    colorFun = colorNumeric("viridis", c(0, 300))
    )), c("starsTileServer", "Plumber", "Hookable", "R6"))
  expect_equal(srv$call(make_req(path = "/map/L7_ETMs.tif/9/206/267"))$status, 200)
  expect_equal(srv$call(make_req(path = "/map/L7_ETMs.tif/9/206/267"))$headers$"Access-Control-Allow-Origin", "*")
  expect_equal(srv$call(make_req(path = "/map/L7_ETMs.tif/9/206/267"))$headers$"Content-Type", "image/png")
  expect_snapshot_value(srv$call(make_req(path = "/map/L7_ETMs.tif/9/206/267")), style = "serialize")
  expect_equal(srv$call(make_req(path = "/map/L7_ETMs.tif/9/206/267")), srv2$call(make_req(path = "/map/L7_ETMs.tif/9/206/267")))
  expect_equal(srv$call(make_req(path = "/map/colorfunction")), srv2$call(make_req(path = "/map/colorfunction")))
})


test_that("colorfun return", {
  expect_equal(
    class(srv <- starsTileServer$new(system.file("tif/L7_ETMs.tif", package = "stars"),
      colorFun = colorNumeric("viridis", c(0, 300))
    )),
    c("starsTileServer", "Plumber", "Hookable", "R6")
  )
  expect_equal(
    parser_rds()(srv$call(make_req(
      path = "/map/L7_ETMs.tif/colorfunction"
    ))$body)(c(0:300)),
    colorNumeric("viridis", c(0, 300))(c(0:300))
  )
})


test_that("colorfun default", {
  withr::with_seed(345345, {
    expect_equal(
      class(srv <- starsTileServer$new(system.file("tif/L7_ETMs.tif", package = "stars"))),
      c("starsTileServer", "Plumber", "Hookable", "R6")
    )
    expect_equal(
      parser_rds()(srv$call(make_req(
        path = "/map/L7_ETMs.tif/colorfunction"
      ))$body)(c(c(100, 120))),
      c("#1E9C89ff", "#36B878ff")
    )
    expect_equal(
      substr(parser_rds()(srv$call(make_req(
        path = "/map/L7_ETMs.tif/colorfunction"
      ))$body)(c(c(100, 120))), 1, 7),
      parser_rds()(srv$call(make_req(
        path = "/map/L7_ETMs.tif/colorfunctionnoalpha"
      ))$body)(c(c(100, 120)))
    )
    expect_warning(
      parser_rds()(srv$call(make_req(
        path = "/map/L7_ETMs.tif/colorfunction"
      ))$body)(c(c(300)))
    )
  })
})

test_that("server with functions", {
  expect_equal(
    class(srv <- starsTileServer$new(
      function(x) {
        x[[1]] <- stars::st_get_dimension_values(x, "x") / 10000
        x
      },
      colorFun = colorNumeric("Greens", c(-2000, 2000))
    )),
    c("starsTileServer", "Plumber", "Hookable", "R6")
  )
  expect_true(all(diff(png::readPNG(srv$call(make_req(path = "/map/1/1/1"))$body)[1, , 2]) <= 0))
  expect_true(all(diff(png::readPNG(srv$call(make_req(path = "/map/1/1/1"))$body)[, 1, 2]) == 0))
  expect_equal(
    substr(parser_rds()(srv$call(make_req(
      path = "/map/colorfunction"
    ))$body)(c(c(1000, 120))), 1, 7),
    parser_rds()(srv$call(make_req(
      path = "/map/colorfunctionnoalpha"
    ))$body)(c(c(1000, 120)))
  )
})


test_that("units input", {
  expect_equal(class(srv2 <-
    starsTileServer$new(grd <- read_stars(
      system.file("tif/L7_ETMs.tif", package = "stars")
    ),
    colorFun = colorNumeric("viridis", c(0, 300))
    )), c("starsTileServer", "Plumber", "Hookable", "R6"))
  expect_silent(grd[[1]] <- units::set_units(grd[[1]], "m"))
  expect_equal(class(srv <- starsTileServer$new(grd, colorFun = colorNumeric("viridis", c(0, 300)))), c("starsTileServer", "Plumber", "Hookable", "R6"))
  expect_equal(srv$call(make_req(path = "/map/L7_ETMs.tif/9/206/267")), srv2$call(make_req(path = "/map/L7_ETMs.tif/9/206/267")))
  expect_equal(srv$call(make_req(path = "/map/colorfunction")), srv2$call(make_req(path = "/map/colorfunction")))
})

test_that("irregular grid", {
  skip_if_not_installed("stars", "0.5-5")
  # x = c(0, 0.5, 1, 2, 4, 5)  # 6 numbers: boundaries!
  #  y = c(0.3, 0.5, 1, 2, 2.2) # 5 numbers: boundaries!
  expect_silent(r <- st_as_stars(list(m = matrix(1:20, ncol = 4)), dimensions = st_dimensions(
    x = c(0, 0.5, 1, 2, 4, 5),
    y = c(0.3, 0.5, 1, 2, 2.2)
  )))
  expect_silent(sf::st_crs(r) <- 4326)
  # The following code can be used to visualize the body writeBin(con =  'a.png',body)
  expect_snapshot_value(starsTileServer$new(r,
    colorFun = colorNumeric("Greens", c(0, 20))
  )$call(make_req(path = "/map/m/6/32/31"))$body, style = "serialize")
  expect_equal(unique(c(png::readPNG(starsTileServer$new(r,
    colorFun = colorNumeric("Greens", c(0, 20))
  )$call(make_req(path = "/map/m/6/31/31"))$body))), 0)
  expect_equal(unique(c(png::readPNG(starsTileServer$new(r,
    colorFun = colorNumeric("Greens", c(0, 20))
  )$call(make_req(path = "/map/m/6/32/30"))$body))), 0)
})


test_that("curvilinear grid", {
  # x = c(0, 0.5, 1, 2, 4, 5)  # 6 numbers: boundaries!
  #  y = c(0.3, 0.5, 1, 2, 2.2) # 5 numbers: boundaries!

  expect_silent(r <- st_as_stars(st_as_stars(bb = matrix(1:50, 5)), curvilinear = list(X1 = do.call("rbind", lapply(3:7, "*", sin((1:10) / 10))), X2 = do.call("rbind", lapply(3:7, "*", cos((1:10) / 10))))))

  expect_silent(sf::st_crs(r) <- 4326)
  # The following code can be used to visualize the body writeBin(con =  'a.png',body)
  expect_warning(expect_snapshot_value(starsTileServer$new(r,
    colorFun = colorNumeric("Greens", c(0, 50))
  )$call(make_req(path = "/map/bb/6/32/31"))$body, style = "serialize"))
  expect_warning(expect_equal(unique(c(png::readPNG(starsTileServer$new(r,
    colorFun = colorNumeric("Greens", c(0, 20))
  )$call(make_req(path = "/map/bb/6/31/31"))$body))), 0), "st_warp fails on curvilinear grids, trying slower fall back method")
  expect_warning(expect_equal(unique(c(png::readPNG(starsTileServer$new(r,
    colorFun = colorNumeric("Greens", c(0, 20))
  )$call(make_req(path = "/map/bb/6/32/32"))$body))), 0))
})
test_that("projection needed", {
  expect_error(starsTileServer$new(st_as_stars(matrix(1:10, 2)),
    colorFun = colorNumeric("Greens", c(0, 50))
  ), ".is.na.sf..st_crs.private.grid.. is not TRUE")
})
