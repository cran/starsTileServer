test_that("coordinate identification", {
  expect_equal(lonfun(xfun(4.34, 5), 5), 4.34)
  expect_equal(xfun(0, 0), .5)
  expect_equal(xfun(-90, 1), .5)
  expect_equal(xfun(90, 1), 1.5)
  expect_equal(yfun(0, 0), .5)
  expect_equal(latfun(0, 0), latfun(0, 1))
  expect_equal(latfun(0, 0), latfun(0, 7))
  expect_equal(latfun(1, 0), latfun(2, 1))
  expect_equal(latfun(yfun(4.34, 5), 5), 4.34)
})

test_that("target matrix", {
  expect_equal((dim(targetGrid(4, 4, 3))), c(x = 256L, y = 256L))
  expect_false(any(sf::st_overlaps(sf::st_as_sf(starsTileServer:::targetGrid(3, 3, 5, 10)), sf::st_as_sf(starsTileServer:::targetGrid(3, 4, 5, 10)), F)))
  expect_equal(sum(sf::st_touches(sf::st_as_sf(starsTileServer:::targetGrid(3, 3, 5, 4)), sf::st_as_sf(starsTileServer:::targetGrid(3, 4, 5, 4)), F)), 10)
})

test_that("target matrix errors", {
  expect_error(targetGrid(-1, 0, 0))
  expect_error(targetGrid(0, -1, 0))
  expect_equal(class(targetGrid(0, 0, 0)), "stars")
  expect_error(targetGrid(0, 0, -1))
})
