test_that("layer_scaled returns a neatline layer sized to a paper and scale", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, crs = 3857)
  small_area <- suppressWarnings(sf::st_buffer(sf::st_centroid(nc[1, ]), 500))

  scaled <- suppressWarnings(
    layer_scaled(data = small_area, paper = "letter", scale = "1:24,000")
  )

  expect_type(scaled, "list")
})

test_that("layer_scaled errors when data is too large to fit the scale and paper", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, crs = 3857)

  expect_error(
    layer_scaled(data = nc[1, ], paper = "letter", scale = "1:24,000"),
    class = "rlang_error"
  )
})

test_that("layer_scaled does not error when clip is TRUE even if data is too large", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, crs = 3857)

  expect_no_error(
    suppressWarnings(
      layer_scaled(data = nc[1, ], paper = "letter", scale = "1:24,000", clip = TRUE)
    )
  )
})
