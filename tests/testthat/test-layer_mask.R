test_that("layer_mask works", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))

  skip_on_ci()
  expect_s3_class(
    ggplot2::ggplot() +
      ggplot2::geom_sf(data = nc) +
      layer_mask(data = nc[1, ], mask = nc),
    "gg"
  )
  expect_s3_class(
    ggplot2::ggplot() +
      ggplot2::geom_sf(data = nc) +
      layer_mask(data = nc[10, ], diag_ratio = 3, neatline = TRUE),
    "gg"
  )
  expect_s3_class(
    ggplot2::ggplot() +
      ggplot2::geom_sf(data = nc) +
      layer_mask(data = nc[10, ], dist = 100000, neatline = TRUE),
    "gg"
  )
})
