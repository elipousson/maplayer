test_that("layer_location_context works", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, crs = 3857)

  layer_context <-
    ggplot() +
    layer_location_context(
      context = nc,
      location = nc[1,]
    )

  expect_s3_class(
    layer_context,
    "gg"
  )
})
