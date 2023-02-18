test_that("layer_arrow works", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, crs = 3857)

  basemap <-
    ggplot() +
    layer_location_data(data = nc) +
    ggplot2::theme_void()

  # icon can be set by name matching a name from map_icons
  layer_arrow_segment <- basemap +
    layer_arrow(data = nc, from = c("xmid", "ymid"), to = c("xmax", "ymax"))

  expect_s3_class(
    layer_arrow_segment,
    "gg"
  )
})
