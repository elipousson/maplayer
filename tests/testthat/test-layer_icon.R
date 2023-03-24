test_that("layer_icon works", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, crs = 3857)

  basemap <-
    ggplot2::ggplot() +
    ggplot2::theme_void() +
    layer_location_data(data = nc)

  # icon can be set by name matching a name from map_icons
  layer_icon_icon <- basemap +
    layer_icon(data = nc, icon = "point-start", size = 8)

  expect_s3_class(
    layer_icon_icon,
    "gg"
  )

  # layer_icon can also use a column from the sf object
  nc$icon <- rep(c("1", "2", "3", "4"), nrow(nc) / 4)

  layer_icon_iconname_col <- basemap +
    layer_icon(data = nc, iconname_col = "icon", size = 6)

  expect_s3_class(
    layer_icon_iconname_col,
    "gg"
  )
})
