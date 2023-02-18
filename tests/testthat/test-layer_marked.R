test_that("layer_marked works", {
  skip_if_not_installed("ggforce")
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, crs = 3857)

  layer_marked_circle <-
    ggplot() +
    geom_sf(data = nc) +
    layer_marked(
      data = nc[c(1:2),],
      geom = "circle"
      ) +
    ggplot2::theme_void()

  expect_s3_class(
    layer_marked_circle,
    "gg"
  )
})
