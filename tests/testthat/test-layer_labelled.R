test_that("layer_labelled works with the default mapping", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, crs = 3857)

  plot <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf() +
    layer_labelled(data = nc, label_col = "NAME")

  expect_s3_class(plot, "gg")
})

test_that("layer_labelled unions geometry by label_col when union is TRUE", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, crs = 3857)
  nc$group <- rep(c("a", "b"), nrow(nc) / 2)

  plot <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf() +
    layer_labelled(data = nc, label_col = "group", union = TRUE)

  expect_s3_class(plot, "gg")
})

test_that("layer_labelled errors for an unsupported geom", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

  expect_error(layer_labelled(data = nc, label_col = "NAME", geom = "not_a_geom"))
})
