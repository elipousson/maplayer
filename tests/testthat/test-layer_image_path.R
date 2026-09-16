test_that("layer_image_path adds an image layer from a path column", {
  skip_if_not_installed("ggpath")
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, crs = 3857)
  nc$path <- system.file("help", "figures", "logo.png", package = "ggplot2")

  plot <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf() +
    layer_image_path(data = nc)

  expect_s3_class(plot, "gg")
})

test_that("layer_image_path errors when path_col is missing from data", {
  skip_if_not_installed("ggpath")
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

  expect_error(
    layer_image_path(data = nc),
    class = "rlang_error"
  )
})

test_that("layer_image_path wraps the layer in a neatline when neatline is TRUE", {
  skip_if_not_installed("ggpath")
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, crs = 3857)
  nc$path <- system.file("help", "figures", "logo.png", package = "ggplot2")

  plot <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf() +
    layer_image_path(data = nc, neatline = TRUE)

  expect_s3_class(plot, "gg")
})

test_that("layer_image_path wraps the layer in a ggplot when basemap is TRUE", {
  skip_if_not_installed("ggpath")
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, crs = 3857)
  nc$path <- system.file("help", "figures", "logo.png", package = "ggplot2")

  plot <- layer_image_path(data = nc, basemap = TRUE)

  expect_s3_class(plot, "gg")
})
