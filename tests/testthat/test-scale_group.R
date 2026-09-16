test_that("group_data_pal generates a named color vector with a default palette", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc$group <- rep(c("a", "b"), nrow(nc) / 2)

  pal <- group_data_pal(data = nc, col = "group")

  expect_type(pal, "character")
  expect_length(pal, 2)
  expect_setequal(names(pal), c("a", "b"))
})

test_that("group_data_pal uses a named paletteer palette when provided", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc$group <- rep(c("a", "b"), nrow(nc) / 2)

  pal <- group_data_pal(data = nc, col = "group", palette = "amerika::Democrat")

  expect_length(pal, 2)
  expect_setequal(names(pal), c("a", "b"))
})

test_that("scale_group_data returns a discrete fill scale", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc$group <- rep(c("a", "b"), nrow(nc) / 2)

  scale <- scale_group_data(data = nc, col = "group", palette = "amerika::Democrat")

  expect_s3_class(scale, "ScaleDiscrete")
})

test_that("scale_group_data can be added to a ggplot", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc$group <- rep(c("a", "b"), nrow(nc) / 2)

  plot <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf(ggplot2::aes(fill = group)) +
    scale_group_data(data = nc, col = "group", palette = "amerika::Democrat")

  built <- ggplot2::ggplot_build(plot)
  expect_s3_class(built, "ggplot_built")
})

test_that("get_group_data_pal_scale returns names, palette, and scale", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc$group <- rep(c("a", "b"), nrow(nc) / 2)

  out <- get_group_data_pal_scale(data = nc, col = "group", palette = "amerika::Democrat")

  expect_named(out, c("names", "palette", "scale"))
  expect_setequal(out$names, c("a", "b"))
  expect_s3_class(out$scale, "ScaleDiscrete")
})
