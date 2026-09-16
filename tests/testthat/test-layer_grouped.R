test_that("layer_grouped returns one layer per group", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, crs = 3857)
  nc$group <- rep(c("a", "b"), nrow(nc) / 2)

  group_layers <- layer_grouped(data = nc, groupname_col = "group")

  expect_type(group_layers, "list")
  expect_length(group_layers, 2)
  expect_s3_class(group_layers[[1]][[1]], "Layer")
})

test_that("layer_grouped wraps each layer in a ggplot when basemap is TRUE", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, crs = 3857)
  nc$group <- rep(c("a", "b"), nrow(nc) / 2)

  group_layers <- layer_grouped(data = nc, groupname_col = "group", basemap = TRUE)

  expect_length(group_layers, 2)
  expect_s3_class(group_layers[[1]], "gg")
})

test_that("layer_grouped adds a fill scale to each map when palette is provided", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, crs = 3857)
  nc$group <- rep(c("a", "b"), nrow(nc) / 2)

  group_layers <- layer_grouped(
    data = nc,
    groupname_col = "group",
    palette = "amerika::Democrat",
    basemap = TRUE
  )

  built <- ggplot2::ggplot_build(group_layers[[1]])
  expect_s3_class(built, "ggplot_built")
})

test_that("layer_grouped passes additional parameters through to layer_location_data", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, crs = 3857)
  nc$group <- rep(c("a", "b"), nrow(nc) / 2)

  group_layers <- layer_grouped(data = nc, groupname_col = "group", alpha = 0.5)

  expect_identical(group_layers[[1]][[1]]$aes_params$alpha, 0.5)
})

test_that("layer_grouped warns when polygon data doesn't map fill", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, crs = 3857)
  nc$group <- rep(c("a", "b"), nrow(nc) / 2)

  expect_warning(
    layer_grouped(data = nc, groupname_col = "group", aesthetics = "color"),
    class = "rlang_warning"
  )
})
