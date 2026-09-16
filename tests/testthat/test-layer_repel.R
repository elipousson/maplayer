test_that("layer_repel creates a text layer by default", {
  skip_if_not_installed("ggrepel")
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, crs = 3857)

  plot <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf() +
    layer_repel(data = nc, label_col = "NAME", geom = "text")

  expect_s3_class(plot, "gg")
  expect_s3_class(plot$layers[[2]]$geom, "GeomTextRepel")
})

test_that("layer_repel creates a label layer when geom is 'label'", {
  skip_if_not_installed("ggrepel")
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, crs = 3857)

  plot <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf() +
    layer_repel(data = nc, label_col = "NAME", geom = "label")

  expect_s3_class(plot, "gg")
  expect_s3_class(plot$layers[[2]]$geom, "GeomLabelRepel")
})

test_that("layer_repel sets xlim and ylim from location_lims", {
  skip_if_not_installed("ggrepel")
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, crs = 3857)

  layer <- layer_repel(
    data = nc,
    label_col = "NAME",
    geom = "text",
    location_lims = nc[1, ]
  )

  bbox <- sfext::as_bbox(nc[1, ])
  expect_identical(layer$geom_params$xlim, c(bbox[["xmin"]], bbox[["xmax"]]))
  expect_setequal(layer$geom_params$ylim, c(bbox[["ymin"]], bbox[["ymax"]]))
})

test_that("layer_repel errors when label_col is not a string", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

  expect_error(layer_repel(data = nc, label_col = 1))
})
