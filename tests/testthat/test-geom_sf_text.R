test_that("geom_sf_text_ext returns a plain geom_sf_text layer by default", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

  layer <- geom_sf_text_ext(mapping = ggplot2::aes(label = NAME), data = nc)

  expect_s3_class(layer, "Layer")
})

test_that("geom_sf_text_ext also blanks axis titles when title_axes is TRUE", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

  out <- geom_sf_text_ext(
    mapping = ggplot2::aes(label = NAME),
    data = nc,
    title_axes = TRUE
  )

  expect_type(out, "list")
  expect_length(out, 2)
  expect_s3_class(out[[1]], "Layer")
  expect_s3_class(out[[2]], "theme")
})

test_that("geom_sf_label_ext returns a plain geom_sf_label layer by default", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

  layer <- geom_sf_label_ext(mapping = ggplot2::aes(label = NAME), data = nc)

  expect_s3_class(layer, "Layer")
})

test_that("geom_sf_label_ext works when family and face are not supplied", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

  plot <- ggplot2::ggplot(nc) +
    geom_sf_label_ext(mapping = ggplot2::aes(label = NAME))

  expect_s3_class(plot, "gg")
})

test_that("geom_sf_label_ext passes face through as fontface without warning", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

  expect_no_warning(
    layer <- geom_sf_label_ext(
      mapping = ggplot2::aes(label = NAME),
      data = nc,
      face = "bold"
    )
  )

  expect_identical(layer$aes_params$fontface, "bold")
})

test_that("geom_sf_label_ext also blanks axis titles when title_axes is TRUE", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

  out <- geom_sf_label_ext(
    mapping = ggplot2::aes(label = NAME),
    data = nc,
    title_axes = TRUE
  )

  expect_type(out, "list")
  expect_length(out, 2)
  expect_s3_class(out[[1]], "Layer")
  expect_s3_class(out[[2]], "theme")
})
