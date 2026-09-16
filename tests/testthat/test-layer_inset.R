test_that("layer_inset adds an inset element to a plot", {
  skip_if_not_installed("patchwork")
  map <- ggplot2::ggplot() + ggplot2::geom_point()
  inset <- ggplot2::ggplot() + ggplot2::geom_line()

  out <- layer_inset(map = map, inset = inset)

  expect_s3_class(out, "patchwork")
})

test_that("layer_inset returns just the inset element when map is NULL", {
  skip_if_not_installed("patchwork")
  inset <- ggplot2::ggplot() + ggplot2::geom_line()

  out <- layer_inset(inset = inset)

  expect_s3_class(out, "inset_patch")
})

test_that("make_inset_map replaces inset with a layer_location_context layer", {
  skip_if_not_installed("patchwork")
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, crs = 3857)
  map <- ggplot2::ggplot(nc[1, ]) + ggplot2::geom_sf()

  out <- make_inset_map(map = map, location = nc[1, ], context = nc)

  expect_s3_class(out, "patchwork")
})

test_that("stamp_inset_img adds an image inset to a plot", {
  skip_if_not_installed("patchwork")
  skip_if_not_installed("figpatch")
  img <- system.file("help", "figures", "logo.png", package = "ggplot2")
  plot <- ggplot2::ggplot() + ggplot2::geom_point()

  out <- stamp_inset_img(path = img, plot = plot)

  expect_s3_class(out, "patchwork")
})

test_that("get_inset_position places the inset in the requested corner", {
  bottom_right <- get_inset_position(position = "bottomright")
  expect_identical(bottom_right$bottom, 0)
  expect_gt(bottom_right$left, 0.5)

  top_left <- get_inset_position(position = "topleft")
  expect_identical(top_left$top, 1)
  expect_identical(top_left$left, 0)
})

test_that("get_inset_position scales the inset width and applies nudges", {
  base <- get_inset_position(position = "bottomright")
  scaled <- get_inset_position(position = "bottomright", scale = 2)
  nudged <- get_inset_position(position = "bottomright", nudge_x = 0.1, nudge_y = 0.1)

  expect_lt(scaled$left, base$left)
  expect_identical(nudged$left, base$left + 0.1)
  expect_identical(nudged$bottom, base$bottom + 0.1)
})
