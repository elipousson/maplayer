test_that("layer_frame works", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  skip_on_ci()
  layer_frame_defaults <-
    ggplot2::ggplot(data = nc) +
    layer_frame(style = "circle") +
    ggplot2::geom_sf()
  expect_s3_class(
    layer_frame_defaults,
    "gg"
  )
  vdiffr::expect_doppelganger(
    title = "layer_frame-data-ggplot",
    layer_frame_defaults
  )
  layer_frame_style_square <-
    ggplot2::ggplot(data = nc) +
    layer_frame(style = "square") +
    ggplot2::geom_sf()
  expect_s3_class(
    layer_frame_style_square,
    "gg"
  )
  vdiffr::expect_doppelganger(
    title = "layer_frame-style-square",
    layer_frame_style_square
  )
  layer_frame_style_rect <-
    ggplot2::ggplot(data = nc) +
    layer_frame(style = "rect") +
    ggplot2::geom_sf()
  expect_s3_class(
    layer_frame_style_rect,
    "gg"
  )
  vdiffr::expect_doppelganger(
    title = "layer_frame-style-rect",
    layer_frame_style_rect
  )
  layer_frame_style_buffer <-
    ggplot2::ggplot(data = nc) +
    layer_frame(data = nc, style = "buffer", dist = 50, unit = "mi") +
    ggplot2::geom_sf()
  expect_s3_class(
    layer_frame_style_buffer,
    "gg"
  )
  vdiffr::expect_doppelganger(
    title = "layer_frame-style-buffer",
    layer_frame_style_buffer
  )
  layer_frame_style_none <-
    ggplot2::ggplot(data = nc) +
    layer_frame(data = nc, style = "none") +
    ggplot2::geom_sf()
  expect_s3_class(
    layer_frame_style_none,
    "gg"
  )
  vdiffr::expect_doppelganger(
    title = "layer_frame-style-none",
    layer_frame_style_none
  )
})
