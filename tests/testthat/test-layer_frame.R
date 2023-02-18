test_that("layer_frame works", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  layer_frame_defaults <-
    ggplot2::ggplot(data = nc) +
    layer_frame(data = nc) +
    ggplot2::geom_sf()
  expect_s3_class(
    layer_frame_defaults,
    "gg"
  )
  layer_frame_style_square <-
    ggplot2::ggplot(data = nc) +
    layer_frame(style = "square") +
    ggplot2::geom_sf()
  expect_s3_class(
    layer_frame_style_square,
    "gg"
  )
  layer_frame_style_rect <-
    ggplot2::ggplot(data = nc) +
    layer_frame(style = "rect") +
    ggplot2::geom_sf()
  expect_s3_class(
    layer_frame_style_rect,
    "gg"
  )
  layer_frame_style_buffer <-
    ggplot2::ggplot(data = nc) +
    layer_frame(data = nc, style = "buffer", dist = 50, unit = "mi") +
    ggplot2::geom_sf()
  expect_s3_class(
    layer_frame_style_buffer,
    "gg"
  )
  layer_frame_style_none <-
    ggplot2::ggplot(data = nc) +
    layer_frame(data = nc, style = "none") +
    ggplot2::geom_sf()
  expect_s3_class(
    layer_frame_style_none,
    "gg"
  )

  # skip_on_ci()
  # vdiffr::expect_doppelganger(
  #   title = "layer_frame-data-ggplot",
  #   layer_frame_defaults
  # )
  # vdiffr::expect_doppelganger(
  #   title = "layer_frame-style-square",
  #   layer_frame_style_square
  # )
  # vdiffr::expect_doppelganger(
  #   title = "layer_frame-style-rect",
  #   layer_frame_style_rect
  # )
  # vdiffr::expect_doppelganger(
  #   title = "layer_frame-style-buffer",
  #   layer_frame_style_buffer
  # )
  # vdiffr::expect_doppelganger(
  #   title = "layer_frame-style-none",
  #   layer_frame_style_none
  # )
})
