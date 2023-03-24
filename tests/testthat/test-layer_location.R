test_that("layer_location works", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  layer_location_type <-
    ggplot2::ggplot(data = nc) +
    ggplot2::geom_sf(fill = NA) +
    layer_location(
      type = nc,
      name = "1825",
      name_col = "CNTY_",
      linewidth = 0.5,
      linetype = "solid",
      fill = "red"
    )

  expect_s3_class(
    layer_location_type,
    "gg"
  )

  # vdiffr::expect_doppelganger(
  #   title = "layer_location_type",
  #   layer_location_type
  # )
})
