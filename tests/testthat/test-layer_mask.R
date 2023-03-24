test_that("layer_mask works", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  layer_mask_data_sf_mask_sf <-
    ggplot2::ggplot() +
    ggplot2::geom_sf(data = nc) +
    layer_mask(data = nc[1, ], mask = nc)

  # skip_on_ci()
  expect_s3_class(
    layer_mask_data_sf_mask_sf,
    "gg"
  )
  # vdiffr::expect_doppelganger(
  #   title = "layer_mask-data-sf-mask-sf",
  #   layer_mask_data_sf_mask_sf
  # )
  data_sf_neatline_true <-
    ggplot2::ggplot() +
    ggplot2::geom_sf(data = nc) +
    layer_mask(data = nc[10, ], neatline = TRUE)
  expect_s3_class(
    data_sf_neatline_true,
    "gg"
  )
  # vdiffr::expect_doppelganger(
  #   title = "layer_mask-data-sf-neatline-true",
  #   data_sf_neatline_true
  # )
  data_sf_dist_num_neatline_true <-
    ggplot2::ggplot() +
    ggplot2::geom_sf(data = nc) +
    layer_mask(data = nc[10, ], dist = 100000, neatline = TRUE)
  expect_s3_class(
    data_sf_dist_num_neatline_true,
    "gg"
  )
  # vdiffr::expect_doppelganger(
  #   title = "layer_mask-data-sf-dist-num-neatline-true",
  #   data_sf_dist_num_neatline_true
  # )
})
