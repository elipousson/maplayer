test_that("layer_markers works", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))

  nc <-
    dplyr::mutate(
      nc,
      category = dplyr::case_when(
        AREA > 0.15 ~ "larger",
        AREA <= 0.15 ~ "smaller"
      )
    )

  plot <-
    ggplot() +
    layer_markers(
      data = nc,
      make = TRUE,
      groupname_col = "category"
    )

  expect_s3_class(
    plot,
    "gg"
  )

  # expect_snapshot(
  #   ggplot2::summarise_layout(ggplot2::ggplot_build(plot))
  # )
})

test_that("layer_markers passes linewidth through to geom_sf_label without warning", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

  expect_no_warning(
    layer <- layer_markers(
      data = nc[1:5, ],
      geom = "label",
      mapping = ggplot2::aes(label = NAME),
      linewidth = 0.5
    )
  )

  expect_identical(layer$aes_params$linewidth, 0.5)
  expect_null(layer$aes_params$label.size)
})
