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
