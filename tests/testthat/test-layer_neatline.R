test_that("layer_neatline works", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  nc_map <-
    ggplot2::ggplot(nc) +
    ggplot2::geom_sf()
  # Check class with default params
  expect_s3_class(
    nc_map +
      layer_neatline(data = nc[1, ]),
    "ggplot"
  )
  # Check class with expand parameter TRUE
  expect_s3_class(
    nc_map +
      layer_neatline(data = nc[1, ], expand = TRUE),
    "ggplot"
  )
  # vdiffr::expect_doppelganger(
  #   title = "layer_neatline",
  #   fig = nc_map +
  #     layer_neatline(data = nc[1, ])
  # )
  expect_s3_class(
    nc_map +
      layer_neatline(data = nc[1, ], bgcolor = "none", color = "none"),
    "gg"
  )
  # vdiffr::expect_doppelganger(
  #   title = "layer_neatline-bgcolor-none-color-none",
  #   fig = nc_map +
  #     layer_neatline(data = nc[1, ], bgcolor = "none", color = "none")
  # )
  expect_s3_class(
    nc_map +
      layer_neatline(data = nc[1, ], bgcolor = NA, color = NA),
    "gg"
  )
  # vdiffr::expect_doppelganger(
  #   title = "layer_neatline-bgcolor-na-color-na",
  #   fig = nc_map +
  #     layer_neatline(data = nc[1, ], bgcolor = NA, color = NA)
  # )
  expect_s3_class(
    nc_map +
      layer_neatline(data = nc[1, ], hide_grid = FALSE),
    "gg"
  )
  # vdiffr::expect_doppelganger(
  #   title = "layer_neatline-hide-grid-false",
  #   fig = nc_map +
  #     layer_neatline(data = nc[1, ], hide_grid = FALSE)
  # )
  expect_s3_class(
    nc_map +
      layer_neatline(data = nc[1, ], label_axes = "--EN"),
    "gg"
  )
  # vdiffr::expect_doppelganger(
  #   title = "layer_neatline-label-axes--EN",
  #   fig = nc_map +
  #     layer_neatline(data = nc[1, ], label_axes = "--EN")
  # )
})

test_that("set_neatline works", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  nc_map <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf()
  # Check class with default params
  expect_s3_class(set_neatline(nc_map, data = nc[1, ]), "ggplot")

  expect_s3_class(
    set_neatline(nc_map, data = nc[1, ], neatline = FALSE),
    "ggplot"
  )

  expect_s3_class(
    set_neatline(nc_map, neatline = layer_neatline(data = nc[1, ])),
    "ggplot"
  )


  # vdiffr::expect_doppelganger(
  #   title = "set_neatline",
  #   fig = set_neatline(nc_map, data = nc[1, ])
  # )
  # vdiffr::expect_doppelganger(
  #   title = "set_neatline-neatline-false",
  #   fig = set_neatline(nc_map, data = nc[1, ], neatline = FALSE)
  # )
  # vdiffr::expect_doppelganger(
  #   title = "set_neatline-neatline-layer",
  #   fig = set_neatline(nc_map, neatline = layer_neatline(data = nc[1, ]))
  # )
})
