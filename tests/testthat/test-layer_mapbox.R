test_that("layer_mapbox errors informatively when no access token is available", {
  skip_if_not_installed("mapboxapi")
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, crs = 3857)

  withr::with_envvar(
    c(MAPBOX_PUBLIC_TOKEN = NA, MAPBOX_SECRET_TOKEN = NA),
    expect_error(
      layer_mapbox(data = nc[1, ]),
      "access token"
    )
  )
})

test_that("make_mapbox_map errors informatively when no access token is available", {
  skip_if_not_installed("mapboxapi")
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, crs = 3857)

  withr::with_envvar(
    c(MAPBOX_PUBLIC_TOKEN = NA, MAPBOX_SECRET_TOKEN = NA),
    expect_error(
      make_mapbox_map(data = nc[1, ]),
      "access token"
    )
  )
})
