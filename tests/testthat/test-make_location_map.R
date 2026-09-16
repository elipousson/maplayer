test_that("make_location_map builds a map from a location", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, crs = 3857)

  plot <- make_location_map(location = nc[1, ])

  expect_s3_class(plot, "gg")
})

test_that("make_location_map builds a map from a paper size", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, crs = 3857)

  plot <- make_location_map(location = nc[1, ], paper = "letter")

  expect_s3_class(plot, "gg")
})

test_that("make_location_map uses a provided layer instead of location", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, crs = 3857)

  plot <- make_location_map(layer = ggplot2::geom_sf(data = nc[1, ]), basemap = TRUE)

  expect_s3_class(plot, "gg")
})

test_that("make_layer_map stacks background, main, and foreground layers", {
  plot <- make_layer_map(
    bg_layer = ggplot2::geom_hline(yintercept = 0),
    layer = ggplot2::geom_point(),
    fg_layer = ggplot2::geom_rug(),
    basemap = TRUE
  )

  expect_s3_class(plot, "gg")
  expect_length(plot$layers, 3)
})

test_that("make_layer_map applies labs_ext_params via labs_ext", {
  plot <- make_layer_map(
    layer = ggplot2::geom_point(),
    basemap = TRUE,
    labs_ext_params = list(title = "A title")
  )

  expect_identical(plot$labels$title, "A title")
})

test_that("make_layer_map saves the plot to file when save is TRUE", {
  path <- withr::local_tempfile(fileext = ".png")

  make_layer_map(
    layer = ggplot2::geom_point(),
    basemap = TRUE,
    save = TRUE,
    ggsave_params = list(
      filename = path,
      width = 5,
      height = 4,
      units = "in",
      dpi = 72
    )
  )

  expect_true(file.exists(path))
})

test_that("make_social_map builds a map sized for a social platform", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, crs = 3857)

  plot <- make_social_map(
    location = nc[1, ],
    geom = "sf",
    platform = "instagram",
    format = "post"
  )

  expect_s3_class(plot, "gg")
})

test_that("make_social_map saves the plot to file when save is TRUE", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, crs = 3857)
  path <- withr::local_tempfile(fileext = ".jpeg")

  make_social_map(
    location = nc[1, ],
    geom = "sf",
    platform = "instagram",
    format = "post",
    save = TRUE,
    ggsave_params = list(filename = path, dpi = 72)
  )

  expect_true(file.exists(path))
})

test_that("make_image_map builds a map with markers from image EXIF locations", {
  skip_if_not_installed("filenamr")
  skip_if_not_installed("exiftoolr")

  img_dir <- withr::local_tempdir()
  file.copy(
    system.file("images", "LaSals.jpg", package = "exiftoolr"),
    img_dir
  )
  file.copy(
    system.file("images", "Lizard.jpg", package = "exiftoolr"),
    img_dir
  )

  plot <- suppressWarnings(
    make_image_map(image_path = img_dir, geom = "sf")
  )

  expect_s3_class(plot, "gg")
})
