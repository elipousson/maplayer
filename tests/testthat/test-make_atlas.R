test_that("make_atlas lays out plots on sheets using explicit dims", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, crs = 3857)
  p1 <- ggplot2::ggplot(nc[1, ]) + ggplot2::geom_sf()
  p2 <- ggplot2::ggplot(nc[2, ]) + ggplot2::geom_sf()

  sheets <- make_atlas(list(p1, p2), dims = c(1, 1))

  expect_type(sheets, "list")
  expect_length(sheets, 1)
  expect_s3_class(sheets[[1]], "patchwork")
})

test_that("make_atlas lays out plots on sheets using ncol and nrow", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, crs = 3857)
  p1 <- ggplot2::ggplot(nc[1, ]) + ggplot2::geom_sf()
  p2 <- ggplot2::ggplot(nc[2, ]) + ggplot2::geom_sf()

  sheets <- make_atlas(list(p1, p2), ncol = 2, nrow = 1)

  expect_type(sheets, "list")
  expect_length(sheets, 1)
})

test_that("make_atlas saves a multi-plot sheet to a single pdf file when save is TRUE", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, crs = 3857)
  p1 <- ggplot2::ggplot(nc[1, ]) + ggplot2::geom_sf()
  p2 <- ggplot2::ggplot(nc[2, ]) + ggplot2::geom_sf()
  path <- withr::local_tempfile(fileext = ".pdf")

  make_atlas(
    list(p1, p2),
    dims = c(1, 1),
    save = TRUE,
    filename = path,
    device = "pdf"
  )

  expect_true(file.exists(path))
})

test_that("make_atlas saves a single-plot sheet to a png file when save is TRUE", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, crs = 3857)
  p1 <- ggplot2::ggplot(nc[1, ]) + ggplot2::geom_sf()
  path <- withr::local_tempfile(fileext = ".png")

  make_atlas(
    list(p1),
    dims = c(1, 1),
    save = TRUE,
    filename = path
  )

  expect_true(
    file.exists(path) ||
      file.exists(
        file.path(
          dirname(path),
          paste0(tools::file_path_sans_ext(basename(path)), "_pg_1.png")
        )
      )
  )
})
