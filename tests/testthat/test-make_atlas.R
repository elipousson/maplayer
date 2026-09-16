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
