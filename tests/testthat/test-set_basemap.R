test_that("set_basemap returns x unchanged when basemap is FALSE", {
  layer <- ggplot2::geom_point()
  expect_identical(set_basemap(layer, basemap = FALSE), layer)
})

test_that("set_basemap wraps x in a new ggplot when basemap is TRUE", {
  layer <- ggplot2::geom_point()
  out <- set_basemap(layer, basemap = TRUE)

  expect_s3_class(out, "gg")
  expect_length(out$layers, 1)
})

test_that("set_basemap adds x to an existing ggplot basemap", {
  layer <- ggplot2::geom_point()
  basemap <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg))
  out <- set_basemap(layer, basemap = basemap)

  expect_s3_class(out, "gg")
  expect_length(out$layers, 1)
  expect_identical(out$data, mtcars)
})

test_that("make_basemap is an alias for set_basemap", {
  layer <- ggplot2::geom_point()
  expect_identical(make_basemap(layer, basemap = TRUE), set_basemap(layer, basemap = TRUE))
})
