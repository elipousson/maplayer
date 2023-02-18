test_that("layer_count works", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  data <- sf::st_sample(nc, 75)

  layer_count_data <- ggplot() +
    layer_count(data = data, location = nc)
  expect_s3_class(
    layer_count_data,
    "gg"
  )

  layer_count_id <- ggplot() +
    layer_count(data = data, y = nc, .id = "FIPS")
  expect_s3_class(
    layer_count_id,
    "gg"
  )
})
