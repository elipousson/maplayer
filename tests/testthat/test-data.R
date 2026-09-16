test_that("map_icons is a tibble with the documented columns", {
  expect_s3_class(map_icons, "data.frame")
  expect_true(nrow(map_icons) > 0)
  expect_named(map_icons, c("name", "url", "size", "style", "repo"))
})
