test_that("labs_ext sets a title using glue string interpolation", {
  labs <- labs_ext(
    title = "Map of {county}",
    .envir = list2env(list(county = "Wake County"))
  )

  expect_identical(as.character(labs$title), "Map of Wake County")
})

test_that("labs_ext appends a source note to the caption", {
  labs <- labs_ext(title = "A map", source_note = "US Census Bureau")

  expect_identical(as.character(labs$caption), "Source: US Census Bureau.")
})

test_that("labs_ext can be added to a ggplot object", {
  plot <- ggplot2::ggplot() + labs_ext(title = "A title", subtitle = "A subtitle")

  expect_s3_class(plot, "gg")
  expect_identical(plot$labels$title, "A title")
  expect_identical(plot$labels$subtitle, "A subtitle")
})

test_that("labs_ext passes additional aesthetic labels through dots", {
  labs <- labs_ext(x = "X axis", y = "Y axis")

  expect_identical(labs$x, "X axis")
  expect_identical(labs$y, "Y axis")
})
