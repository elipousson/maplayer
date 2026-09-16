test_that("gg_plot_layers builds a plot from a base plot and layers", {
  plot <- gg_plot_layers(
    ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)),
    .layer = ggplot2::geom_point()
  )

  expect_s3_class(plot, "gg")
  expect_length(plot$layers, 1)
})

test_that("gg_plot_layers combines background, main, and foreground layers", {
  plot <- gg_plot_layers(
    ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)),
    .bg_layer = ggplot2::geom_smooth(),
    .layer = ggplot2::geom_point(),
    .fg_layer = ggplot2::geom_rug()
  )

  expect_s3_class(plot, "gg")
  expect_length(plot$layers, 3)
})

test_that("gg_plot_layers applies labs_params using labs_fn", {
  plot <- gg_plot_layers(
    ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)),
    .layer = ggplot2::geom_point(),
    labs_params = list(title = "A title")
  )

  expect_identical(plot$labels$title, "A title")
})

test_that("gg_plot_layers builds a ggplot when plot is not already a ggplot", {
  plot <- gg_plot_layers(
    ggplot2::geom_point(),
    plot = TRUE,
    data = mtcars,
    mapping = ggplot2::aes(wt, mpg)
  )

  expect_s3_class(plot, "gg")
})

test_that("combine_gg_list reduces a list of ggplot objects with +", {
  base <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg))
  combined <- combine_gg_list(list(base, ggplot2::geom_point()))

  expect_s3_class(combined, "gg")
  expect_length(combined$layers, 1)
})

test_that("combine_gg_list adds y to a ggplot x", {
  base <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg))
  combined <- combine_gg_list(base, ggplot2::geom_point())

  expect_s3_class(combined, "gg")
  expect_length(combined$layers, 1)
})

test_that("combine_gg_list concatenates x and y when x is not a ggplot", {
  combined <- combine_gg_list(ggplot2::geom_point(), ggplot2::geom_line())

  expect_type(combined, "list")
  expect_length(combined, 2)
})

test_that("combine_gg_list returns x unchanged when y is NULL or empty", {
  layer <- ggplot2::geom_point()
  expect_identical(combine_gg_list(layer, NULL), layer)
  expect_identical(combine_gg_list(layer, list()), layer)
})

test_that("gg_labs sets title, subtitle, and caption from glue strings", {
  labs <- gg_labs(title = "Hello {name}", .envir = list2env(list(name = "World")))

  expect_s3_class(labs, "ggplot2::labels")
  expect_identical(as.character(labs$title), "Hello World")
})

test_that("gg_labs drops waiver and NULL values", {
  labs <- gg_labs(title = "A title")

  expect_false("subtitle" %in% names(labs))
  expect_identical(names(labs), "title")
})

test_that("gg_labs appends source_note to caption via gg_caption", {
  labs <- gg_labs(source_note = "US Census Bureau")

  expect_identical(as.character(labs$caption), "Source: US Census Bureau.")
})

test_that("gg_labs combines an existing caption with source_note", {
  labs <- gg_labs(caption = "My caption", source_note = "US Census Bureau")

  expect_identical(as.character(labs$caption), "My caption. Source: US Census Bureau.")
})

test_that("gg_caption returns NULL when caption and source_note are missing", {
  expect_null(gg_caption())
})

test_that("gg_caption returns source note alone when caption is missing", {
  caption <- gg_caption(source_note = "US Census Bureau")
  expect_identical(as.character(caption), "Source: US Census Bureau.")
})

test_that("gg_caption combines multiple caption strings with collapse", {
  caption <- gg_caption(caption = c("Line 1", "Line 2"), collapse = " / ")
  expect_identical(as.character(caption), "Line 1 / Line 2")
})

test_that("aes_label adds a label aesthetic when mapping has none", {
  data <- data.frame(name = c("a", "b"))
  mapping <- aes_label(NULL, data = data, label_col = "name")

  expect_true(rlang::has_name(mapping, "label"))
})

test_that("aes_label preserves an existing label aesthetic", {
  mapping <- ggplot2::aes(label = category)
  out <- aes_label(mapping, label_col = "name")

  expect_identical(out$label, mapping$label)
})

test_that("modify_mapping adds new aesthetics without overwriting existing ones", {
  mapping <- modify_mapping(mapping = ggplot2::aes(color = existing), fill = "group_col")

  expect_true(rlang::has_name(mapping, "fill"))
  expect_true(rlang::has_name(mapping, "colour"))
})

test_that("modify_mapping returns an empty aes() when mapping and params are NULL", {
  mapping <- modify_mapping()
  expect_s3_class(mapping, "uneval")
  expect_length(mapping, 0)
})
