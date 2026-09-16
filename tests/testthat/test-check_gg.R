test_that("check_gg passes for a gg object", {
  expect_null(check_gg(ggplot2::geom_point()))
})

test_that("check_gg passes for a list of gg objects by default", {
  expect_null(check_gg(list(ggplot2::geom_point(), ggplot2::geom_line())))
})

test_that("check_gg errors for a list when allow_list is FALSE", {
  expect_error(
    check_gg(list(ggplot2::geom_point()), allow_list = FALSE),
    class = "rlang_error"
  )
})

test_that("check_gg passes for NULL when allow_null is TRUE", {
  expect_null(check_gg(NULL, allow_null = TRUE))
})

test_that("check_gg errors for NULL when allow_null is FALSE", {
  expect_error(check_gg(NULL), class = "rlang_error")
})

test_that("check_gg errors with an informative message for invalid input", {
  expect_snapshot(error = TRUE, check_gg("not a gg"))
})

test_that("check_ggplot passes for a ggplot object", {
  expect_null(check_ggplot(ggplot2::ggplot()))
})

test_that("check_ggplot passes for a list starting with a ggplot", {
  expect_null(check_ggplot(list(ggplot2::ggplot(), ggplot2::geom_point())))
})

test_that("check_ggplot errors for a non-ggplot object", {
  expect_error(check_ggplot("not a ggplot"), class = "rlang_error")
})

test_that("check_ggplot errors for NULL by default", {
  expect_error(check_ggplot(NULL), class = "rlang_error")
})

test_that("check_ggplot passes for NULL when allow_null is TRUE", {
  expect_null(check_ggplot(NULL, allow_null = TRUE))
})
