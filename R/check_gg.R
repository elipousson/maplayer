#' Check gg objects or a list of gg class objects
#'
#' @noRd
check_gg <- function(x,
                     ...,
                     allow_list = TRUE,
                     allow_null = FALSE,
                     arg = caller_arg(x),
                     call = caller_env()) {
  if (!missing(x)) {
    if (is_gg(x)) {
      return(invisible(NULL))
    }

    if (allow_list && obj_is_gg(x)) {
      return(invisible(NULL))
    }

    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  what <- "a gg class object"

  if (allow_list) {
    what <- "a gg class object or a list of gg class objects"
  }

  stop_input_type(
    x,
    what,
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}


#' @noRd
check_ggplot <- function(x,
                         ...,
                         allow_list = TRUE,
                         allow_null = FALSE,
                         arg = caller_arg(x),
                         call = caller_env()) {
  if (!missing(x)) {
    if (is.ggplot(x)) {
      return(invisible(NULL))
    }

    if (allow_list && is.ggplot(x[[1]])) {
      return(invisible(NULL))
    }

    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  what <- "a ggplot object"

  if (allow_list) {
    what <- "a ggplot or a list starting with a ggplot"
  }

  stop_input_type(
    x,
    what,
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}
