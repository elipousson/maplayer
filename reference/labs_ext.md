# Add labels to a ggplot2 plot or map

A helper function that converts strings to glue strings for the title,
subtitle, and caption.

## Usage

``` r
labs_ext(
  ...,
  title = ggplot2::waiver(),
  subtitle = ggplot2::waiver(),
  caption = ggplot2::waiver(),
  tag = ggplot2::waiver(),
  alt = ggplot2::waiver(),
  alt_insight = ggplot2::waiver(),
  source_note = NULL,
  source_sep = ". ",
  source_before = "Source: ",
  source_end = ".",
  .sep = "",
  .envir = parent.frame(),
  .open = "{",
  .close = "}",
  .na = "NA",
  .null = character(),
  .comment = "#",
  .literal = FALSE,
  .transformer = glue::identity_transformer,
  .trim = TRUE
)
```

## Arguments

- ...:

  Arguments passed on to
  [`ggplot2::labs`](https://ggplot2.tidyverse.org/reference/labs.html)

  `dictionary`

  :   A named character vector to serve as dictionary. Automatically
      derived labels, such as those based on variables will be matched
      with `names(dictionary)` and replaced by the matching entry in
      `dictionary`.

  `alt,alt_insight`

  :   Text used for the generation of alt-text for the plot. See
      [get_alt_text](https://ggplot2.tidyverse.org/reference/get_alt_text.html)
      for examples. `alt` can also be a function that takes the plot as
      input and returns text as output. `alt` also accepts rlang
      [lambda](https://rlang.r-lib.org/reference/as_function.html)
      function notation.

  `label`

  :   The title of the respective axis (for
      [`xlab()`](https://ggplot2.tidyverse.org/reference/labs.html) or
      [`ylab()`](https://ggplot2.tidyverse.org/reference/labs.html)) or
      of the plot (for
      [`ggtitle()`](https://ggplot2.tidyverse.org/reference/labs.html)).

  `plot`

  :   A ggplot object

- title:

  The text for the title.

- subtitle:

  The text for the subtitle for the plot which will be displayed below
  the title.

- caption:

  The text for the caption which will be displayed in the bottom-right
  of the plot by default.

- tag:

  The text for the tag label which will be displayed at the top-left of
  the plot by default.

- alt, alt_insight:

  Text used for the generation of alt-text for the plot. See
  [get_alt_text](https://ggplot2.tidyverse.org/reference/get_alt_text.html)
  for examples. `alt` can also be a function that takes the plot as
  input and returns text as output. `alt` also accepts rlang
  [lambda](https://rlang.r-lib.org/reference/as_function.html) function
  notation.

- source_note:

  Data source(s) to append to caption or use as caption (if no caption
  is supplied). Also supports glue string interpolation.

- source_sep, source_before, source_end:

  Strings used to separate caption (if supplied) and source note, add
  before the source note, and add after the source note.

- .sep:

  \[`character(1)`: ‘""’\]  
  Separator used to separate elements.

- .envir:

  \[`environment`:
  [`parent.frame()`](https://rdrr.io/r/base/sys.parent.html)\]  
  Environment to evaluate each expression in. Expressions are evaluated
  from left to right. If `.x` is an environment, the expressions are
  evaluated in that environment and `.envir` is ignored. If `NULL` is
  passed, it is equivalent to
  [`emptyenv()`](https://rdrr.io/r/base/environment.html).

- .open:

  \[`character(1)`: ‘\\’\]  
  The opening delimiter. Doubling the full delimiter escapes it.

- .close:

  \[`character(1)`: ‘\\’\]  
  The closing delimiter. Doubling the full delimiter escapes it.

- .na:

  \[`character(1)`: ‘NA’\]  
  Value to replace `NA` values with. If `NULL` missing values are
  propagated, that is an `NA` result will cause `NA` output. Otherwise
  the value is replaced by the value of `.na`.

- .null:

  \[`character(1)`: ‘character()’\]  
  Value to replace NULL values with. If
  [`character()`](https://rdrr.io/r/base/character.html) whole output is
  [`character()`](https://rdrr.io/r/base/character.html). If `NULL` all
  NULL values are dropped (as in
  [`paste0()`](https://rdrr.io/r/base/paste.html)). Otherwise the value
  is replaced by the value of `.null`.

- .comment:

  \[`character(1)`: ‘#’\]  
  Value to use as the comment character.

- .literal:

  \[`boolean(1)`: ‘FALSE’\]  
  Whether to treat single or double quotes, backticks, and comments as
  regular characters (vs. as syntactic elements), when parsing the
  expression string. Setting `.literal = TRUE` probably only makes sense
  in combination with a custom `.transformer`, as is the case with
  `glue_col()`. Regard this argument (especially, its name) as
  experimental.

- .transformer:

  \[`function`\]  
  A function taking two arguments, `text` and `envir`, where `text` is
  the unparsed string inside the glue block and `envir` is the execution
  environment. A `.transformer` lets you modify a glue block before,
  during, or after evaluation, allowing you to create your own custom
  `glue()`-like functions. See `vignette("transformers")` for examples.

- .trim:

  \[`logical(1)`: ‘TRUE’\]  
  Whether to trim the input template with
  [`trim()`](https://glue.tidyverse.org/reference/trim.html) or not.
