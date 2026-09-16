# Changelog

## maplayer (development version)

- Improve
  [`labs_ext()`](https://elipousson.github.io/maplayer/reference/labs_ext.md)
  handling of blank values, drop `use_md` feature, and add new
  `source_note` argument.
- Rename `theme_sf_axes()` to
  [`theme_sf_axis()`](https://elipousson.github.io/maplayer/reference/layer_neatline.md)
  and fix handling of axis.text.x and axis.text.y theme elements.
  (2024-02-21)
- Update `map_icons` with [temaki
  v5.7.0](https://github.com/rapideditor/temaki/releases/tag/v5.7.0)
  (2024-02-21)
- Update `map_icons` with [temaki
  v5.9.0](https://github.com/rapideditor/temaki/releases/tag/v5.9.0)
  (2024-09-06)
- Require `ggplot2 (>= 4.0.0)` and finish adapting the package to
  ggplot2 4.0, including its S7-based theme and scale objects.
  [`is_ggplot()`](https://ggplot2.tidyverse.org/reference/is_tests.html)
  replaces the deprecated
  [`is.ggplot()`](https://ggplot2.tidyverse.org/reference/is_tests.html),
  and
  [`get_last_plot()`](https://ggplot2.tidyverse.org/reference/get_last_plot.html)
  replaces
  [`last_plot()`](https://ggplot2.tidyverse.org/reference/get_last_plot.html)
  in reexports.
  [`theme_text()`](https://elipousson.github.io/maplayer/reference/theme_ext.md),
  [`theme_margin()`](https://elipousson.github.io/maplayer/reference/theme_ext.md),
  and
  [`theme_legend()`](https://elipousson.github.io/maplayer/reference/theme_ext.md)
  continue to use
  [`ggplot2::theme_set()`](https://ggplot2.tidyverse.org/reference/get_theme.html)/[`theme_update()`](https://ggplot2.tidyverse.org/reference/get_theme.html)/[`theme_replace()`](https://ggplot2.tidyverse.org/reference/get_theme.html)
  (rather than the newer
  [`set_theme()`](https://ggplot2.tidyverse.org/reference/get_theme.html)/[`update_theme()`](https://ggplot2.tidyverse.org/reference/get_theme.html)/[`replace_theme()`](https://ggplot2.tidyverse.org/reference/get_theme.html))
  for their `method` argument; note `method = "update"`/`"replace"`
  still don’t apply the constructed theme correctly and `method = "set"`
  (the default) remains recommended.
- Fix
  [`geom_sf_label_ext()`](https://elipousson.github.io/maplayer/reference/geom_sf_text_ext.md)
  erroring on every call due to a missing `face` argument, and passing
  `face` instead of `fontface` on to
  [`ggplot2::geom_sf_label()`](https://ggplot2.tidyverse.org/reference/ggsf.html).
- Fix
  [`labs_ext()`](https://elipousson.github.io/maplayer/reference/labs_ext.md)/internal
  `gg_labs()` dropping the caption entirely when `source_note` was
  supplied without an existing `caption`.
- Fix
  [`layer_grouped()`](https://elipousson.github.io/maplayer/reference/layer_grouped.md)
  erroring on multi-row `sf` data (a vectorized geometry-type check), on
  any additional arguments passed through `...` (a `!!!`-splicing bug),
  and on `palette` use (wrong argument name passed to
  [`scale_group_data()`](https://elipousson.github.io/maplayer/reference/scale_group_data.md)).
- Fix
  [`scale_group_data()`](https://elipousson.github.io/maplayer/reference/scale_group_data.md)
  erroring under ggplot2 4.0 due to a
  `scale_fill_discrete(type = <Scale object>)` pattern no longer
  supported by ggplot2’s new S7 scales.
- Fix internal `aes_label()` erroring when `mapping` is `NULL`, which
  broke the default (no `mapping` supplied) use of
  [`layer_labelled()`](https://elipousson.github.io/maplayer/reference/layer_labelled.md),
  [`layer_repel()`](https://elipousson.github.io/maplayer/reference/layer_repel.md),
  and
  [`layer_count()`](https://elipousson.github.io/maplayer/reference/layer_count.md).
- Fix
  [`layer_scaled()`](https://elipousson.github.io/maplayer/reference/layer_scaled.md)
  erroring due to stale [sfext](https://github.com/elipousson/sfext)
  column names and a call to a removed
  [`sfext::as_dist_units()`](https://elipousson.github.io/sfext/reference/is_dist_units.html)
  signature.
- Fix
  [`geom_sf_text_repel()`](https://elipousson.github.io/maplayer/reference/layer_repel.md)
  (used by
  [`layer_repel()`](https://elipousson.github.io/maplayer/reference/layer_repel.md))
  calling
  [`ggrepel::geom_label_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html)
  instead of
  [`ggrepel::geom_text_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html).
- Fix missing `is_ggplot` import that broke the internal
  `check_ggplot()` helper.
- Add test coverage for previously untested functions, including
  `check_gg()`/`check_ggplot()`,
  [`geom_sf_text_ext()`](https://elipousson.github.io/maplayer/reference/geom_sf_text_ext.md)/[`geom_sf_label_ext()`](https://elipousson.github.io/maplayer/reference/geom_sf_text_ext.md),
  [`labs_ext()`](https://elipousson.github.io/maplayer/reference/labs_ext.md),
  [`layer_grouped()`](https://elipousson.github.io/maplayer/reference/layer_grouped.md),
  [`layer_inset()`](https://elipousson.github.io/maplayer/reference/layer_inset.md),
  [`layer_labelled()`](https://elipousson.github.io/maplayer/reference/layer_labelled.md),
  [`layer_repel()`](https://elipousson.github.io/maplayer/reference/layer_repel.md),
  [`layer_scaled()`](https://elipousson.github.io/maplayer/reference/layer_scaled.md),
  [`scale_group_data()`](https://elipousson.github.io/maplayer/reference/scale_group_data.md)/[`group_data_pal()`](https://elipousson.github.io/maplayer/reference/scale_group_data.md),
  [`set_basemap()`](https://elipousson.github.io/maplayer/reference/set_basemap.md),
  internal ggplot2 utility helpers, and `map_icons`; update the
  `theme_ext` test snapshot for ggplot2 4.0’s S7 theme elements.
- Fix
  [`make_location_map()`](https://elipousson.github.io/maplayer/reference/make_location_map.md)
  erroring when `paper` was supplied, due to a
  [`papersize::get_paper()`](https://elipousson.github.io/papersize/reference/get_page_size.html)
  argument rename (`paper` -\> `name`).
- Fix
  [`make_social_map()`](https://elipousson.github.io/maplayer/reference/make_location_map.md)
  erroring for non-Mapbox `geom` values because a `bbox` object was
  passed directly to
  [`layer_location_data()`](https://elipousson.github.io/maplayer/reference/layer_location_data.md)
  instead of being converted to `sf` first.
- Add test coverage for
  [`layer_image_path()`](https://elipousson.github.io/maplayer/reference/layer_image_path.md),
  [`layer_mapbox()`](https://elipousson.github.io/maplayer/reference/layer_mapbox.md)
  (access-token error path),
  [`make_atlas()`](https://elipousson.github.io/maplayer/reference/make_atlas.md),
  [`make_location_map()`](https://elipousson.github.io/maplayer/reference/make_location_map.md),
  [`make_layer_map()`](https://elipousson.github.io/maplayer/reference/make_location_map.md),
  [`make_social_map()`](https://elipousson.github.io/maplayer/reference/make_location_map.md),
  [`make_mapbox_map()`](https://elipousson.github.io/maplayer/reference/make_mapbox_map.md)
  (access-token error path),
  [`make_inset_map()`](https://elipousson.github.io/maplayer/reference/layer_inset.md),
  and
  [`stamp_inset_img()`](https://elipousson.github.io/maplayer/reference/layer_inset.md),
  now that all `Suggests` packages are installed; overall test coverage
  is now ~76%.
- Add `exiftoolr` to Suggests and fix
  [`make_image_map()`](https://elipousson.github.io/maplayer/reference/make_location_map.md),
  which errored due to
  [`filenamr::read_exif()`](https://elipousson.github.io/filenamr/reference/read_exif.html)
  no longer supporting a `geometry` argument (now converted to `sf` with
  [`sfext::df_to_sf()`](https://elipousson.github.io/sfext/reference/sf_to_df.html))
  and due to passing a raw `bbox` object as `location` to
  [`make_location_map()`](https://elipousson.github.io/maplayer/reference/make_location_map.md)
  instead of converting it to `sf` first. Add test coverage for
  [`make_image_map()`](https://elipousson.github.io/maplayer/reference/make_location_map.md);
  overall test coverage is now ~78%.
- Add test coverage for `save = TRUE` in
  [`make_layer_map()`](https://elipousson.github.io/maplayer/reference/make_location_map.md)/[`make_location_map()`](https://elipousson.github.io/maplayer/reference/make_location_map.md)/[`make_social_map()`](https://elipousson.github.io/maplayer/reference/make_location_map.md)/[`make_atlas()`](https://elipousson.github.io/maplayer/reference/make_atlas.md),
  now that the underlying
  [`papersize::ggsave_ext()`](https://elipousson.github.io/papersize/reference/ggsave_ext.html)/[`ggsave_social()`](https://elipousson.github.io/papersize/reference/ggsave_ext.html)/[`map_ggsave_ext()`](https://elipousson.github.io/papersize/reference/ggsave_ext.html)
  file-path handling has been fixed upstream; `make_atlas.R` reaches
  100% test coverage.

### 0.1.0.9003 (2023-08-25)

- Add
  [`make_mapbox_map()`](https://elipousson.github.io/maplayer/reference/make_mapbox_map.md)
  function and fix outstanding issues
  [`make_layer_map()`](https://elipousson.github.io/maplayer/reference/make_location_map.md)
  helper.
- Add [ggtext](https://wilkelab.org/ggtext/) to Suggests (supporting new
  `use_md` parameter for
  [`labs_ext()`](https://elipousson.github.io/maplayer/reference/labs_ext.md))
- Add [lwgeom](https://r-spatial.github.io/lwgeom/) to Suggests
  (avoiding error that popped up for
  [`layer_count()`](https://elipousson.github.io/maplayer/reference/layer_count.md))
- Add `alpha` as a fixed aesthetic parameter for
  [`layer_location()`](https://elipousson.github.io/maplayer/reference/layer_location.md)
- Add
  [`ggplot2::geom_sf_text`](https://ggplot2.tidyverse.org/reference/ggsf.html)
  and
  [`ggplot2::geom_sf_label`](https://ggplot2.tidyverse.org/reference/ggsf.html)
  to re-exports
- Update `map_icons` with [temaki
  v5.4.0](https://github.com/rapideditor/temaki/releases/tag/v5.4.0)

## maplayer 0.1.0.9002 (2023-05-05)

- Add lower-level
  [`make_layer_map()`](https://elipousson.github.io/maplayer/reference/make_location_map.md)
  helper for
  [`make_location_map()`](https://elipousson.github.io/maplayer/reference/make_location_map.md).
- Rename
  [`make_basemap()`](https://elipousson.github.io/maplayer/reference/set_basemap.md)
  to
  [`set_basemap()`](https://elipousson.github.io/maplayer/reference/set_basemap.md)

## maplayer 0.1.0.9001 (2023-04-10)

- Add
  [`layer_arrow()`](https://elipousson.github.io/maplayer/reference/layer_arrow.md).
- Add default filetype (“gpkg”) to
  [`layer_location_data()`](https://elipousson.github.io/maplayer/reference/layer_location_data.md).
- Rename `null.ok` parameter to `allow_null`.
- Add
  [`ggplot2::geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html)
  to re-exports.
- Update `map_icons` with [maki
  v8.0.1](https://github.com/mapbox/maki/releases/tag/v8.0.1) and
  [temaki
  v5.3.0](https://github.com/rapideditor/temaki/releases/tag/v5.3.0).
- Add [papersize](https://github.com/elipousson/papersize) and
  [cliExtras](https://github.com/elipousson/cliExtras) to Imports.
- Remove
  [`ggsave_ext()`](https://elipousson.github.io/papersize/reference/ggsave_ext.html),
  [`map_ggsave_ext()`](https://elipousson.github.io/papersize/reference/ggsave_ext.html),
  and
  [`ggsave_social()`](https://elipousson.github.io/papersize/reference/ggsave_ext.html)
  from package functions but add to reexports from papersize.
- Drop [purrr](https://purrr.tidyverse.org/) from Imports and adopt
  rlang equivalents from `standalone-purrr.R`.

## maplayer 0.1.0.9000 (2022-10-13)

- First version for package!
