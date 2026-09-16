# maplayer (development version)

- Improve `labs_ext()` handling of blank values, drop `use_md` feature, and add new `source_note` argument.
- Rename `theme_sf_axes()` to `theme_sf_axis()` and fix handling of axis.text.x and axis.text.y theme elements. (2024-02-21)
- Update `map_icons` with [temaki v5.7.0](https://github.com/rapideditor/temaki/releases/tag/v5.7.0) (2024-02-21)
- Update `map_icons` with [temaki v5.9.0](https://github.com/rapideditor/temaki/releases/tag/v5.9.0) (2024-09-06)
- Require `ggplot2 (>= 4.0.0)` and finish adapting the package to ggplot2 4.0, including its S7-based theme and scale objects. `is_ggplot()` replaces the deprecated `is.ggplot()`, and `get_last_plot()` replaces `last_plot()` in reexports. `theme_text()`, `theme_margin()`, and `theme_legend()` continue to use `ggplot2::theme_set()`/`theme_update()`/`theme_replace()` (rather than the newer `set_theme()`/`update_theme()`/`replace_theme()`) for their `method` argument; note `method = "update"`/`"replace"` still don't apply the constructed theme correctly and `method = "set"` (the default) remains recommended.
- Fix `geom_sf_label_ext()` erroring on every call due to a missing `face` argument, and passing `face` instead of `fontface` on to `ggplot2::geom_sf_label()`.
- Fix `labs_ext()`/internal `gg_labs()` dropping the caption entirely when `source_note` was supplied without an existing `caption`.
- Fix `layer_grouped()` erroring on multi-row `sf` data (a vectorized geometry-type check), on any additional arguments passed through `...` (a `!!!`-splicing bug), and on `palette` use (wrong argument name passed to `scale_group_data()`).
- Fix `scale_group_data()` erroring under ggplot2 4.0 due to a `scale_fill_discrete(type = <Scale object>)` pattern no longer supported by ggplot2's new S7 scales.
- Fix internal `aes_label()` erroring when `mapping` is `NULL`, which broke the default (no `mapping` supplied) use of `layer_labelled()`, `layer_repel()`, and `layer_count()`.
- Fix `layer_scaled()` erroring due to stale `{sfext}` column names and a call to a removed `sfext::as_dist_units()` signature.
- Fix `geom_sf_text_repel()` (used by `layer_repel()`) calling `ggrepel::geom_label_repel()` instead of `ggrepel::geom_text_repel()`.
- Fix missing `is_ggplot` import that broke the internal `check_ggplot()` helper.
- Add test coverage for previously untested functions, including `check_gg()`/`check_ggplot()`, `geom_sf_text_ext()`/`geom_sf_label_ext()`, `labs_ext()`, `layer_grouped()`, `layer_inset()`, `layer_labelled()`, `layer_repel()`, `layer_scaled()`, `scale_group_data()`/`group_data_pal()`, `set_basemap()`, internal ggplot2 utility helpers, and `map_icons`; update the `theme_ext` test snapshot for ggplot2 4.0's S7 theme elements.
- Fix `make_location_map()` erroring when `paper` was supplied, due to a `papersize::get_paper()` argument rename (`paper` -> `name`).
- Fix `make_social_map()` erroring for non-Mapbox `geom` values because a `bbox` object was passed directly to `layer_location_data()` instead of being converted to `sf` first.
- Add test coverage for `layer_image_path()`, `layer_mapbox()` (access-token error path), `make_atlas()`, `make_location_map()`, `make_layer_map()`, `make_social_map()`, `make_mapbox_map()` (access-token error path), `make_inset_map()`, and `stamp_inset_img()`, now that all `Suggests` packages are installed; overall test coverage is now ~76%.
- Add `exiftoolr` to Suggests and fix `make_image_map()`, which errored due to `filenamr::read_exif()` no longer supporting a `geometry` argument (now converted to `sf` with `sfext::df_to_sf()`) and due to passing a raw `bbox` object as `location` to `make_location_map()` instead of converting it to `sf` first. Add test coverage for `make_image_map()`; overall test coverage is now ~78%.
- Add test coverage for `save = TRUE` in `make_layer_map()`/`make_location_map()`/`make_social_map()`, now that the underlying `papersize::ggsave_ext()`/`ggsave_social()` file-path handling has been fixed upstream. `make_atlas(..., save = TRUE)` (which uses `papersize::map_ggsave_ext()` instead) still errors and remains untested pending a further upstream fix.

## 0.1.0.9003 (2023-08-25)

- Add `make_mapbox_map()` function and fix outstanding issues `make_layer_map()` helper.
- Add `{ggtext}` to Suggests (supporting new `use_md` parameter for `labs_ext()`)
- Add `{lwgeom}` to Suggests (avoiding error that popped up for `layer_count()`)
- Add `alpha` as a fixed aesthetic parameter for `layer_location()`
- Add `ggplot2::geom_sf_text` and `ggplot2::geom_sf_label` to re-exports
- Update `map_icons` with [temaki v5.4.0](https://github.com/rapideditor/temaki/releases/tag/v5.4.0)

# maplayer 0.1.0.9002 (2023-05-05)

- Add lower-level `make_layer_map()` helper for `make_location_map()`.
- Rename `make_basemap()` to `set_basemap()`

# maplayer 0.1.0.9001 (2023-04-10)

* Add `layer_arrow()`.
* Add default filetype ("gpkg") to `layer_location_data()`.
* Rename `null.ok` parameter to `allow_null`.
* Add `ggplot2::geom_sf()` to re-exports.
* Update `map_icons` with [maki v8.0.1](https://github.com/mapbox/maki/releases/tag/v8.0.1) and [temaki v5.3.0](https://github.com/rapideditor/temaki/releases/tag/v5.3.0).
* Add `{papersize}` and `{cliExtras}` to Imports.
* Remove `ggsave_ext()`, `map_ggsave_ext()`, and `ggsave_social()` from package functions but add to reexports from papersize.
* Drop `{purrr}` from Imports and adopt rlang equivalents from `standalone-purrr.R`.

# maplayer 0.1.0.9000 (2022-10-13)

- First version for package!
