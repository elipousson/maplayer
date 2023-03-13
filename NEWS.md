<!-- NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->

# maplayer development

* Add `layer_arrow()`.
* Add default filetype ("gpkg") to `layer_location_data()`.
* Add `ggplot2::geom_sf()` to re-exports.
* Update `map_icons` with [maki v8.0.1](https://github.com/mapbox/maki/releases/tag/v8.0.1) and [temaki v5.3.0](https://github.com/rapideditor/temaki/releases/tag/v5.3.0).
* Add `{papersize}` and `{cliExtras}` to Imports.
* Drop `{purrr}` from Imports and adopt rlang equivalents from `standalone-purrr.R`.

# maplayer 0.1.0.9000 (2022-10-13)

- feat: add working layer_count wrapping count_sf_ext
- refactor: add aes_label to layer_labelled
- feat: add support for functions to geom for layer_labelled
- feat: add location_lims param to layer_repel
- fix: pass label_col param from layer_repel to geom_sf_label_repel and geom_sf_text_repel
- refactor: add guides and margin to ggplot2 reexports
- feat: add magick-image support to ggsave_ext
- fix: correct minor issue w/ passing filetype as filename in ggsave_ext
- refactor: update DESCRIPTION + NAMESPACE
- refactor; import functions from rlang
- refactor: incorporate geom_sf_coordinates in layer_icon + correct package dependency checks
- feat: remove color parameter from layer_icon (was not working)
- refactor: geom_sf_coordinates sets mapping to aes() if mapping is NULL
- feat: add layer_repel wrapping ggrepel functions
- refactor: add geom_sf_coordinates helper (expect to add this helper to other functions as well)
- feat: add by_feature arg to layer_frame
- refactor: remove check_sf from make_frame (st_buffer_ext and st_bbox_ext should error on bad inputs)
- feat: get layer_icon working w/ inherited data (may still break if sf_column is not named "geometry")
- refactor: reorganize function to work w/ join_map_icons and get_map_icon helper functions
- refactor: add set_mask and set_neatline
- fix: correct handling of NULL data (require type or index to also be not NULL)
- refactor: remove with_smooth + with_shadow
- refactor: add is_fn to checks
- refactor: restructure logic w/ case_when + switch
- refactor: use set_neatline
- docs: rewrite documentation to reflect updates
- feat: add shadow_params
- refactor: switch to use use_fn utility
- refactor: reorder parameters
- docs: add definition for location
- feat: add set_neatline function to support neatline arg
- refactor: modify layer_neatline to (possibly) support data = NULL
- fix: address issue w/ line around panel when bgcolor is "white"
- refactor: add ggsave_params arg to reduce total args (uses new eval_tidy_fn utility for execution)
- refactor: make page parameter optional
- feat: add make_basemap function
- feat: add eval_tidy_fn utility function (supports with_smooth and with_shadow)
- refactor: split make_fn off from use_fn
- feat: add is_fn to check for formulas and functions
- refactor: clean up imports
- fix: correct parameter name in theme_margin from plot_width to block_width
- refactor: set default plot = NULL for stamp_inset_img
- refactor: use make_basemap
- refactor: rename context_aes to context_params
- feat: allow formula/function location values to subset context data
- feat: allow layer_location_data to inherit data (won't work for all geoms)
- refactor: create with_fn utility to apply smooth_params and shadow_params
- fix: correct check for sf context object
- feat: add basemap parameter using renamed make_basemap utility function
- refactor: pass expand parameter directly to ggplot2::coord_sf which effectively reverses the function (new default TRUE is equivalent to old default of FALSE)
- feat: add stamp_inset_img function
- refactor: add make_inset_element helper function (replacing layer_inset as the function calling patchwork)
- feat: add smooth_params + shadow_params functions to layer_location_data (replace smooth param for layer_location)
- feat: migrate make_markers from sfext to maplayer
- test: add test for layer_markers
- refactor: rename geom_fn to more general layer_fn
- style: adjust white space w/ styler
- feat: add styles to layer_frame
- docs: rename layer_show_location to layer_location
- refactor: switch default crs to 3857 and simplify return
- feat: add stat parameter to layer_marked
- docs: remove reference to overedge from the example
- docs: update layer_location_data + improve examples
- fix: correct handling of default geom parameter for layer_markers and layer_numbers
- feat: add a few more options for geom parameter (e.g. "sf_text" as well as "text")
- refactor: remove ... parameter from use_fn helper function
- refactor: set default crs to 3857
- refactor: add location parameter for consistency w/ other location functions
- refactor: don't add theme_void to neatline_layer
- refactor: improve handling of geom_fn parameter
- fix: remove url and path parameters from get_location_data call in layer_location_data
- refactor: add .. params to use_fn
- refactor: improve is_geom_pkg_installed
- refactor: add is_geom_pkg_installed helper and add map_icons to .onLoad
- fix: add walkerke/mapboxapi to Remotes
- docs: update package title
- feat: add as_basemap utility function
- docs: finish set-up for pkgdown site + minor tweaks to function docs


