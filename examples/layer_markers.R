nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
nc <- sf::st_transform(nc, 3857)

basemap <-
  ggplot() +
  layer_location_data(
    data = nc,
    fill = NA
  )

basemap +
  layer_markers(
    data = nc[1:10],
    mapping = aes(size = AREA),
    make = TRUE
  )

large_nc <-
  getdata::get_location_data(
    data = nc,
    fn = ~ dplyr::filter(.x, AREA > 0.2)
  )

large_nc$number <- 1
large_nc$dist <- 2

basemap +
  layer_numbers(
    data = large_nc,
    mapping = aes(fill = NAME),
    sort = "dist_xmax_ymin",
    num_style = "Roman",
    geom = "label",
    size = 3
    ) +
  guides(fill = "none")

