library(ggplot2)

nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

nc_map <- ggplot(data = nc) +
  layer_location_data(
    location = nc[1, ],
    mapping = aes(fill = NAME)
    )

nc_map +
  theme_legend(
    position = "topleft"
  )

nc_map +
  layer_labelled(
    data = getdata::get_location_data(nc[1, ], data = nc),
    mapping = aes(label = NAME)
  ) +
  theme_text(
    "Georgia"
  )
