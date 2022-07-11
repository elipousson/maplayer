nc <- getdata::get_location(system.file("shape/nc.shp", package = "sf"), crs = 3857)

basemap <-
  ggplot2::ggplot() +
  ggplot2::theme_void() +
  layer_location_data(data = nc)

# icon can be set by name matching a name from map_icons
basemap +
  layer_icon(data = nc, icon = "point-start", size = 8)

# layer_icon can also use a column from the sf object
nc$icon <- rep(c("1", "2", "3", "4"), nrow(nc) / 4)

basemap +
  layer_icon(data = nc, iconname_col = "icon", size = 6)
