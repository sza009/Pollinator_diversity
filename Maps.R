source("Descriptive.R")

# Map

#tibble with coordinates

my_coordinates <- tibble(lon = c(4.91796, 4.88906, 4.90968, 4.88338, 4.81368, 4.95728),                
             lat = c(60.68902, 60.76457, 60.72374, 60.78565, 60.82510, 60.79579),             
             st_id = c('B', 'L','S', 'Ø', 'K', 'U')) 


kart <- st_read("Basisdata/Fylker.geojson")

sites <- ggplot(kart) + 
  geom_sf() +
  coord_sf(xlim = c(-70000, -30000), ylim = c(6760000, 6789999)) +
  annotation_scale(location = "tl") +
  theme_bw() +
  geom_spatial_point(aes(x = lon, y = lat), 
                     data = my_coordinates,
                     colour = "red") +
  geom_spatial_text_repel(data = my_coordinates, aes(x= lon,
                                             y = lat,
                                             label = st_id), 
                          colour = "red",
                          size = 5) +
  theme(axis.title = element_blank())

#Europe map

world <- ne_countries(scale = 110, returnclass = "sf") 
big_scale_map <- ggplot() +
  geom_sf(data = world) +
  coord_sf(xlim = c(-20, 50), ylim = c(33, 80)) +
  theme_bw() + 
  geom_spatial_point(aes(x = lon, y = lat), 
                     data = my_coordinates,
                     colour = "red") +
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())

#combine
sites + 
  inset_element(big_scale_map, 
                left = 0.01, 
                right = 0.25, 
                top = 0.4, 
                bottom = 0.01)




