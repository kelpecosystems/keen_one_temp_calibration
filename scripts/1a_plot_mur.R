library(ggplot2)
library(purrr)
library(dplyr)
library(glue)
library(readr)
library(patchwork)


sst <- map_df(2013:2022,
              ~read_csv(glue("raw_data/MURSST41/{.x}.csv")))

tseries_plot <- ggplot(sst,
       aes(x = Date, y = value, 
           color = Latitude,
           group = ID)) +
  geom_point() +
  geom_line() +
  theme_minimal(base_size = 15) +
  labs(title = "MUR SST 4.1", 
       x = "", 
       y = "Sea Surface Temperature\n(Degrees C)")+
  scale_colour_distiller(palette = "RdYlBu", direction = 1,
                         guide = "none")



###

keen_bbox <- st_bbox(c(ymin = 41.0158484503146, 
                       xmin = -72.41734371426318,
                       ymax = 44.630811336788774, 
                       xmax = -65.96160946413212),
                     crs = 4326)

land <- ne_countries(scale = "large", returnclass = "sf") |>
  st_crop(keen_bbox)

ll_info <- read_excel("raw_data/lat_longs.xlsx") |>
  group_by(Latitude, Longitude) |>
  slice_head() |>
  ungroup() |>
  dplyr::select(Site, Latitude, Longitude) |>
  st_as_sf(coords = c("Longitude", "Latitude"),
           crs = 4326,
           remove = FALSE)



site_plot <- ggplot() +
  geom_sf(data = land) +
  geom_sf(data = ll_info, aes(color = Latitude),
          size = 4) +
  theme_minimal(base_size = 15) +
  scale_colour_distiller(palette = "RdYlBu", direction = 1,
                         guide = "none")

site_plot + tseries_plot + plot_layout(guides='collect') &
  theme(legend.position='bottom',
        legend.key.size = unit(1, 'cm'))

ggsave("figures/mur_and_map.jpg", width = 10)
