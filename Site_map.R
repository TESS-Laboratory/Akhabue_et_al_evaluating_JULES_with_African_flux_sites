# Load necessary libraries ----
library(ggplot2)
library(sf)
library(rnaturalearth)
library(dplyr)
library(readr)
library(ggrepel)

# Load the dataset ----
flux_sites <- read_csv("FluxTowerSites.csv")

# Convert the dataset to an sf object
flux_sites_sf <- st_as_sf(flux_sites, coords = c("Site_Longitude", "Site_Latitude"), crs = 4326)

# Get Africa map
africa <- ne_countries(continent = "Africa", returnclass = "sf")

# Plot the flux sites on the Africa map
flux_map <- ggplot() +
  geom_sf(data = africa, fill = "lightgrey", color = "black") +
  geom_sf(data = flux_sites_sf, aes(color = IGBP), size = 4) +
  geom_text_repel(data = flux_sites, aes(x = Site_Longitude, y = Site_Latitude, label = Site_Code),
                  size = 7, box.padding = 0.8, point.padding = 0.1, max.overlaps = 30) +
  labs(color = "Ecosystem Type") +
  theme_classic() +  # Set a white background
  theme(axis.text = element_blank(),  # Remove axis labels
        axis.title = element_blank(),
        panel.grid = element_blank(),  # Remove grid lines
        panel.background = element_rect(fill = "white", color = NA),
        legend.text = element_text(size = 20),  # Increase legend text size
        legend.title = element_text(size = 16, face = "bold"),
        legend.position = c(0.17, 0.13))  # Increase legend title size


flux_map

# Save the plot
ggsave("flux_sites_map.png", plot = flux_map, width = 10, height = 9, dpi = 300)





# plot for whittaker biome, for eas of combination ----
# to run this code, run the code in clim_data

Whittaker_plot <- ggplot() +
  
  # Biome polygons
  geom_polygon(data = Whittaker_biomes,
               aes(x = temp_c, y = precp_cm * 10, fill = biome),
               color = "gray98", size = 0.5) +
  
  # Site points
  geom_point(data = site_data_2, aes(x = MAT, y = MAP, color = Site), size = 3) +
  
  # Biome colors
  scale_fill_manual(
    name = "Whittaker Biomes",
    values = c(
      "Tropical rain forest" = "grey72",
      "Temperate rain forest" = "grey80",
      "Boreal forest" = "grey88",
      "Temperate seasonal forest" = "#A5C790",
      "Tropical seasonal forest/savanna" = "#e6da00",
      "Woodland/shrubland" = "#DCBB50",
      "Temperate grassland/desert" = "#FCD57A",
      "Subtropical desert" = "#D16E3F",
      "Tundra" = "#C1E1DD"
    )
  ) +
  
  # Guides order: Whittaker first (1), Sites second (2)
  guides(
    fill = guide_legend(order = 1),
    color = guide_legend(order = 2)
  ) +
  
  # Labels and theme
  labs(
    x = expression("Mean Annual Temperature " ( degree~C)),
    y = "Mean Annual Precipitation (mm)"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 19),
    axis.text = element_text(face = "bold", size = 19),
    legend.position = c(0.3, 0.59),  # Adjust as needed
    legend.text = element_text(size = 19),
    legend.title = element_text(size = 17, face = "bold")
  )

Whittaker_plot


Whittaker_plot_tagged <- Whittaker_plot + 
  theme(plot.tag = element_text(face = "bold", size = 20))

flux_map_tagged <- flux_map + 
  theme(plot.tag = element_text(face = "bold", size = 20))



combined_plot <- (flux_map_tagged | Whittaker_plot_tagged) +
  plot_annotation(tag_levels = 'A')



combined_plot 



ggsave("flux_whittaker22.png", 
       plot = combined_plot,   
       width = 19, height = 10,  # Adjust dimensions
       dpi = 600, bg = "white")  # High resolution


