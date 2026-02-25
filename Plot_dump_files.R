# --- Load packages ----
library(ncdf4)
library(tidyverse)
library(stringr)

# --- Function: extract total cs from a NetCDF file ----
extract_cs_total <- function(file_path) {
  ncd <- nc_open(file_path)
  on.exit(nc_close(ncd))
  
  cs_vals <- tryCatch(ncvar_get(ncd, "cs"), error = function(e) NA)
  
  if (!all(is.na(cs_vals))) {
    cs_total <- sum(cs_vals, na.rm = TRUE)
  } else {
    cs_total <- NA
  }
  
  return(cs_total)
}

# --- Function: extract metadata from filename ---
parse_filename <- function(fname) {
  match <- str_match(fname, "^part(\\d+)_([^-]+)-JULES.*\\.spin(\\d+)\\.(\\d{8})\\.0\\.nc$")
  
  if (any(is.na(match[1, 2:5]))) {
    warning(paste("Filename doesn't match expected pattern:", fname))
    return(tibble(part = NA_integer_, site = NA_character_,
                  spin = NA_integer_, year = NA_integer_))
  }
  
  part <- as.integer(match[1, 2])
  site <- match[1, 3]
  spin <- as.integer(match[1, 4])
  year <- as.integer(substr(match[1, 5], 1, 4))
  
  tibble(part = part, site = site, spin = spin, year = year)
}

# --- Set base directory ---
base_dir <- "C:/Users/efa206/OneDrive - University of Exeter/Desktop/JULES_Output/Spin_1000"

# --- List all NetCDF files across all sites ---
file_list <- list.files(base_dir, pattern = "\\.nc$", full.names = TRUE)

# --- Extract cs_total and metadata from filenames ---
cs_all_sites <- map_dfr(file_list, function(file) {
  meta <- parse_filename(basename(file))
  cs <- extract_cs_total(file)
  tibble(file = file, cs_total = cs,
         part = meta$part, spin = meta$spin,
         year = meta$year, site = meta$site)
})

# --- Compute continuous spin index within each site ---
cs_all_sites <- cs_all_sites |>
  filter(!is.na(site)) |>
  group_by(site) |>
  arrange(part, spin, year, .by_group = TRUE) |>
  mutate(continuous_spin = row_number()) |>
  ungroup()

# --- Plot 1: Combined line plots for all sites (continuous spin) ---
p_all <- ggplot(cs_all_sites, aes(x = continuous_spin, y = cs_total, color = site)) +
  geom_line(linewidth = 1) +
  labs(title = "Soil Carbon Equilibrium After Spin-Up (All Sites)",
       x = "Spin (Continuous)", y = "Total Soil Carbon (cs_total)", color = "Site") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = NA))

# --- Plot 2: Faceted plot (1 panel per site) ---
p_facet <- ggplot(cs_all_sites, aes(x = continuous_spin, y = cs_total)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  facet_wrap(~site, scales = "free_y", ncol = 4) +
  labs(#title = "Soil Carbon Spin-Up Trajectory",
       x = "Spin years", y = expression("Total Soil Carbon (kg" ~ m^{-2}*")")) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = "none",
    strip.text = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 16, face = "bold")
  )

# --- Show plots ---
print(p_all)
print(p_facet)

# --- Optional: Save plots and data ---
ggsave("cs_spinup_all_sites_combined.png", p_all, width = 10, height = 6)
ggsave("cs_spinup_all_sites_facet2.png", p_facet, width = 16, height = 10)
# write_csv(cs_all_sites, "cs_spinup_summary_all_sites.csv")
