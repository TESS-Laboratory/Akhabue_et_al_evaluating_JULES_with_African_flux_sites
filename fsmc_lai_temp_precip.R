# --- Load packages ----
library(ncdf4)
library(tidyverse)
library(stringr)
library(lubridate)


# ---- plotting total fsmc ----
# --- Function to extract total fsmc time series from a NetCDF file ---
extract_fsmc_total <- function(file_path) {
  ncd <- nc_open(file_path)
  on.exit(nc_close(ncd))
  
  fsmc_vals <- ncvar_get(ncd, "fsmc")
  dims <- dim(fsmc_vals)
  cat("File:", basename(file_path), "- dims:", paste(dims, collapse = " x "), "\n")
  
  if (length(dims) == 4) {
    # Expected [time, pft, y, x], average over x & y
    fsmc_time_pft <- apply(fsmc_vals, c(1,2), mean, na.rm = TRUE)
  } else if (length(dims) == 3) {
    # Could be [time, pft, tile] or similar, average last dim
    fsmc_time_pft <- apply(fsmc_vals, c(1,2), mean, na.rm = TRUE)
  } else if (length(dims) == 2) {
    # Looks like [pft, time], need to transpose
    cat("Detected shape [pft, time], transposing.\n")
    fsmc_time_pft <- t(fsmc_vals)
  } else {
    stop("Unexpected shape for fsmc array.")
  }
  
  # Sum across pfts
  fsmc_total <- rowSums(fsmc_time_pft, na.rm = TRUE)
  
  # Convert time to dates
  time_vals <- ncvar_get(ncd, "time")
  time_units <- ncatt_get(ncd, "time", "units")$value
  ref_time <- as_datetime(substr(time_units, 15, 33), tz = "UTC")
  dates <- ref_time + seconds(time_vals)
  
  tibble(date = dates, fsmc_total = fsmc_total)
}

# --- Function to parse site name from filename ---
parse_site_name <- function(fname) {
  match <- str_match(fname, "^part6_([^-]+)-JULES")
  if (is.na(match[1,2])) return(NA_character_)
  return(match[1,2])
}

# --- Set base directory ---
base_dir <- "C:/Users/efa206/OneDrive - University of Exeter/Desktop/JULES_Output/File_1000"

# --- List only part6 files ---
file_list <- list.files(base_dir, pattern = "^part6_.*\\.nc$", full.names = TRUE)

# --- Extract fsmc time series for all sites ---
fsmc_all_sites <- map_dfr(file_list, function(file) {
  site <- parse_site_name(basename(file))
  fsmc_df <- extract_fsmc_total(file)
  fsmc_df$site <- site
  return(fsmc_df)
})

# --- Plot combined line plot for all sites ---
#p_all_fsmc <- ggplot(fsmc_all_sites, aes(x = date, y = fsmc_total, color = site)) +
#  geom_line(linewidth = 1) +
# labs(title = "Time Series of fsmc (All Sites)",
#      x = "Date", y = "Total fsmc", color = "Site") +
# theme_minimal()

# --- Faceted plot ---
p_facet_fsmc <- ggplot(fsmc_all_sites, aes(x = date, y = fsmc_total)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  facet_wrap(~site, scales = "free", ncol = 3) +
  labs(title = "fsmc Time Series by Site",
       x = "Date", y = "Total fsmc") +
  theme_minimal()

# --- Show plots ---
#print(p_all_fsmc)
print(p_facet_fsmc)

# --- Save plots and data ---
#ggsave("fsmc_time_series_all_sites.png", p_all_fsmc, width = 10, height = 6, bg = "white")
ggsave("fsmc_time_series_faceted.png", p_facet_fsmc, width = 16, height = 10, bg = "white")
write_csv(fsmc_all_sites, "fsmc_time_series_all_sites.csv")





# ---- plotting fsmc by PFT ----





# --- Function to extract fsmc time series by PFT from a NetCDF file ---
extract_fsmc_by_pft <- function(file_path) {
  ncd <- nc_open(file_path)
  on.exit(nc_close(ncd))
  
  fsmc_vals <- ncvar_get(ncd, "fsmc")
  dims <- dim(fsmc_vals)
  cat("File:", basename(file_path), "- dims:", paste(dims, collapse = " x "), "\n")
  
  if (length(dims) == 4) {
    # [time, pft, y, x], average over x and y
    fsmc_time_pft <- apply(fsmc_vals, c(1,2), mean, na.rm = TRUE)
  } else if (length(dims) == 3) {
    # [time, pft, tile] or similar
    fsmc_time_pft <- apply(fsmc_vals, c(1,2), mean, na.rm = TRUE)
  } else if (length(dims) == 2) {
    cat("Detected shape [pft, time], transposing.\n")
    fsmc_time_pft <- t(fsmc_vals)
  } else {
    stop("Unexpected shape for fsmc array.")
  }
  
  # Time handling
  time_vals <- ncvar_get(ncd, "time")
  time_units <- ncatt_get(ncd, "time", "units")$value
  ref_time <- as_datetime(substr(time_units, 15, 33), tz = "UTC")
  dates <- ref_time + seconds(time_vals)
  
  # Create tibble in long format: one row per time x pft
  fsmc_df <- as_tibble(fsmc_time_pft) %>%
    mutate(date = dates) %>%
    pivot_longer(-date, names_to = "pft", values_to = "fsmc") %>%
    mutate(pft = as.integer(str_remove(pft, "V")))  # clean up column names
  
  return(fsmc_df)
}

# --- Function to parse site name from filename ---
parse_site_name <- function(fname) {
  match <- str_match(fname, "^part6_([^-]+)-JULES")
  if (is.na(match[1,2])) return(NA_character_)
  return(match[1,2])
}



# --- Extract fsmc by PFT for all sites ---
fsmc_pft_all_sites <- map_dfr(file_list, function(file) {
  site <- parse_site_name(basename(file))
  fsmc_df <- extract_fsmc_by_pft(file)
  fsmc_df$site <- site
  return(fsmc_df)
})

# --- Plot: Facet by site, colour by PFT ---
p_fsmc_pft <- ggplot(fsmc_pft_all_sites, aes(x = date, y = fsmc, color = as.factor(pft))) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~site, scales = "free", ncol = 3) +
  labs(title = "fsmc by PFT and Site",
       x = "Date", y = "fsmc",
       color = "PFT") +
  theme_minimal()

# --- Show plot ---
print(p_fsmc_pft)

# --- Save ---
ggsave("fsmc_by_pft_facet_by_site.png", p_fsmc_pft, width = 16, height = 10, bg = "white")
write_csv(fsmc_pft_all_sites, "fsmc_by_pft_all_sites.csv")





# ---- Stacked area plot by site ----
p_fsmc_stack <- ggplot(fsmc_pft_all_sites, aes(x = date, y = fsmc, fill = as.factor(pft))) +
  geom_area(alpha = 0.8, linewidth = 0.1) +
  facet_wrap(~site, scales = "free", ncol = 4) +
  labs(title = "Stacked fsmc by PFT and Site",
       x = "Date", y = "Total fsmc",
       fill = "PFT") +
  theme_minimal()

# --- Show plot ---
print(p_fsmc_stack)

# --- Save to file ---
ggsave("fsmc_stacked_by_pft_facet_by_site.png", p_fsmc_stack, width = 16, height = 10, bg = "white")



# ---- fsmc_gb ----


extract_fsmc_gb <- function(file_path) {
  ncd <- nc_open(file_path)
  on.exit(nc_close(ncd))
  
  fsmc_gb_vals <- ncvar_get(ncd, "fsmc_gb")
  dims <- dim(fsmc_gb_vals)
  cat("File:", basename(file_path), "- fsmc_gb dims:", paste(dims, collapse = " x "), "\n")
  
  if (length(dims) == 3) {
    fsmc_gb <- fsmc_gb_vals[,1,1]
  } else if (length(dims) == 1) {
    fsmc_gb <- fsmc_gb_vals
  } else {
    stop("Unexpected shape for fsmc_gb array.")
  }
  
  time_vals <- ncvar_get(ncd, "time")
  time_units <- ncatt_get(ncd, "time", "units")$value
  ref_time <- as_datetime(substr(time_units, 15, 33), tz = "UTC")
  dates <- ref_time + seconds(time_vals)
  
  tibble(date = dates, fsmc_gb = fsmc_gb)
}



fsmc_gb_all_sites <- map_dfr(file_list, function(file) {
  site <- parse_site_name(basename(file))
  fsmc_df <- extract_fsmc_gb(file)
  fsmc_df$site <- site
  return(fsmc_df)
})




fsmc_gb_all_sites <- fsmc_gb_all_sites %>%
  mutate(site = dplyr::recode(site,
                              "BW_GUM"      = "BW_GUM - Wetland",
                              "BW_NXR"      = "BW_NXR - Wetland",
                              "CG_TCH"      = "CG_TCH - Grassland",
                              "GH_ANK"      = "GH_ANK - Forest",
                              "ML_AGG"      = "ML_AGG - Grassland",
                              "NE_WAF"      = "NE_WAF - Savanna",
                              "NE_WAM"      = "NE_WAM - Savanna",
                              "SD_DEM"      = "SD_DEM - Grassland",
                              "SN_DHR"      = "SN_DHR - Grassland",
                              "SN_NKR"      = "SN_NKR - Cropland",
                              "SN_RAG"      = "SN_RAG - Cropland",
                              "UG_JIN"      = "UG_JIN - Wetland",
                              "ZA_CATH"     = "ZA_CATH - Grassland",
                              "ZA_KRU"      = "ZA_KRU - Savanna",
                              "ZA_WGN"      = "ZA_WGN - Grassland",
                              "ZM_MON"      = "ZM_MON - Forest"
  ))




p_facet_fsmc_gb <- ggplot(fsmc_gb_all_sites, aes(x = date, y = fsmc_gb)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  facet_wrap(~site, scales = "free", ncol = 3) +
  scale_y_continuous(limits = c(0,1.5)) +
  labs(#title = "fsmc_gb Time Series by Site",
    x = "Date", y = "fsmc_gb") +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 16, face = "bold"))



print(p_facet_fsmc_gb)
ggsave("new_fsmc_gb_time_series_faceted.png", p_facet_fsmc_gb, width = 18, height = 18, bg = "white")
#write_csv(fsmc_gb_all_sites, "fsmc_gb_time_series_all_sites.csv")










# --- plotting total LAI ----
# --- Function to extract total LAI time series from a NetCDF file ---
extract_lai_total <- function(file_path) {
  ncd <- nc_open(file_path)
  on.exit(nc_close(ncd))
  
  lai_vals <- ncvar_get(ncd, "lai")
  dims <- dim(lai_vals)
  cat("File:", basename(file_path), "- dims:", paste(dims, collapse = " x "), "\n")
  
  if (length(dims) == 4) {
    # Expected [time, pft, y, x], average over x & y
    lai_time_pft <- apply(lai_vals, c(1,2), mean, na.rm = TRUE)
  } else if (length(dims) == 3) {
    # Could be [time, pft, tile] or similar, average last dim
    lai_time_pft <- apply(lai_vals, c(1,2), mean, na.rm = TRUE)
  } else if (length(dims) == 2) {
    # Looks like [pft, time], need to transpose
    cat("Detected shape [pft, time], transposing.\n")
    lai_time_pft <- t(lai_vals)
  } else {
    stop("Unexpected shape for lai array.")
  }
  
  # Sum across pfts
  lai_total <- rowSums(lai_time_pft, na.rm = TRUE)
  
  # Convert time to dates
  time_vals <- ncvar_get(ncd, "time")
  time_units <- ncatt_get(ncd, "time", "units")$value
  ref_time <- as_datetime(substr(time_units, 15, 33), tz = "UTC")
  dates <- ref_time + seconds(time_vals)
  
  tibble(date = dates, lai_total = lai_total)
}

# --- Function to parse site name from filename ---
parse_site_name <- function(fname) {
  match <- str_match(fname, "^part6_([^-]+)-JULES")
  if (is.na(match[1,2])) return(NA_character_)
  return(match[1,2])
}



# --- Extract LAI time series for all sites ---
lai_all_sites <- map_dfr(file_list, function(file) {
  site <- parse_site_name(basename(file))
  lai_df <- extract_lai_total(file)
  lai_df$site <- site
  return(lai_df)
})

# --- Plot combined line plot for all sites ---
#p_all_lai <- ggplot(lai_all_sites, aes(x = date, y = lai_total, color = site)) +
#  geom_line(linewidth = 1) +
# labs(title = "Time Series of LAI (All Sites)",
#      x = "Date", y = "Total LAI", color = "Site") +
# theme_minimal()

# --- Faceted plot ---
p_facet_lai <- ggplot(lai_all_sites, aes(x = date, y = lai_total)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  facet_wrap(~site, scales = "free", ncol = 3) +
  labs(title = "LAI Time Series by Site",
       x = "Date", y = "Total LAI") +
  theme_minimal()

# --- Show plots ---
#print(p_all_lai)
print(p_facet_lai)

# --- Save plots and data ---
#ggsave("lai_time_series_all_sites.png", p_all_lai, width = 10, height = 6, bg = "white")
ggsave("lai_time_series_faceted.png", p_facet_lai, width = 16, height = 10, bg = "white")
write_csv(lai_all_sites, "lai_time_series_all_sites.csv")





# --- plotting LAI by PFT ----





# --- Function to extract LAI time series by PFT from a NetCDF file ---
extract_lai_by_pft <- function(file_path) {
  ncd <- nc_open(file_path)
  on.exit(nc_close(ncd))
  
  lai_vals <- ncvar_get(ncd, "lai")
  dims <- dim(lai_vals)
  cat("File:", basename(file_path), "- dims:", paste(dims, collapse = " x "), "\n")
  
  if (length(dims) == 4) {
    # [time, pft, y, x], average over x and y
    lai_time_pft <- apply(lai_vals, c(1,2), mean, na.rm = TRUE)
  } else if (length(dims) == 3) {
    # [time, pft, tile] or similar
    lai_time_pft <- apply(lai_vals, c(1,2), mean, na.rm = TRUE)
  } else if (length(dims) == 2) {
    cat("Detected shape [pft, time], transposing.\n")
    lai_time_pft <- t(lai_vals)
  } else {
    stop("Unexpected shape for lai array.")
  }
  
  # Time handling
  time_vals <- ncvar_get(ncd, "time")
  time_units <- ncatt_get(ncd, "time", "units")$value
  ref_time <- as_datetime(substr(time_units, 15, 33), tz = "UTC")
  dates <- ref_time + seconds(time_vals)
  
  # Create tibble in long format: one row per time x pft
  lai_df <- as_tibble(lai_time_pft) %>%
    mutate(date = dates) %>%
    pivot_longer(-date, names_to = "pft", values_to = "lai") %>%
    mutate(pft = as.integer(str_remove(pft, "V")))  # clean up column names
  
  return(lai_df)
}

# --- Function to parse site name from filename ---
parse_site_name <- function(fname) {
  match <- str_match(fname, "^part6_([^-]+)-JULES")
  if (is.na(match[1,2])) return(NA_character_)
  return(match[1,2])
}



# --- Extract LAI by PFT for all sites ---
lai_pft_all_sites <- map_dfr(file_list, function(file) {
  site <- parse_site_name(basename(file))
  lai_df <- extract_lai_by_pft(file)
  lai_df$site <- site
  return(lai_df)
})

# --- Plot: Facet by site, colour by PFT ---
p_lai_pft <- ggplot(lai_pft_all_sites, aes(x = date, y = lai, color = as.factor(pft))) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~site, scales = "free", ncol = 3) +
  labs(title = "LAI by PFT and Site",
       x = "Date", y = "LAI",
       color = "PFT") +
  theme_minimal()

# --- Show plot ---
print(p_lai_pft)

# --- Save ---
ggsave("lai_by_pft_facet_by_site.png", p_lai_pft, width = 16, height = 10, bg = "white")
write_csv(lai_pft_all_sites, "lai_by_pft_all_sites.csv")





# --- Stacked area plot by site ----
p_lai_stack <- ggplot(lai_pft_all_sites, aes(x = date, y = lai, fill = as.factor(pft))) +
  geom_area(alpha = 0.8, linewidth = 0.1) +
  facet_wrap(~site, scales = "free", ncol = 4) +
  labs(title = "Stacked LAI by PFT and Site",
       x = "Date", y = "Total LAI",
       fill = "PFT") +
  theme_minimal()

# --- Show plot ---
print(p_lai_stack)

# --- Save to file ---
ggsave("lai_stacked_by_pft_facet_by_site.png", p_lai_stack, width = 16, height = 10)



# --- lai_gb ----

extract_lai_gb <- function(file_path) {
  ncd <- nc_open(file_path)
  on.exit(nc_close(ncd))
  
  lai_gb_vals <- ncvar_get(ncd, "lai_gb")
  dims <- dim(lai_gb_vals)
  cat("File:", basename(file_path), "- lai_gb dims:", paste(dims, collapse = " x "), "\n")
  
  if (length(dims) == 3) {
    lai_gb <- lai_gb_vals[,1,1]
  } else if (length(dims) == 1) {
    lai_gb <- lai_gb_vals
  } else {
    stop("Unexpected shape for lai_gb array.")
  }
  
  time_vals <- ncvar_get(ncd, "time")
  time_units <- ncatt_get(ncd, "time", "units")$value
  ref_time <- as_datetime(substr(time_units, 15, 33), tz = "UTC")
  dates <- ref_time + seconds(time_vals)
  
  tibble(date = dates, lai_gb = lai_gb)
}



lai_gb_all_sites <- map_dfr(file_list, function(file) {
  site <- parse_site_name(basename(file))
  fsmc_df <- extract_lai_gb(file)
  fsmc_df$site <- site
  return(fsmc_df)
})


lai_gb_all_sites <- lai_gb_all_sites %>%
  mutate(site = dplyr::recode(site,
                              "BW_GUM"      = "BW_GUM - Wetland",
                              "BW_NXR"      = "BW_NXR - Wetland",
                              "CG_TCH"      = "CG_TCH - Grassland",
                              "GH_ANK"      = "GH_ANK - Forest",
                              "ML_AGG"      = "ML_AGG - Grassland",
                              "NE_WAF"      = "NE_WAF - Savanna",
                              "NE_WAM"      = "NE_WAM - Savanna",
                              "SD_DEM"      = "SD_DEM - Grassland",
                              "SN_DHR"      = "SN_DHR - Grassland",
                              "SN_NKR"      = "SN_NKR - Cropland",
                              "SN_RAG"      = "SN_RAG - Cropland",
                              "UG_JIN"      = "UG_JIN - Wetland",
                              "ZA_CATH"     = "ZA_CATH - Grassland",
                              "ZA_KRU"      = "ZA_KRU - Savanna",
                              "ZA_WGN"      = "ZA_WGN - Grassland",
                              "ZM_MON"      = "ZM_MON - Forest"
  ))



p_facet_lai_gb <- ggplot(lai_gb_all_sites, aes(x = date, y = lai_gb)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  facet_wrap(~site, scales = "free", ncol = 3) +
  #scale_y_continuous(limits = c(0, NA)) +   # << Force y-axis to start at 0
  #scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  labs(#title = "lai_gb Time Series by Site",
    x = "Date", y = "lai_gb") +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 16, face = "bold"))


print(p_facet_lai_gb)


ggsave("new_lai_gb_time_series_faceted.png", p_facet_lai_gb, width = 18, height = 16, bg = "white")
#write_csv(lai_gb_all_sites, "lai_gb_time_series_all_sites.csv")






# --- Function to parse site name from file name ----
parse_site_name <- function(fname) {
  match <- str_match(fname, "^part6_([^-]+)-JULES")
  if (is.na(match[1,2])) return(NA_character_)
  return(match[1,2])
}

# --- Function to extract temperature and precipitation from JULES outputs ---
extract_temp_precip <- function(file_path) {
  ncd <- nc_open(file_path)
  on.exit(nc_close(ncd))
  
  temp_K <- ncvar_get(ncd, "min_t1p5m_gb")
  precip <- ncvar_get(ncd, "precip")
  
  # Time
  time_vals <- ncvar_get(ncd, "time")
  time_units <- ncatt_get(ncd, "time", "units")$value
  ref_time <- as_datetime(substr(time_units, 15, 33), tz = "UTC")
  dates <- ref_time + seconds(time_vals)
  
  tibble(
    date = dates,
    temp_C = temp_K - 273.15,
    precip = precip  # keep in kg m-2 s-1
  )
}



# --- Extract data ---
climate_all_sites <- map_dfr(file_list, function(file) {
  site <- parse_site_name(basename(file))
  df <- extract_temp_precip(file)
  df$site <- site
  return(df)
})




climate_all_sites <- climate_all_sites %>%
  mutate(site = dplyr::recode(site,
                              "BW_GUM"      = "BW_GUM - Wetland",
                              "BW_NXR"      = "BW_NXR - Wetland",
                              "CG_TCH"      = "CG_TCH - Grassland",
                              "GH_ANK"      = "GH_ANK - Forest",
                              "ML_AGG"      = "ML_AGG - Grassland",
                              "NE_WAF"      = "NE_WAF - Savanna",
                              "NE_WAM"      = "NE_WAM - Savanna",
                              "SD_DEM"      = "SD_DEM - Grassland",
                              "SN_DHR"      = "SN_DHR - Grassland",
                              "SN_NKR"      = "SN_NKR - Cropland",
                              "SN_RAG"      = "SN_RAG - Cropland",
                              "UG_JIN"      = "UG_JIN - Wetland",
                              "ZA_CATH"     = "ZA_CATH - Grassland",
                              "ZA_KRU"      = "ZA_KRU - Savanna",
                              "ZA_WGN"      = "ZA_WGN - Grassland",
                              "ZM_MON"      = "ZM_MON - Forest"
  ))





# --- Plot temperature by site ---
p_temp <- ggplot(climate_all_sites, aes(x = date, y = temp_C)) +
  geom_line(color = "blue", linewidth = 0.8) +
  facet_wrap(~site, scales = "free", ncol = 3) +
  labs(#title = "JULES Temperature Time Series by Site",
    y = "Temperature (°C)", x = "Date") +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 16, face = "bold"))



# --- Plot precipitation by site ---
p_precip <- ggplot(climate_all_sites, aes(x = date, y = precip)) +
  geom_line(color = "blue", linewidth = 0.8) +
  facet_wrap(~site, scales = "free", ncol = 3) +
  labs(#title = "JULES Precipitation Time Series by Site",
    y = "Precipitation (kg m⁻² s⁻¹)", x = "Date") +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 16, face = "bold"))




print(p_temp)
print(p_precip)


# --- Save and print ---
ggsave("new_temp_time_series_faceted.png", p_temp, width = 20, height = 16, bg = "white")
ggsave("new_precip_time_series_faceted.png", p_precip, width = 18, height = 18, bg = "white")



# --- Write data to CSV ---
write_csv(climate_all_sites, "climate_time_series_all_sites.csv")
