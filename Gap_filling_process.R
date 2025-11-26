# Post-hoc data processing
# this includes;
# De-spike flux using thresholds ±50
# calculate mean predictors
# assign NA values in observation with mean values of prediction
# make plot to see trends
# Make plots to observed, gap filled, and uncertainty (just like Songyan's plot) for each site

# load library ----
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(tidyverse)


# run code ----


## for all sites for CO2----

# Define the directory containing the CSV files
input_dir <- "C:/Users/efa206/OneDrive - University of Exeter/Desktop/output_CO2/"
output_dir <- "C:/Users/efa206/OneDrive - University of Exeter/Desktop/processed_output/"

# List of site file names (excluding extensions)
site_files <- c("BW_GUM_gapfilling", "BW_NXR_gapfilling", "CG_TCH_gapfilling",
                "GH_ANK_gapfilling", "ML_AGG_gapfilling", "NE_WAF_gapfilling",
                "NE_WAM_gapfilling", "SD_DEM_gapfilling", "SN_DHR_gapfilling",
                "SN_NKR_gapfilling", "SN_RAG_gapfilling", "UG_JIN_gapfilling",
                "ZA_CATH2014_gapfilling", "ZA_CATH2016_gapfilling", "ZA_KRU_gapfilling", 
                "ZA_WGN_gapfilling", "ZM_MON_gapfilling")

# Loop through each site and process the dataset
for (site in site_files) {
  
  # Construct file paths
  file_path <- paste0(input_dir, site, ".csv")
  output_path <- paste0(output_dir, site, "_processed.csv")
  
  # Read the dataset
  df <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Rename second column for consistency (ensure correct name if different)
  colnames(df)[2] <- "co2_flux"
  
  # Identify prediction columns
  pred_columns <- grep("^pred_", names(df), value = TRUE)
  
  # Compute ensemble mean
  df$ensemble_mean <- rowMeans(df[, pred_columns], na.rm = TRUE)
  
  # Remove extreme values in `co2_flux`
  df <- df %>%
    mutate(co2_flux = ifelse(co2_flux > 50 | co2_flux < -50, NA, co2_flux))
  
  # Replace missing `co2_flux` with `ensemble_mean`
  df$co2_flux[is.na(df$co2_flux)] <- df$ensemble_mean[is.na(df$co2_flux)]
  
  # Save processed file
  write.csv(df, output_path, row.names = FALSE)
  
  print(paste("Processed:", site))
}

print("All sites processed successfully!")





### make plots ----


# Read a processed dataset
df <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/processed_output/ZA_CATH2014_gapfilling_processed.csv")

# Convert DateTime to proper format
df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Plot CO2 flux over time
ggplot(df, aes(x = DateTime)) +
  geom_line(aes(y = co2_flux, color = "Observed")) +
  geom_line(aes(y = ensemble_mean, color = "Ensemble Mean"), linetype = "dashed") +
  labs(title = "CO2 Flux Time Series",
       y = "CO2 Flux (µmol m⁻² s⁻¹)",
       x = "Time") +
  scale_color_manual(values = c("Observed" = "blue", "Ensemble Mean" = "red")) +
  theme_minimal()








### including uncertainty ----

# Identify the prediction columns
pred_columns <- grep("^pred_", names(df), value = TRUE)

# Compute ensemble mean
df$ensemble_mean <- rowMeans(df[, pred_columns], na.rm = TRUE)

# Compute uncertainty as standard deviation
df$uncertainty <- apply(df[, pred_columns], 1, sd, na.rm = TRUE)

# Plot with uncertainty ribbon
ggplot(df, aes(x = DateTime)) +
  geom_ribbon(aes(ymin = ensemble_mean - uncertainty, ymax = ensemble_mean + uncertainty), 
              fill = "green", alpha = 1) +
  geom_line(aes(y = co2_flux, color = "Observed")) +
  geom_line(aes(y = ensemble_mean, color = "Ensemble Mean"), linetype = "dashed") +
  labs(title = "CO2 Flux Time Series",
       y = "CO2 Flux (µmol m⁻² s⁻¹)",
       x = "Time") +
  scale_color_manual(values = c("Observed" = "blue", "Ensemble Mean" = "red")) +
  theme_minimal()



# OR 

# Identify the prediction columns
pred_columns <- grep("^pred_", names(df), value = TRUE)

# Compute ensemble mean
df$ensemble_mean <- rowMeans(df[, pred_columns], na.rm = TRUE)

# Compute ymin and ymax using min & max of predictions
df$ymin <- apply(df[, pred_columns], 1, min, na.rm = TRUE)
df$ymax <- apply(df[, pred_columns], 1, max, na.rm = TRUE)

# Plot with uncertainty ribbon
ggplot(df, aes(x = DateTime)) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), fill = "green", alpha = 1) +
  geom_line(aes(y = co2_flux, color = "Observed")) +
  geom_line(aes(y = ensemble_mean, color = "Ensemble Mean"), linetype = "dashed") +
  labs(title = "CO2 Flux Time Series (BW_GUM)",
       y = "CO2 Flux (µmol m⁻² s⁻¹)",
       x = "Time") +
  scale_color_manual(values = c("Observed" = "blue", "Ensemble Mean" = "red")) +
  theme_minimal()




## for all sites for H----

# Define the directory containing the CSV files
input_dir <- "C:/Users/efa206/OneDrive - University of Exeter/Desktop/output_H/"
output_dir <- "C:/Users/efa206/OneDrive - University of Exeter/Desktop/processed_H/"

# List of site file names (excluding extensions)
site_files <- c("BJ_BEF_gapfilling", "BJ_NAF_gapfilling", "BW_GUM_gapfilling", 
                "BW_NXR_gapfilling", "CG_TCH_gapfilling", "GH_ANK_gapfilling", 
                "ML_AGG_gapfilling", "NE_WAF_gapfilling", "NE_WAM_gapfilling", 
                "SD_DEM_gapfilling", "SN_DHR_gapfilling", "SN_NKR_gapfilling", 
                "SN_RAG_gapfilling", "UG_JIN_gapfilling", "ZA_CATH2014_gapfilling", 
                "ZA_CATH2016_gapfilling", "ZA_KRU_gapfilling", "ZA_WGN_gapfilling", 
                "ZM_MON_gapfilling")

# Loop through each site and process the dataset
for (site in site_files) {
  
  # Construct file paths
  file_path <- paste0(input_dir, site, ".csv")
  output_path <- paste0(output_dir, site, "_processed_H.csv")
  
  # Read the dataset
  df <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Rename second column for consistency (ensure correct name if different)
  colnames(df)[2] <- "H"
  
  # Identify prediction columns
  pred_columns <- grep("^pred_", names(df), value = TRUE)
  
  # Compute ensemble mean
  df$ensemble_mean <- rowMeans(df[, pred_columns], na.rm = TRUE)
  
  
  # Remove extreme values in `H`
  #df <- df %>%
    #mutate(H = ifelse(H > 900 | H < -200, NA, H))
  
  # Replace missing `H` with `ensemble_mean`
  df$H[is.na(df$H)] <- df$ensemble_mean[is.na(df$H)]
  
  
  #df <- df %>%
    #mutate(H = ifelse(H > 900 | H < -100, 0, H))
  
  
  # Save processed file
  write.csv(df, output_path, row.names = FALSE)
  
  print(paste("Processed:", site))
}

print("All sites processed successfully!")




## for all sites for LE----

# Define the directory containing the CSV files
input_dir <- "C:/Users/efa206/OneDrive - University of Exeter/Desktop/output_LE/"
output_dir <- "C:/Users/efa206/OneDrive - University of Exeter/Desktop/processed_LE/"

# List of site file names (excluding extensions)
site_files <- c("BJ_BEF_gapfilling", "BJ_NAF_gapfilling", "BW_GUM_gapfilling", 
                "BW_NXR_gapfilling", "CG_TCH_gapfilling", "GH_ANK_gapfilling", 
                "ML_AGG_gapfilling", "NE_WAF_gapfilling", "NE_WAM_gapfilling", 
                "SD_DEM_gapfilling", "SN_DHR_gapfilling", "SN_NKR_gapfilling", 
                "SN_RAG_gapfilling", "UG_JIN_gapfilling", "ZA_CATH2014_gapfilling", 
                "ZA_CATH2016_gapfilling", "ZA_KRU_gapfilling", "ZA_WGN_gapfilling", 
                "ZM_MON_gapfilling")

# Loop through each site and process the dataset
for (site in site_files) {
  
  # Construct file paths
  file_path <- paste0(input_dir, site, ".csv")
  output_path <- paste0(output_dir, site, "_processed_LE.csv")
  
  # Read the dataset
  df <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Rename second column for consistency (ensure correct name if different)
  colnames(df)[2] <- "LE"
  
  # Identify prediction columns
  pred_columns <- grep("^pred_", names(df), value = TRUE)
  
  # Compute ensemble mean
  df$ensemble_mean <- rowMeans(df[, pred_columns], na.rm = TRUE)
  
  
  # Remove extreme values in `LE`
  #df <- df %>%
    #mutate(LE = ifelse(LE > 1000 | LE < -300, NA, LE))
  
  
  # Replace missing `LE` with `ensemble_mean`
  df$LE[is.na(df$LE)] <- df$ensemble_mean[is.na(df$LE)]
  
  
 # df <- df %>%
   # mutate(LE = ifelse(LE > 900 | LE < -100, 0, LE))
  
  
  # Save processed file
  write.csv(df, output_path, row.names = FALSE)
  
  print(paste("Processed:", site))
}

print("All sites processed successfully!")




## Combine all dataset for all sites to include all variables in a single file ----
# note that flux partitioning must happen before this step


# Base path
base_path <- "C:/Users/efa206/OneDrive - University of Exeter/Desktop/"

# Directories
dirs <- list(
  flux = file.path(base_path, "partitioned_flux_output"),
  H = file.path(base_path, "processed_H"),
  LE = file.path(base_path, "processed_LE"),
  partition = file.path(base_path, "for_partitioning")
)

# List of sites
sites <- c("BW_GUM", "BW_NXR", "CG_TCH", "GH_ANK", "ML_AGG", "NE_WAF",
           "NE_WAM", "SD_DEM", "SN_DHR", "SN_NKR", "SN_RAG", "UG_JIN",
           "ZA_CATH2014", "ZA_CATH2016", "ZA_KRU", "ZA_WGN", "ZM_MON")

# Output folder
output_dir <- file.path(base_path, "combined_data")
if (!dir.exists(output_dir)) dir.create(output_dir)

# ✅ Revised timestamp standardizer to fix ZA_KRU and support all sites
standardize_datetime <- function(df, possible_names = c("datetime", "DateTime")) {
  name <- intersect(possible_names, names(df))[1]
  df <- df %>% rename(TIMESTAMP_RAW = !!sym(name))
  
  df <- df %>%
    mutate(TIMESTAMP = case_when(
      str_detect(TIMESTAMP_RAW, "/") ~ suppressWarnings(dmy_hm(TIMESTAMP_RAW)),
      TRUE ~ suppressWarnings(ymd_hms(TIMESTAMP_RAW))
    )) %>%
    select(-TIMESTAMP_RAW)
  
  return(df)
}

# Loop through sites
for (site in sites) {
  message("Processing: ", site)
  
  # File paths
  flux_file <- file.path(dirs$flux, paste0(site, "_partitioned.csv"))
  H_file <- file.path(dirs$H, paste0(site, "_gapfilling_processed_H.csv"))
  LE_file <- file.path(dirs$LE, paste0(site, "_gapfilling_processed_LE.csv"))
  part_file <- file.path(dirs$partition, paste0(site, "_selected.csv"))
  
  if (!all(file.exists(flux_file, H_file, LE_file, part_file))) {
    warning("Missing one or more files for site: ", site)
    next
  }
  
  df_flux <- read_csv(flux_file) %>%
    standardize_datetime() %>%
    select(TIMESTAMP, NEE, GPP, Reco)
  
  df_H <- read_csv(H_file) %>%
    standardize_datetime() %>%
    select(TIMESTAMP, H)
  
  df_LE <- read_csv(LE_file) %>%
    standardize_datetime() %>%
    select(TIMESTAMP, LE)
  
  df_part <- read_csv(part_file) %>%
    standardize_datetime() %>%
    select(TIMESTAMP, TA, PAR, VPD)
  
  # Full join to keep full record (unchanged)
  df_combined <- df_flux %>%
    full_join(df_H, by = "TIMESTAMP") %>%
    full_join(df_LE, by = "TIMESTAMP") %>%
    full_join(df_part, by = "TIMESTAMP") %>%
    arrange(TIMESTAMP)
  
  # Format TIMESTAMP to original readable form
  df_combined$TIMESTAMP <- format(df_combined$TIMESTAMP, "%d/%m/%Y %H:%M:%S")
  
  write_csv(df_combined, file.path(output_dir, paste0(site, "_All.csv")))
}

message("✅ All done — with fixed timestamp handling!")





## calculate ET using latent heat of vaporization and temperature  ----

# Path to combined files
input_dir <- "C:/Users/efa206/OneDrive - University of Exeter/Desktop/combined_data"
output_dir <- "C:/Users/efa206/OneDrive - University of Exeter/Desktop/combined_data_with_ET"

# Create output folder if it doesn't exist
if (!dir.exists(output_dir)) dir.create(output_dir)

# Site list
sites <- c("BW_GUM", "BW_NXR", "CG_TCH", "GH_ANK", "ML_AGG", "NE_WAF",
           "NE_WAM", "SD_DEM", "SN_DHR", "SN_NKR", "SN_RAG", "UG_JIN",
           "ZA_CATH2014", "ZA_CATH2016", "ZA_KRU", "ZA_WGN", "ZM_MON")

# Loop through each site
for (site in sites) {
  message("Calculating ET for: ", site)
  
  # File path
  input_file <- file.path(input_dir, paste0(site, "_All.csv"))
  output_file <- file.path(output_dir, paste0(site, "_with_ET.csv"))
  
  # Check if file exists
  if (!file.exists(input_file)) {
    warning("File missing for site: ", site)
    next
  }
  
  # Read data
  df <- read_csv(input_file)
  
  # Convert TIMESTAMP back to POSIXct if needed
  df$TIMESTAMP <- dmy_hms(df$TIMESTAMP)
  
  # Calculate lambda and ET
  df <- df %>%
    mutate(
      lambda = (2.501 - 0.00237 * TA) * 1e6,  # J/kg
      ET_kg_m2_s = LE / lambda
    )
  
  # Save file with new ET column
  write_csv(df, output_file)
}

message("🌍✅ ET added for all sites!")
