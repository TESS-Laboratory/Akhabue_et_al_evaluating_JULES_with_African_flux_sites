# This is to prepare data fro flux partitioning that was done in python.
# The processed scripts were moved to continue flux partitioning in Python using Anaconda Jupyter.


# Load library ----
library(dplyr)


# Read the dataset ----
# BW_GUM ----


BW_NXR_processed <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/processed_output/BW_NXR_gapfilling_processed.csv", stringsAsFactors = FALSE)


BW_NXR_flux <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/Flux_data/BW_NXR_DATA.csv", stringsAsFactors = FALSE)


# Merge the two datasets by DateTime
BW_NXR_combined_data <- merge(BW_NXR_processed, BW_NXR_flux, by = "DateTime")

# Select only the columns of interest

BW_NXR_selected_data <- BW_NXR_combined_data[, c("DateTime", "co2_flux", "ERA5_Temp_C", "ERA5_SSR_W.M2", "VPD_hPa")]

colnames(BW_NXR_selected_data) <- c("datetime", "NEE", "TA", "PAR", "VPD")



write.csv(BW_NXR_selected_data, 
          file = "C:/Users/efa206/OneDrive - University of Exeter/Desktop/for_partitioning/BW_NXR_selected.csv", 
          row.names = FALSE)





# BW_NXR ----
BW_NXR_processed <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/processed_output/BW_NXR_gapfilling_processed.csv", stringsAsFactors = FALSE)


BW_NXR_flux <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/Flux_data/BW_NXR_DATA.csv", stringsAsFactors = FALSE)


# Merge the two datasets by DateTime
BW_NXR_combined_data <- merge(BW_NXR_processed, BW_NXR_flux, by = "DateTime")

# Select only the columns of interest

BW_NXR_selected_data <- BW_NXR_combined_data[, c("DateTime", "co2_flux", "ERA5_Temp_C", "ERA5_SSR_W.M2", "VPD_hPa")]

colnames(BW_NXR_selected_data) <- c("datetime", "NEE", "TA", "PAR", "VPD")



write.csv(BW_NXR_selected_data, 
          file = "C:/Users/efa206/OneDrive - University of Exeter/Desktop/for_partitioning/BW_NXR_selected.csv", 
          row.names = FALSE)lux <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/Flux_data/BW_NXR_DATA.csv", stringsAsFactors = FALSE)



# CG_TCH ----

CG_TCH_processed <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/processed_output/CG_TCH_gapfilling_processed.csv", stringsAsFactors = FALSE)


CG_TCH_flux <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/Flux_data/CG_TCH_DATA.csv", stringsAsFactors = FALSE)


# Merge the two datasets by DateTime
CG_TCH_combined_data <- merge(CG_TCH_processed, CG_TCH_flux, by = "DateTime")

# Select only the columns of interest

CG_TCH_selected_data <- CG_TCH_combined_data[, c("DateTime", "co2_flux", "ERA5_Temp_C", "ERA5_SSR_W.M2", "VPD_hPa")]

colnames(CG_TCH_selected_data) <- c("datetime", "NEE", "TA", "PAR", "VPD")



write.csv(CG_TCH_selected_data, 
          file = "C:/Users/efa206/OneDrive - University of Exeter/Desktop/for_partitioning/CG_TCH_selected.csv", 
          row.names = FALSE)




# GH_ANK ----

GH_ANK_processed <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/processed_output/GH_ANK_gapfilling_processed.csv", stringsAsFactors = FALSE)


GH_ANK_flux <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/Flux_data/GH_ANK_DATA.csv", stringsAsFactors = FALSE)


# Merge the two datasets by DateTime
GH_ANK_combined_data <- merge(GH_ANK_processed, GH_ANK_flux, by = "DateTime")

# Select only the columns of interest

GH_ANK_selected_data <- GH_ANK_combined_data[, c("DateTime", "co2_flux", "ERA5_Temp_C", "ERA5_SSR_W.M2", "VPD_hPa")]

colnames(GH_ANK_selected_data) <- c("datetime", "NEE", "TA", "PAR", "VPD")



write.csv(GH_ANK_selected_data, 
          file = "C:/Users/efa206/OneDrive - University of Exeter/Desktop/for_partitioning/GH_ANK_selected.csv", 
          row.names = FALSE)





# ML_AGG ----

ML_AGG_processed <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/processed_output/ML_AGG_gapfilling_processed.csv", stringsAsFactors = FALSE)


ML_AGG_flux <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/Flux_data/ML_AGG_DATA.csv", stringsAsFactors = FALSE)


# Merge the two datasets by DateTime
ML_AGG_combined_data <- merge(ML_AGG_processed, ML_AGG_flux, by = "DateTime")

# Select only the columns of interest

ML_AGG_selected_data <- ML_AGG_combined_data[, c("DateTime", "co2_flux", "ERA5_Temp_C", "ERA5_SSR_W.M2", "VPD_hPa")]

colnames(ML_AGG_selected_data) <- c("datetime", "NEE", "TA", "PAR", "VPD")



write.csv(ML_AGG_selected_data, 
          file = "C:/Users/efa206/OneDrive - University of Exeter/Desktop/for_partitioning/ML_AGG_selected.csv", 
          row.names = FALSE)




# NW_WAF ----

NE_WAF_processed <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/processed_output/NE_WAF_gapfilling_processed.csv", stringsAsFactors = FALSE)


NE_WAF_flux <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/Flux_data/NE_WAF_DATA.csv", stringsAsFactors = FALSE)


# Merge the two datasets by DateTime
NE_WAF_combined_data <- merge(NE_WAF_processed, NE_WAF_flux, by = "DateTime")

# Select only the columns of interest

NE_WAF_selected_data <- NE_WAF_combined_data[, c("DateTime", "co2_flux", "ERA5_Temp_C", "ERA5_SSR_W.M2", "VPD_hPa")]

colnames(NE_WAF_selected_data) <- c("datetime", "NEE", "TA", "PAR", "VPD")



write.csv(NE_WAF_selected_data, 
          file = "C:/Users/efa206/OneDrive - University of Exeter/Desktop/for_partitioning/NE_WAF_selected.csv", 
          row.names = FALSE)




# NE_WAM ----

NE_WAM_processed <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/processed_output/NE_WAM_gapfilling_processed.csv", stringsAsFactors = FALSE)


NE_WAM_flux <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/Flux_data/NE_WAM_DATA.csv", stringsAsFactors = FALSE)


# Merge the two datasets by DateTime
NE_WAM_combined_data <- merge(NE_WAM_processed, NE_WAM_flux, by = "DateTime")

# Select only the columns of interest

NE_WAM_selected_data <- NE_WAM_combined_data[, c("DateTime", "co2_flux", "ERA5_Temp_C", "ERA5_SSR_W.M2", "VPD_hPa")]

colnames(NE_WAM_selected_data) <- c("datetime", "NEE", "TA", "PAR", "VPD")



write.csv(NE_WAM_selected_data, 
          file = "C:/Users/efa206/OneDrive - University of Exeter/Desktop/for_partitioning/NE_WAM_selected.csv", 
          row.names = FALSE)





# SD_DEM ----

SD_DEM_processed <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/processed_output/SD_DEM_gapfilling_processed.csv", stringsAsFactors = FALSE)


SD_DEM_flux <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/Flux_data/SD_DEM_DATA.csv", stringsAsFactors = FALSE)


# Merge the two datasets by DateTime
SD_DEM_combined_data <- merge(SD_DEM_processed, SD_DEM_flux, by = "DateTime")

# Select only the columns of interest

SD_DEM_selected_data <- SD_DEM_combined_data[, c("DateTime", "co2_flux", "ERA5_Temp_C", "ERA5_SSR_W.M2", "VPD_hPa")]

colnames(SD_DEM_selected_data) <- c("datetime", "NEE", "TA", "PAR", "VPD")



write.csv(SD_DEM_selected_data, 
          file = "C:/Users/efa206/OneDrive - University of Exeter/Desktop/for_partitioning/SD_DEM_selected.csv", 
          row.names = FALSE)







# SN_DHR ----

SN_DHR_processed <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/processed_output/SN_DHR_gapfilling_processed.csv", stringsAsFactors = FALSE)


SN_DHR_flux <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/Flux_data/SN_DHR_DATA.csv", stringsAsFactors = FALSE)


# Merge the two datasets by DateTime
SN_DHR_combined_data <- merge(SN_DHR_processed, SN_DHR_flux, by = "DateTime")

# Select only the columns of interest

SN_DHR_selected_data <- SN_DHR_combined_data[, c("DateTime", "co2_flux", "ERA5_Temp_C", "ERA5_SSR_W.M2", "VPD_hPa")]

colnames(SN_DHR_selected_data) <- c("datetime", "NEE", "TA", "PAR", "VPD")



write.csv(SN_DHR_selected_data, 
          file = "C:/Users/efa206/OneDrive - University of Exeter/Desktop/for_partitioning/SN_DHR_selected.csv", 
          row.names = FALSE)





# SN_NKR ----

SN_NKR_processed <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/processed_output/SN_NKR_gapfilling_processed.csv", stringsAsFactors = FALSE)


SN_NKR_flux <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/Flux_data/SN_NKR_DATA.csv", stringsAsFactors = FALSE)


# Merge the two datasets by DateTime
SN_NKR_combined_data <- merge(SN_NKR_processed, SN_NKR_flux, by = "DateTime")

# Select only the columns of interest

SN_NKR_selected_data <- SN_NKR_combined_data[, c("DateTime", "co2_flux", "ERA5_Temp_C", "ERA5_SSR_W.M2", "VPD_hPa")]

colnames(SN_NKR_selected_data) <- c("datetime", "NEE", "TA", "PAR", "VPD")



write.csv(SN_NKR_selected_data, 
          file = "C:/Users/efa206/OneDrive - University of Exeter/Desktop/for_partitioning/SN_NKR_selected.csv", 
          row.names = FALSE)





# SN_RAG ----

SN_RAG_processed <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/processed_output/SN_RAG_gapfilling_processed.csv", stringsAsFactors = FALSE)


SN_RAG_flux <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/Flux_data/SN_RAG_DATA.csv", stringsAsFactors = FALSE)


# Merge the two datasets by DateTime
SN_RAG_combined_data <- merge(SN_RAG_processed, SN_RAG_flux, by = "DateTime")

# Select only the columns of interest

SN_RAG_selected_data <- SN_RAG_combined_data[, c("DateTime", "co2_flux", "ERA5_Temp_C", "ERA5_SSR_W.M2", "VPD_hPa")]

colnames(SN_RAG_selected_data) <- c("datetime", "NEE", "TA", "PAR", "VPD")



write.csv(SN_RAG_selected_data, 
          file = "C:/Users/efa206/OneDrive - University of Exeter/Desktop/for_partitioning/SN_RAG_selected.csv", 
          row.names = FALSE)





# UG_JIN ----

UG_JIN_processed <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/processed_output/UG_JIN_gapfilling_processed.csv", stringsAsFactors = FALSE)


UG_JIN_flux <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/Flux_data/UG_JIN_DATA.csv", stringsAsFactors = FALSE)


# Merge the two datasets by DateTime
UG_JIN_combined_data <- merge(UG_JIN_processed, UG_JIN_flux, by = "DateTime")

# Select only the columns of interest

UG_JIN_selected_data <- UG_JIN_combined_data[, c("DateTime", "co2_flux", "ERA5_Temp_C", "ERA5_SSR_W.M2", "VPD_hPa")]

colnames(UG_JIN_selected_data) <- c("datetime", "NEE", "TA", "PAR", "VPD")



write.csv(UG_JIN_selected_data, 
          file = "C:/Users/efa206/OneDrive - University of Exeter/Desktop/for_partitioning/UG_JIN_selected.csv", 
          row.names = FALSE)





# ZA_CATH2014 ----

ZA_CATH2014_processed <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/processed_output/ZA_CATH2014_gapfilling_processed.csv", stringsAsFactors = FALSE)


ZA_CATH2014_flux <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/Flux_data/ZA_CATH_DATA_2014.csv", stringsAsFactors = FALSE)


# Merge the two datasets by DateTime
ZA_CATH2014_combined_data <- merge(ZA_CATH2014_processed, ZA_CATH2014_flux, by = "DateTime")

# Select only the columns of interest

ZA_CATH2014_selected_data <- ZA_CATH2014_combined_data[, c("DateTime", "co2_flux", "ERA5_Temp_C", "ERA5_SSR_W.M2", "VPD_hPa")]

colnames(ZA_CATH2014_selected_data) <- c("datetime", "NEE", "TA", "PAR", "VPD")



write.csv(ZA_CATH2014_selected_data, 
          file = "C:/Users/efa206/OneDrive - University of Exeter/Desktop/for_partitioning/ZA_CATH2014_selected.csv", 
          row.names = FALSE)





# ZA_CATH2016 ----

ZA_CATH2016_processed <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/processed_output/ZA_CATH2016_gapfilling_processed.csv", stringsAsFactors = FALSE)


ZA_CATH2016_flux <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/Flux_data/ZA_CATH_DATA_2016.csv", stringsAsFactors = FALSE)


# Merge the two datasets by DateTime
ZA_CATH2016_combined_data <- merge(ZA_CATH2016_processed, ZA_CATH2016_flux, by = "DateTime")

# Select only the columns of interest

ZA_CATH2016_selected_data <- ZA_CATH2016_combined_data[, c("DateTime", "co2_flux", "ERA5_Temp_C", "ERA5_SSR_W.M2", "VPD_hPa")]

colnames(ZA_CATH2016_selected_data) <- c("datetime", "NEE", "TA", "PAR", "VPD")



write.csv(ZA_CATH2016_selected_data, 
          file = "C:/Users/efa206/OneDrive - University of Exeter/Desktop/for_partitioning/ZA_CATH2016_selected.csv", 
          row.names = FALSE)





# ZA_KRU ----

ZA_KRU_processed <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/processed_output/ZA_KRU_gapfilling_processed.csv", stringsAsFactors = FALSE)


ZA_KRU_flux <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/Flux_data/ZA_KRU_DATA.csv", stringsAsFactors = FALSE)


# Merge the two datasets by DateTime
ZA_KRU_combined_data <- merge(ZA_KRU_processed, ZA_KRU_flux, by = "DateTime")

# Select only the columns of interest

ZA_KRU_selected_data <- ZA_KRU_combined_data[, c("DateTime", "co2_flux", "ERA5_Temp_C", "ERA5_SSR_W.M2", "VPD_hPa")]

colnames(ZA_KRU_selected_data) <- c("datetime", "NEE", "TA", "PAR", "VPD")



write.csv(ZA_KRU_selected_data, 
          file = "C:/Users/efa206/OneDrive - University of Exeter/Desktop/for_partitioning/ZA_KRU_selected.csv", 
          row.names = FALSE)




# ZA_WGN ----

ZA_WGN_processed <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/processed_output/ZA_WGN_gapfilling_processed.csv", stringsAsFactors = FALSE)


ZA_WGN_flux <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/Flux_data/ZA_WGN_DATA.csv", stringsAsFactors = FALSE)


# Merge the two datasets by DateTime
ZA_WGN_combined_data <- merge(ZA_WGN_processed, ZA_WGN_flux, by = "DateTime")

# Select only the columns of interest

ZA_WGN_selected_data <- ZA_WGN_combined_data[, c("DateTime", "co2_flux", "ERA5_Temp_C", "ERA5_SSR_W.M2", "VPD_hPa")]

colnames(ZA_WGN_selected_data) <- c("datetime", "NEE", "TA", "PAR", "VPD")



write.csv(ZA_WGN_selected_data, 
          file = "C:/Users/efa206/OneDrive - University of Exeter/Desktop/for_partitioning/ZA_WGN_selected.csv", 
          row.names = FALSE)




# ZM_MON ----

ZM_MON_processed <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/processed_output/ZM_MON_gapfilling_processed.csv", stringsAsFactors = FALSE)


ZM_MON_flux <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/Flux_data/ZM_MON_DATA.csv", stringsAsFactors = FALSE)


# Merge the two datasets by DateTime
ZM_MON_combined_data <- merge(ZM_MON_processed, ZM_MON_flux, by = "DateTime")

# Select only the columns of interest

ZM_MON_selected_data <- ZM_MON_combined_data[, c("DateTime", "co2_flux", "ERA5_Temp_C", "ERA5_SSR_W.M2", "VPD_hPa")]

colnames(ZM_MON_selected_data) <- c("datetime", "NEE", "TA", "PAR", "VPD")



write.csv(ZM_MON_selected_data, 
          file = "C:/Users/efa206/OneDrive - University of Exeter/Desktop/for_partitioning/ZM_MON_selected.csv", 
          row.names = FALSE)
