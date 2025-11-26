# load library ----
library(tidyverse)
library(tidyr)
library(magrittr)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)
library(ggplot2)
library(patchwork)

# read data of PET ----
PET_1991_2020 <- read_csv("Mean_Annual_PET_1991_2020.csv")

# for BW_GUM ----

BW_GUM_Temp <- read_csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/clim_data/BW_GUM_ERA5_export_TEMP.csv")
# Renaming columns in the BJ_BEF_Temp data frame
colnames(BW_GUM_Temp)[colnames(BW_GUM_Temp) == "system:index"] <- "DateTime"
colnames(BW_GUM_Temp)[colnames(BW_GUM_Temp) == "mean"] <- "ERA5_Temp_K"




BW_GUM_Precip <- read_csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/clim_data/BW_GUM_ERA5_export_PRECIP.csv")
# Renaming columns in the BJ_BEF_Temp data frame
colnames(BW_GUM_Precip)[colnames(BW_GUM_Precip) == "system:index"] <- "DateTime"
colnames(BW_GUM_Precip)[colnames(BW_GUM_Precip) == "mean"] <- "ERA5_Precip_m"




# List of datasets to combine
datasets <- list(BW_GUM_Temp, BW_GUM_Precip)

# Combine all datasets in the list by "DateTime"
BW_GUM_Combined <- Reduce(function(x, y) merge(x, y, by = "DateTime", all = TRUE), datasets)




# Select columns you want to keep
BW_Gum_clim <- BW_GUM_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
BW_Gum_clim$DateTime <- as.POSIXct(substr(BW_Gum_clim$DateTime, 1, 15), 
                                       format = "%Y%m%dT%H", tz = "UTC")



# for BW_NXR ----

BW_NXR_Temp <- read_csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/clim_data/BW_NXR_ERA5_export_TEMP.csv")
# Renaming columns in the BJ_BEF_Temp data frame
colnames(BW_NXR_Temp)[colnames(BW_NXR_Temp) == "system:index"] <- "DateTime"
colnames(BW_NXR_Temp)[colnames(BW_NXR_Temp) == "mean"] <- "ERA5_Temp_K"




BW_NXR_Precip <- read_csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/clim_data/BW_NXR_ERA5_export_PRECIP.csv")
# Renaming columns in the BJ_BEF_Temp data frame
colnames(BW_NXR_Precip)[colnames(BW_NXR_Precip) == "system:index"] <- "DateTime"
colnames(BW_NXR_Precip)[colnames(BW_NXR_Precip) == "mean"] <- "ERA5_Precip_m"




# List of datasets to combine
datasets <- list(BW_NXR_Temp, BW_NXR_Precip)

# Combine all datasets in the list by "DateTime"
BW_NXR_Combined <- Reduce(function(x, y) merge(x, y, by = "DateTime", all = TRUE), datasets)




# Select columns you want to keep
BW_Nxr_clim <- BW_NXR_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
BW_Nxr_clim$DateTime <- as.POSIXct(substr(BW_Nxr_clim$DateTime, 1, 15), 
                                   format = "%Y%m%dT%H", tz = "UTC")




# for CG_TCH ----

CG_TCH_Temp <- read_csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/clim_data/CG_TCH_ERA5_export_TEMP.csv")
# Renaming columns in the BJ_BEF_Temp data frame
colnames(CG_TCH_Temp)[colnames(CG_TCH_Temp) == "system:index"] <- "DateTime"
colnames(CG_TCH_Temp)[colnames(CG_TCH_Temp) == "mean"] <- "ERA5_Temp_K"




CG_TCH_Precip <- read_csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/clim_data/CG_TCH_ERA5_export_PRECIP.csv")
# Renaming columns in the BJ_BEF_Temp data frame
colnames(CG_TCH_Precip)[colnames(CG_TCH_Precip) == "system:index"] <- "DateTime"
colnames(CG_TCH_Precip)[colnames(CG_TCH_Precip) == "mean"] <- "ERA5_Precip_m"




# List of datasets to combine
datasets <- list(CG_TCH_Temp, CG_TCH_Precip)

# Combine all datasets in the list by "DateTime"
CG_TCH_Combined <- Reduce(function(x, y) merge(x, y, by = "DateTime", all = TRUE), datasets)




# Select columns you want to keep
CG_Tch_clim <- CG_TCH_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
CG_Tch_clim$DateTime <- as.POSIXct(substr(CG_Tch_clim$DateTime, 1, 15), 
                                   format = "%Y%m%dT%H", tz = "UTC")



# for GH_ANK ----

GH_ANK_Temp <- read_csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/clim_data/GH_ANK_ERA5_export_TEMP.csv")
# Renaming columns in the BJ_BEF_Temp data frame
colnames(GH_ANK_Temp)[colnames(GH_ANK_Temp) == "system:index"] <- "DateTime"
colnames(GH_ANK_Temp)[colnames(GH_ANK_Temp) == "mean"] <- "ERA5_Temp_K"




GH_ANK_Precip <- read_csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/clim_data/GH_ANK_ERA5_export_PRECIP.csv")
# Renaming columns in the BJ_BEF_Temp data frame
colnames(GH_ANK_Precip)[colnames(GH_ANK_Precip) == "system:index"] <- "DateTime"
colnames(GH_ANK_Precip)[colnames(GH_ANK_Precip) == "mean"] <- "ERA5_Precip_m"




# List of datasets to combine
datasets <- list(GH_ANK_Temp, GH_ANK_Precip)

# Combine all datasets in the list by "DateTime"
GH_ANK_Combined <- Reduce(function(x, y) merge(x, y, by = "DateTime", all = TRUE), datasets)




# Select columns you want to keep
GH_Ank_clim <- GH_ANK_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
GH_Ank_clim$DateTime <- as.POSIXct(substr(GH_Ank_clim$DateTime, 1, 15), 
                                   format = "%Y%m%dT%H", tz = "UTC")




# for ML_AGG ----

ML_AGG_Temp <- read_csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/clim_data/ML_AGG_ERA5_export_TEMP.csv")
# Renaming columns in the BJ_BEF_Temp data frame
colnames(ML_AGG_Temp)[colnames(ML_AGG_Temp) == "system:index"] <- "DateTime"
colnames(ML_AGG_Temp)[colnames(ML_AGG_Temp) == "mean"] <- "ERA5_Temp_K"




ML_AGG_Precip <- read_csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/clim_data/ML_AGG_ERA5_export_PRECIP.csv")
# Renaming columns in the BJ_BEF_Temp data frame
colnames(ML_AGG_Precip)[colnames(ML_AGG_Precip) == "system:index"] <- "DateTime"
colnames(ML_AGG_Precip)[colnames(ML_AGG_Precip) == "mean"] <- "ERA5_Precip_m"




# List of datasets to combine
datasets <- list(ML_AGG_Temp, ML_AGG_Precip)

# Combine all datasets in the list by "DateTime"
ML_AGG_Combined <- Reduce(function(x, y) merge(x, y, by = "DateTime", all = TRUE), datasets)




# Select columns you want to keep
ML_AgG_clim <- ML_AGG_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
ML_AgG_clim$DateTime <- as.POSIXct(substr(ML_AgG_clim$DateTime, 1, 15), 
                                   format = "%Y%m%dT%H", tz = "UTC")



# for NE_WAF ----

NE_WAF_Temp <- read_csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/clim_data/NE_WAF_ERA5_export_TEMP.csv")
# Renaming columns in the BJ_BEF_Temp data frame
colnames(NE_WAF_Temp)[colnames(NE_WAF_Temp) == "system:index"] <- "DateTime"
colnames(NE_WAF_Temp)[colnames(NE_WAF_Temp) == "mean"] <- "ERA5_Temp_K"




NE_WAF_Precip <- read_csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/clim_data/NE_WAF_ERA5_export_PRECIP.csv")
# Renaming columns in the BJ_BEF_Temp data frame
colnames(NE_WAF_Precip)[colnames(NE_WAF_Precip) == "system:index"] <- "DateTime"
colnames(NE_WAF_Precip)[colnames(NE_WAF_Precip) == "mean"] <- "ERA5_Precip_m"




# List of datasets to combine
datasets <- list(NE_WAF_Temp, NE_WAF_Precip)

# Combine all datasets in the list by "DateTime"
NE_WAF_Combined <- Reduce(function(x, y) merge(x, y, by = "DateTime", all = TRUE), datasets)




# Select columns you want to keep
NE_Waf_clim <- NE_WAF_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
NE_Waf_clim$DateTime <- as.POSIXct(substr(NE_Waf_clim$DateTime, 1, 15), 
                                   format = "%Y%m%dT%H", tz = "UTC")



# for NE_WAM ----

NE_WAM_Temp <- read_csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/clim_data/NE_WAM_ERA5_export_TEMP.csv")
# Renaming columns in the BJ_BEF_Temp data frame
colnames(NE_WAM_Temp)[colnames(NE_WAM_Temp) == "system:index"] <- "DateTime"
colnames(NE_WAM_Temp)[colnames(NE_WAM_Temp) == "mean"] <- "ERA5_Temp_K"




NE_WAM_Precip <- read_csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/clim_data/NE_WAM_ERA5_export_PRECIP.csv")
# Renaming columns in the BJ_BEF_Temp data frame
colnames(NE_WAM_Precip)[colnames(NE_WAM_Precip) == "system:index"] <- "DateTime"
colnames(NE_WAM_Precip)[colnames(NE_WAM_Precip) == "mean"] <- "ERA5_Precip_m"




# List of datasets to combine
datasets <- list(NE_WAM_Temp, NE_WAM_Precip)

# Combine all datasets in the list by "DateTime"
NE_WAM_Combined <- Reduce(function(x, y) merge(x, y, by = "DateTime", all = TRUE), datasets)




# Select columns you want to keep
NE_Wam_clim <- NE_WAM_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
NE_Wam_clim$DateTime <- as.POSIXct(substr(NE_Wam_clim$DateTime, 1, 15), 
                                   format = "%Y%m%dT%H", tz = "UTC")




# for SD_DEM ----

SD_DEM_Temp <- read_csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/clim_data/SD_DEM_ERA5_export_TEMP.csv")
# Renaming columns in the BJ_BEF_Temp data frame
colnames(SD_DEM_Temp)[colnames(SD_DEM_Temp) == "system:index"] <- "DateTime"
colnames(SD_DEM_Temp)[colnames(SD_DEM_Temp) == "mean"] <- "ERA5_Temp_K"




SD_DEM_Precip <- read_csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/clim_data/SD_DEM_ERA5_export_PRECIP.csv")
# Renaming columns in the BJ_BEF_Temp data frame
colnames(SD_DEM_Precip)[colnames(SD_DEM_Precip) == "system:index"] <- "DateTime"
colnames(SD_DEM_Precip)[colnames(SD_DEM_Precip) == "mean"] <- "ERA5_Precip_m"




# List of datasets to combine
datasets <- list(SD_DEM_Temp, SD_DEM_Precip)

# Combine all datasets in the list by "DateTime"
SD_DEM_Combined <- Reduce(function(x, y) merge(x, y, by = "DateTime", all = TRUE), datasets)




# Select columns you want to keep
SD_Dem_clim <- SD_DEM_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
SD_Dem_clim$DateTime <- as.POSIXct(substr(SD_Dem_clim$DateTime, 1, 15), 
                                   format = "%Y%m%dT%H", tz = "UTC")




# for SN_DHR ----

SN_DHR_Temp <- read_csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/clim_data/SN_DHR_ERA5_export_TEMP.csv")
# Renaming columns in the BJ_BEF_Temp data frame
colnames(SN_DHR_Temp)[colnames(SN_DHR_Temp) == "system:index"] <- "DateTime"
colnames(SN_DHR_Temp)[colnames(SN_DHR_Temp) == "mean"] <- "ERA5_Temp_K"




SN_DHR_Precip <- read_csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/clim_data/SN_DHR_ERA5_export_PRECIP.csv")
# Renaming columns in the BJ_BEF_Temp data frame
colnames(SN_DHR_Precip)[colnames(SN_DHR_Precip) == "system:index"] <- "DateTime"
colnames(SN_DHR_Precip)[colnames(SN_DHR_Precip) == "mean"] <- "ERA5_Precip_m"




# List of datasets to combine
datasets <- list(SN_DHR_Temp, SN_DHR_Precip)

# Combine all datasets in the list by "DateTime"
SN_DHR_Combined <- Reduce(function(x, y) merge(x, y, by = "DateTime", all = TRUE), datasets)




# Select columns you want to keep
SN_Dhr_clim <- SN_DHR_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
SN_Dhr_clim$DateTime <- as.POSIXct(substr(SN_Dhr_clim$DateTime, 1, 15), 
                                   format = "%Y%m%dT%H", tz = "UTC")



# for SN_NKR ----

SN_NKR_Temp <- read_csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/clim_data/SN_NKR_ERA5_export_TEMP.csv")
# Renaming columns in the BJ_BEF_Temp data frame
colnames(SN_NKR_Temp)[colnames(SN_NKR_Temp) == "system:index"] <- "DateTime"
colnames(SN_NKR_Temp)[colnames(SN_NKR_Temp) == "mean"] <- "ERA5_Temp_K"




SN_NKR_Precip <- read_csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/clim_data/SN_NKR_ERA5_export_PRECIP.csv")
# Renaming columns in the BJ_BEF_Temp data frame
colnames(SN_NKR_Precip)[colnames(SN_NKR_Precip) == "system:index"] <- "DateTime"
colnames(SN_NKR_Precip)[colnames(SN_NKR_Precip) == "mean"] <- "ERA5_Precip_m"




# List of datasets to combine
datasets <- list(SN_NKR_Temp, SN_NKR_Precip)

# Combine all datasets in the list by "DateTime"
SN_NKR_Combined <- Reduce(function(x, y) merge(x, y, by = "DateTime", all = TRUE), datasets)




# Select columns you want to keep
SN_Nkr_clim <- SN_NKR_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
SN_Nkr_clim$DateTime <- as.POSIXct(substr(SN_Nkr_clim$DateTime, 1, 15), 
                                   format = "%Y%m%dT%H", tz = "UTC")



# for SN_RAG ----

SN_RAG_Temp <- read_csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/clim_data/SN_RAG_ERA5_export_TEMP.csv")
# Renaming columns in the BJ_BEF_Temp data frame
colnames(SN_RAG_Temp)[colnames(SN_RAG_Temp) == "system:index"] <- "DateTime"
colnames(SN_RAG_Temp)[colnames(SN_RAG_Temp) == "mean"] <- "ERA5_Temp_K"




SN_RAG_Precip <- read_csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/clim_data/SN_RAG_ERA5_export_PRECIP.csv")
# Renaming columns in the BJ_BEF_Temp data frame
colnames(SN_RAG_Precip)[colnames(SN_RAG_Precip) == "system:index"] <- "DateTime"
colnames(SN_RAG_Precip)[colnames(SN_RAG_Precip) == "mean"] <- "ERA5_Precip_m"




# List of datasets to combine
datasets <- list(SN_RAG_Temp, SN_RAG_Precip)

# Combine all datasets in the list by "DateTime"
SN_RAG_Combined <- Reduce(function(x, y) merge(x, y, by = "DateTime", all = TRUE), datasets)




# Select columns you want to keep
SN_Rag_clim <- SN_RAG_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
SN_Rag_clim$DateTime <- as.POSIXct(substr(SN_Rag_clim$DateTime, 1, 15), 
                                   format = "%Y%m%dT%H", tz = "UTC")



# for UG_JIN ----

UG_JIN_Temp <- read_csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/clim_data/UG_JIN_ERA5_export_TEMP.csv")
# Renaming columns in the BJ_BEF_Temp data frame
colnames(UG_JIN_Temp)[colnames(UG_JIN_Temp) == "system:index"] <- "DateTime"
colnames(UG_JIN_Temp)[colnames(UG_JIN_Temp) == "mean"] <- "ERA5_Temp_K"




UG_JIN_Precip <- read_csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/clim_data/UG_JIN_ERA5_export_PRECIP.csv")
# Renaming columns in the BJ_BEF_Temp data frame
colnames(UG_JIN_Precip)[colnames(UG_JIN_Precip) == "system:index"] <- "DateTime"
colnames(UG_JIN_Precip)[colnames(UG_JIN_Precip) == "mean"] <- "ERA5_Precip_m"




# List of datasets to combine
datasets <- list(UG_JIN_Temp, UG_JIN_Precip)

# Combine all datasets in the list by "DateTime"
UG_JIN_Combined <- Reduce(function(x, y) merge(x, y, by = "DateTime", all = TRUE), datasets)




# Select columns you want to keep
UG_Jin_clim <- UG_JIN_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
UG_Jin_clim$DateTime <- as.POSIXct(substr(UG_Jin_clim$DateTime, 1, 15), 
                                   format = "%Y%m%dT%H", tz = "UTC")



# for ZA_CATH ----

ZA_CATH_Temp <- read_csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/clim_data/ZA_CATH_ERA5_export_TEMP.csv")
# Renaming columns in the BJ_BEF_Temp data frame
colnames(ZA_CATH_Temp)[colnames(ZA_CATH_Temp) == "system:index"] <- "DateTime"
colnames(ZA_CATH_Temp)[colnames(ZA_CATH_Temp) == "mean"] <- "ERA5_Temp_K"




ZA_CATH_Precip <- read_csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/clim_data/ZA_CATH_ERA5_export_PRECIP.csv")
# Renaming columns in the BJ_BEF_Temp data frame
colnames(ZA_CATH_Precip)[colnames(ZA_CATH_Precip) == "system:index"] <- "DateTime"
colnames(ZA_CATH_Precip)[colnames(ZA_CATH_Precip) == "mean"] <- "ERA5_Precip_m"




# List of datasets to combine
datasets <- list(ZA_CATH_Temp, ZA_CATH_Precip)

# Combine all datasets in the list by "DateTime"
ZA_CATH_Combined <- Reduce(function(x, y) merge(x, y, by = "DateTime", all = TRUE), datasets)




# Select columns you want to keep
ZA_Cath_clim <- ZA_CATH_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
ZA_Cath_clim$DateTime <- as.POSIXct(substr(ZA_Cath_clim$DateTime, 1, 15), 
                                    format = "%Y%m%dT%H", tz = "UTC")



# for ZA_KRU ----

ZA_KRU_Temp <- read_csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/clim_data/ZA_KRU_ERA5_export_TEMP.csv")
# Renaming columns in the BJ_BEF_Temp data frame
colnames(ZA_KRU_Temp)[colnames(ZA_KRU_Temp) == "system:index"] <- "DateTime"
colnames(ZA_KRU_Temp)[colnames(ZA_KRU_Temp) == "mean"] <- "ERA5_Temp_K"




ZA_KRU_Precip <- read_csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/clim_data/ZA_KRU_ERA5_export_PRECIP.csv")
# Renaming columns in the BJ_BEF_Temp data frame
colnames(ZA_KRU_Precip)[colnames(ZA_KRU_Precip) == "system:index"] <- "DateTime"
colnames(ZA_KRU_Precip)[colnames(ZA_KRU_Precip) == "mean"] <- "ERA5_Precip_m"




# List of datasets to combine
datasets <- list(ZA_KRU_Temp, ZA_KRU_Precip)

# Combine all datasets in the list by "DateTime"
ZA_KRU_Combined <- Reduce(function(x, y) merge(x, y, by = "DateTime", all = TRUE), datasets)




# Select columns you want to keep
ZA_Kru_clim <- ZA_KRU_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
ZA_Kru_clim$DateTime <- as.POSIXct(substr(ZA_Kru_clim$DateTime, 1, 15), 
                                   format = "%Y%m%dT%H", tz = "UTC")



# for ZA_WGN ----

ZA_WGN_Temp <- read_csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/clim_data/ZA_WGN_ERA5_export_TEMP.csv")
# Renaming columns in the BJ_BEF_Temp data frame
colnames(ZA_WGN_Temp)[colnames(ZA_WGN_Temp) == "system:index"] <- "DateTime"
colnames(ZA_WGN_Temp)[colnames(ZA_WGN_Temp) == "mean"] <- "ERA5_Temp_K"




ZA_WGN_Precip <- read_csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/clim_data/ZA_WGN_ERA5_export_PRECIP.csv")
# Renaming columns in the BJ_BEF_Temp data frame
colnames(ZA_WGN_Precip)[colnames(ZA_WGN_Precip) == "system:index"] <- "DateTime"
colnames(ZA_WGN_Precip)[colnames(ZA_WGN_Precip) == "mean"] <- "ERA5_Precip_m"




# List of datasets to combine
datasets <- list(ZA_WGN_Temp, ZA_WGN_Precip)

# Combine all datasets in the list by "DateTime"
ZA_WGN_Combined <- Reduce(function(x, y) merge(x, y, by = "DateTime", all = TRUE), datasets)




# Select columns you want to keep
ZA_Wgn_clim <- ZA_WGN_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
ZA_Wgn_clim$DateTime <- as.POSIXct(substr(ZA_Wgn_clim$DateTime, 1, 15), 
                                   format = "%Y%m%dT%H", tz = "UTC")



# for ZM_MON ----

ZM_MON_Temp <- read_csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/clim_data/ZM_MON_ERA5_export_TEMP.csv")
# Renaming columns in the BJ_BEF_Temp data frame
colnames(ZM_MON_Temp)[colnames(ZM_MON_Temp) == "system:index"] <- "DateTime"
colnames(ZM_MON_Temp)[colnames(ZM_MON_Temp) == "mean"] <- "ERA5_Temp_K"




ZM_MON_Precip <- read_csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/clim_data/ZM_MON_ERA5_export_PRECIP.csv")
# Renaming columns in the BJ_BEF_Temp data frame
colnames(ZM_MON_Precip)[colnames(ZM_MON_Precip) == "system:index"] <- "DateTime"
colnames(ZM_MON_Precip)[colnames(ZM_MON_Precip) == "mean"] <- "ERA5_Precip_m"




# List of datasets to combine
datasets <- list(ZM_MON_Temp, ZM_MON_Precip)

# Combine all datasets in the list by "DateTime"
ZM_MON_Combined <- Reduce(function(x, y) merge(x, y, by = "DateTime", all = TRUE), datasets)




# Select columns you want to keep
ZM_Mon_clim <- ZM_MON_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
ZM_Mon_clim$DateTime <- as.POSIXct(substr(ZM_Mon_clim$DateTime, 1, 15), 
                                   format = "%Y%m%dT%H", tz = "UTC")







# calculate MAP and MAT from 1991-2020 ERA5-Land Hourly climate reanalysis data ----

# List of site dataframe names (already loaded in R)
site_names <- c("BW_Gum_clim", "BW_Nxr_clim", "CG_Tch_clim", "GH_Ank_clim",
                "ML_AgG_clim", "NE_Waf_clim", "NE_Wam_clim", "SD_Dem_clim",
                "SN_Dhr_clim", "SN_Nkr_clim", "SN_Rag_clim", "UG_Jin_clim",
                "ZA_Cath_clim", "ZA_Kru_clim", "ZA_Wgn_clim", "ZM_Mon_clim")

# Initialize an empty list to store results
all_sites_results <- list()

# Store yearly anomaly tables per site
all_sites_anomalies <- list()


# Loop through each site dataframe in R's environment
for (site in site_names) {
  
  # Get the dataframe for the site
  df <- get(site)
  
  # Ensure DateTime is in correct format and extract Year
  df <- df %>%
    mutate(Year = year(ymd_hms(DateTime)))  # Extract year from DateTime
  
  # Compute **yearly precipitation totals**
  yearly_precip <- df %>%
    group_by(Year) %>%
    summarise(
      Annual_Precip = sum(ERA5_Precip_m, na.rm = TRUE) * 1000  # Convert meters to mm
    )
  
  # Compute **Mean Annual Temperature (MAT) directly from all time steps** 
  MAT_value <- mean(df$ERA5_Temp_K, na.rm = TRUE) - 273.15  # Convert Kelvin to Celsius
  
  # Compute **MAP as the mean of annual precipitation totals**
  MAP_value <- mean(yearly_precip$Annual_Precip, na.rm = TRUE)
  
  # Store results in a dataframe
  site_summary <- data.frame(
    Site = site,
    MAP = MAP_value,
    MAT = MAT_value
  )
  
  # Store results for each site
  all_sites_results[[site]] <- site_summary
  
  
  # Calculate % anomaly per year relative to MAP
  anomalies <- yearly_precip %>%
    mutate(
      Site = site,
      MAP = MAP_value,
      Precip_Anomaly_Percent = ((Annual_Precip - MAP_value) / MAP_value) * 100
    )
  all_sites_anomalies[[site]] <- anomalies
  
  
}

# Combine all site results into one dataframe
final_MAP_MAT <- bind_rows(all_sites_results)
final_precip_anomalies <- bind_rows(all_sites_anomalies)

# View results
print(final_MAP_MAT)



# Save results as CSV (optional)
#write.csv(final_MAP_MAT, "MAP_MAT_1991_2020.csv", row.names = FALSE)
#write.csv(final_precip_anomalies, "precip_anomaly.csv", row.names = FALSE)





# Plot the lollipop plot ----

# Site-year count for each site
site_years <- data.frame(
  Site = c("BW_Gum", "BW_Nxr", "CG_Tch", "GH_Ank", "ML_AgG", "NE_Waf", "NE_Wam",
           "SD_Dem", "SN_Dhr", "SN_Nkr", "SN_Rag", "UG_Jin", "ZA_Cath", "ZA_Kru",
           "ZA_Wgn", "ZM_Mon"),
  Site_Years = c(3, 3, 4, 4, 5, 14, 14, 5, 11, 4, 4, 1, 8, 14, 13, 10)
)



# Clean site names to match `site_years`
final_MAP_MAT <- final_MAP_MAT %>%
  mutate(Site = gsub("_clim", "", Site))


# Merge PET with MAP and MAT data
clim_data <- merge(final_MAP_MAT, PET_1991_2020, by = "Site")

# Calculate Aridity Index (AI = MAP / PET)
clim_data$AI <- clim_data$MAP / clim_data$PET


#write.csv(clim_data, "MAP_MAT_AI_1991_2020.csv", row.names = FALSE)



# Combine site-years with MAP & MAT values
site_data <- left_join(clim_data, site_years, by = "Site")




# Convert to long format for MAP, MAT and AI
site_data_long <- site_data %>%
  pivot_longer(cols = c("MAP", "MAT", "AI"), names_to = "Gradient", values_to = "Value")


# Shared base theme with panel border
custom_theme <- theme_minimal(base_size = 21) +
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 20),
    axis.text = element_text(size = 16),
    panel.grid.major = element_line(color = "grey80", linetype = "dotted"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    legend.position = "right",
    legend.text = element_text(size = 22),
    legend.title = element_text(size = 20, face = "bold"),
    legend.key.size = unit(1.5, "lines"),
    strip.text = element_text(face = "bold", size = 18),
    strip.background = element_blank(),
    panel.spacing = unit(2, "cm")
  )


# Base plot
base_plot <- ggplot(site_data_long, aes(x = Value, y = Site_Years, color = Site)) +
  geom_segment(aes(xend = Value, yend = 0), size = 1) +
  geom_point(size = 4) +
  labs(x = NULL, y = "Number of site-years of flux observations") +
  guides(color = guide_legend(title = "Site")) +
  scale_y_continuous(breaks = seq(0, 16, by = 2)) +
  custom_theme


# MAP panel
p_map <- base_plot %+% subset(site_data_long, Gradient == "MAP") +
  scale_x_continuous(limits = c(0, NA)) +
  labs(x = "Mean Annual Precipitation (mm)") +
  theme(
    strip.text = element_blank(),
    strip.background = element_blank()
  )


# MAT panel 
p_mat <- base_plot %+% subset(site_data_long, Gradient == "MAT") +
  scale_x_continuous(limits = c(10, NA)) +
  labs(x = "Mean Annual Temperature (°C)") +
  theme(
    strip.text = element_blank(),
    strip.background = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )


# AI panel
p_ai <- base_plot %+% subset(site_data_long, Gradient == "AI") +
  scale_x_continuous(limits = c(0, 1.5)) +  # adjust based on your AI range
  labs(x = "Aridity Index (MAP/PET)") +
  theme(
    strip.text = element_blank(),
    strip.background = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )




# Combine with collected guides and clear panel distinction
(p_map + p_mat + p_ai) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = 'A')






# Save the final plot
#ggsave("lollipop_Final5.png", width = 16, height = 10, dpi = 300, bg = "white")




# Plot the Whitaker plot ----

site_data_2 <- final_MAP_MAT %>% select(Site, MAP, MAT)


# Create the Whittaker Biome Plot
Whittaker_plot <- ggplot() +
  
  # Add biome classification from plotbiomes
  geom_polygon(data = Whittaker_biomes,
               aes(x = temp_c, y = precp_cm * 10, fill = biome),  # Convert cm to mm for your scale
               color = "gray98", size = 0.5) +  # Light polygon borders
  
  # Define Whittaker biome colors 
  scale_fill_manual(name = "Whittaker Biomes",
                    values = c("Tropical rain forest" = "grey72",
                               "Temperate rain forest" = "grey80",
                               "Boreal forest" = "grey88",
                               "Temperate seasonal forest" = "#A5C790",
                               "Tropical seasonal forest/savanna" = "#e6da00",
                               "Woodland/shrubland" = "#DCBB50",
                               "Temperate grassland/desert" = "#FCD57A",
                               "Subtropical desert" = "#D16E3F",
                               "Tundra" = "#C1E1DD")) +
  
  # Add site-specific data points
  geom_point(data = site_data_2, aes(x = MAT, y = MAP, color = Site), size = 3) +
  
  # Formatting & labels
  labs(
    x = expression("Mean Annual Temperature " ( degree~C)),
    y = "Mean Annual Precipitation (mm)"
  ) +
  
  # Theme settings 
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 19),
    axis.text = element_text(face = "bold", size = 19),
    legend.position = c("left"),  # Position legend in similar location
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16, face = "bold")
  )

# Print the plot
print(Whittaker_plot)

# Define file path and resolution
ggsave("Whittaker_Biome.png", 
       plot = Whittaker_plot,   
       width = 12, height = 10,  # Adjust dimensions
       dpi = 600, bg = "white")  # High resolution








# Make plots to visualize trends across sites ----

# Remove "_clim" from Site names
#final_MAP_MAT <- final_MAP_MAT %>%
  #mutate(Site = gsub("_clim", "", Site))  # Replace "_clim" with an empty string



# Convert data to long format for faceted plot
final_MAP_MAT_long <- final_MAP_MAT %>%
  pivot_longer(cols = c(MAP, MAT), names_to = "Variable", values_to = "Value")


# Faceted line graph for MAP & MAT
MAT_MAP_plot <- ggplot(final_MAP_MAT_long, aes(x = Site, y = Value, group = Variable, color = Variable)) +
  geom_line(size = 1) +  # Line plot
  geom_point(size = 3) +  # Add points for emphasis
  theme_minimal() +
  labs(title = "Mean Annual Precipitation and Temperature Across Sites",
       x = "Site",
       y = "Value") +
  facet_wrap(~Variable, scales = "free_y") +  # Create separate panels for MAP & MAT
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability


# Save the MAP plot as a high-resolution PNG
ggsave("MAT_MAP.png", MAT_MAP_plot, width = 12, height = 8, dpi = 300, bg = "white")



# relationship between MAT and MAP 
MAT_MAP_trend <- ggplot(final_MAP_MAT, aes(x = MAP, y = MAT)) +
  geom_point(aes(color = Site), size = 4, alpha = 0.8) +  # Color by site
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +  # Trend line
  theme_minimal() +
  labs(title = "Relationship Between MAP and MAT Across Sites",
       x = "Mean Annual Precipitation (mm)",
       y = "Mean Annual Temperature (°C)",
       color = "Site")




ggsave("MAT_MAP_trend.png", MAT_MAP_trend, width = 12, height = 8, dpi = 300, bg = "white")







# List of site dataframe names (already loaded in R)
# site_names <- c("BW_GUM_clim", "BW_NXR_clim", "CG_TCH_clim", "GH_ANK_clim",
# "ML_AGG_clim", "NE_WAF_clim", "NE_WAM_clim", "SD_DEM_clim",
# "SN_DHR_clim", "SN_NKR_clim", "SN_RAG_clim", "UG_JIN_clim",
# "ZA_CATH_clim", "ZA_KRU_clim", "ZA_WGN_clim", "ZM_MON_clim")

# Initialize an empty list to store results

#all_sites_results <- list()

# Loop through each site dataframe in R's environment
for (site in site_names) {
  
  # Get the dataframe for the site
  df <- get(site)
  
  # Ensure DateTime is in correct format and extract Year
  df <- df %>%
    mutate(Year = year(ymd_hms(DateTime)))  # Extract Year from DateTime
  
  # Compute **Annual Mean Temperature (MAT)**
  yearly_MAT <- df %>%
    group_by(Year) %>%
    summarise(MAT = mean(ERA5_Temp_K, na.rm = TRUE) - 273.15) %>%  # Convert K to °C
    mutate(Site = gsub("_clim", "", site))  # Remove "_clim" from site names
  
  # Store results for each site
  all_sites_results[[site]] <- yearly_MAT
}

# Combine all site results into one dataframe
final_MAP_MAT_yearly <- bind_rows(all_sites_results)



#ggplot(final_MAP_MAT_yearly, aes(x = Year, y = MAT, color = Site, group = Site)) +
#geom_line(size = 1) +  # Line plot
#theme_minimal() +
#labs(title = "Annual Temperature (MAT) Across Sites",
#  x = "Year",
# y = "MAT (°C)",
#  color = "Site") +
# theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels


MAT_plot <-  ggplot(final_MAP_MAT_yearly, aes(x = Year, y = MAT, group = Site, color = Site)) +
  geom_line(size = 1) +  # Line plot
  theme_minimal() +
  labs(title = "Annual Temperature (MAT) Across Sites",
       x = "Year",
       y = "MAT (°C)") +
  facet_wrap(~ Site, scales = "free_y") +  # Create separate panels for each site
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        legend.position = "none")  # Remove legend since sites are in facets







# Initialize an empty list to store results
all_sites_results_MAP <- list()

# Loop through each site dataframe in R's environment
for (site in site_names) {
  
  # Get the dataframe for the site
  df <- get(site)
  
  # Ensure DateTime is in correct format and extract Year
  df <- df %>%
    mutate(Year = year(ymd_hms(DateTime)))  # Extract Year from DateTime
  
  # Compute **Annual Precipitation Total (MAP)**
  yearly_MAP <- df %>%
    group_by(Year) %>%
    summarise(MAP = sum(ERA5_Precip_m, na.rm = TRUE) * 1000) %>%  # Convert m to mm
    mutate(Site = gsub("_clim", "", site))  # Remove "_clim" from site names
  
  # Store results for each site
  all_sites_results_MAP[[site]] <- yearly_MAP
}

# Combine all site results into one dataframe
final_MAP_yearly <- bind_rows(all_sites_results_MAP)



# Faceted plot for MAP trends
MAP_plot <- ggplot(final_MAP_yearly, aes(x = Year, y = MAP, group = Site, color = Site)) +
  geom_line(size = 1) +  # Line plot
  theme_minimal() +
  labs(title = "Annual Precipitation (MAP) Across Sites",
       x = "Year",
       y = "MAP (mm)") +
  facet_wrap(~ Site, scales = "free_y") +  # Create separate panels for each site
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        legend.position = "none")  # Remove legend since sites are in facets





# Save the MAP plot as a high-resolution PNG
ggsave("MAP_Across_Sites.png", MAP_plot, width = 12, height = 8, dpi = 300, bg = "white")


ggsave("MAT_Across_Sites.png", MAT_plot, width = 12, height = 8, dpi = 300, bg = "white")



