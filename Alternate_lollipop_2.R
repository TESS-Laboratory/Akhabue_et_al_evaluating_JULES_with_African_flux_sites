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
library(ggrepel)
library(cowplot)
library(grid)

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
BW_GUM_clim <- BW_GUM_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
BW_GUM_clim$DateTime <- as.POSIXct(substr(BW_GUM_clim$DateTime, 1, 15), 
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
BW_NXR_clim <- BW_NXR_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
BW_NXR_clim$DateTime <- as.POSIXct(substr(BW_NXR_clim$DateTime, 1, 15), 
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
CG_TCH_clim <- CG_TCH_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
CG_TCH_clim$DateTime <- as.POSIXct(substr(CG_TCH_clim$DateTime, 1, 15), 
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
GH_ANK_clim <- GH_ANK_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
GH_ANK_clim$DateTime <- as.POSIXct(substr(GH_ANK_clim$DateTime, 1, 15), 
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
ML_AGG_clim <- ML_AGG_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
ML_AGG_clim$DateTime <- as.POSIXct(substr(ML_AGG_clim$DateTime, 1, 15), 
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
NE_WAF_clim <- NE_WAF_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
NE_WAF_clim$DateTime <- as.POSIXct(substr(NE_WAF_clim$DateTime, 1, 15), 
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
NE_WAM_clim <- NE_WAM_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
NE_WAM_clim$DateTime <- as.POSIXct(substr(NE_WAM_clim$DateTime, 1, 15), 
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
SD_DEM_clim <- SD_DEM_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
SD_DEM_clim$DateTime <- as.POSIXct(substr(SD_DEM_clim$DateTime, 1, 15), 
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
SN_DHR_clim <- SN_DHR_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
SN_DHR_clim$DateTime <- as.POSIXct(substr(SN_DHR_clim$DateTime, 1, 15), 
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
SN_NKR_clim <- SN_NKR_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
SN_NKR_clim$DateTime <- as.POSIXct(substr(SN_NKR_clim$DateTime, 1, 15), 
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
SN_RAG_clim <- SN_RAG_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
SN_RAG_clim$DateTime <- as.POSIXct(substr(SN_RAG_clim$DateTime, 1, 15), 
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
UG_JIN_clim <- UG_JIN_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
UG_JIN_clim$DateTime <- as.POSIXct(substr(UG_JIN_clim$DateTime, 1, 15), 
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
ZA_CATH_clim <- ZA_CATH_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
ZA_CATH_clim$DateTime <- as.POSIXct(substr(ZA_CATH_clim$DateTime, 1, 15), 
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
ZA_KRU_clim <- ZA_KRU_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
ZA_KRU_clim$DateTime <- as.POSIXct(substr(ZA_KRU_clim$DateTime, 1, 15), 
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
ZA_WGN_clim <- ZA_WGN_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
ZA_WGN_clim$DateTime <- as.POSIXct(substr(ZA_WGN_clim$DateTime, 1, 15), 
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
ZM_MON_clim <- ZM_MON_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
ZM_MON_clim$DateTime <- as.POSIXct(substr(ZM_MON_clim$DateTime, 1, 15), 
                                   format = "%Y%m%dT%H", tz = "UTC")







# calculate MAP and MAT from 1991-2020 ERA5-Land Hourly climate reanalysis data ----

# List of site dataframe names (already loaded in R)
site_names <- c("BW_GUM_clim", "BW_NXR_clim", "CG_TCH_clim", "GH_ANK_clim",
                "ML_AGG_clim", "NE_WAF_clim", "NE_WAM_clim", "SD_DEM_clim",
                "SN_DHR_clim", "SN_NKR_clim", "SN_RAG_clim", "UG_JIN_clim",
                "ZA_CATH_clim", "ZA_KRU_clim", "ZA_WGN_clim", "ZM_MON_clim")

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
  Site = c(
    "BW_GUM", "BW_NXR", "CG_TCH", "GH_ANK", "ML_AGG", "NE_WAF", "NE_WAM", "SD_DEM",
    "SN_DHR", "SN_NKR", "SN_RAG", "UG_JIN", "ZA_CATH", "ZA_KRU", "ZA_WGN", "ZM_MON"
  ),
  
  Site_Label = letters[1:16],
  
  Site_Years = c(
    3, 3, 4, 4,
    5, 14, 14, 5,
    11, 4, 4, 1,
    8, 14, 13, 10
  ),
  
  EcosystemType = c(
    "Wetland",   # a: BW_GUM
    "Wetland",   # b: BW_NXR
    "Grassland", # c: CG_TCH
    "Forest",    # d: GH_ANK
    "Grassland", # e: ML_AGG
    "Savanna",   # f: NE_WAF
    "Savanna",   # g: NE_WAM
    "Grassland", # h: SD_DEM
    "Grassland", # i: SN_DHR
    "Cropland",  # j: SN_NKR
    "Cropland",  # k: SN_RAG
    "Wetland",   # l: UG_JIN
    "Grassland", # m: ZA_CATH
    "Savanna",   # n: ZA_KRU
    "Grassland", # o: ZA_WGN
    "Forest"     # p: ZM_MON
  )
)


# Set the ecosystem-type order used in the legend
site_years$EcosystemType <- factor(
  site_years$EcosystemType,
  levels = c(
    "Forest",
    "Savanna",
    "Grassland",
    "Cropland",
    "Wetland"
  )
)


site_key_entries <- paste0(
  site_years$Site_Label,
  ": ",
  site_years$Site
)

# Split the site key into two columns
site_key_left <- paste(
  site_key_entries[1:8],
  collapse = "\n"
)

site_key_right <- paste(
  site_key_entries[9:16],
  collapse = "\n"
)



my_ecosystem_colors <- c(
  "Forest" = "#2ca25f",
  "Savanna" = "#b8860b",
  "Grassland" = "#8a5fd3",
  "Cropland" = "#f28e2b",
  "Wetland" = "#1f78b4"
)



# Clean site names in the MAT/MAP dataset
final_MAP_MAT <- final_MAP_MAT %>%
  mutate(
    Site = gsub("_clim", "", Site)
  )


# Merge PET with MAP and MAT data
clim_data <- final_MAP_MAT %>%
  left_join(
    PET_1991_2020,
    by = "Site"
  )


# Calculate Aridity Index: MAP/PET
clim_data <- clim_data %>%
  mutate(
    AI = MAP / PET
  )


# Optional: save the combined climatic data
# write.csv(
#   clim_data,
#   "MAP_MAT_AI_1991_2020.csv",
#   row.names = FALSE
# )



site_data <- clim_data %>%
  left_join(
    site_years,
    by = "Site"
  )


# Check that all sites were matched successfully
unmatched_sites <- site_data %>%
  filter(
    is.na(Site_Label) |
      is.na(Site_Years) |
      is.na(EcosystemType)
  )

if (nrow(unmatched_sites) > 0) {
  print(unmatched_sites)
  
  stop(
    "Some sites did not match the site_years table. ",
    "Check the site names shown above."
  )
}



# 6. Convert to long format


site_data_long <- site_data %>%
  pivot_longer(
    cols = c("MAP", "MAT", "AI"),
    names_to = "Gradient",
    values_to = "Value"
  )


# Shared plot theme


custom_theme <- theme_minimal(base_size = 21) +
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 22),
    axis.text = element_text(size = 20),
    panel.grid.major = element_line(colour = "grey80", linetype = "dotted"),
    panel.grid.minor = element_line(colour = "grey90", linewidth = 0.5 ),
    panel.border = element_rect(colour = "black", fill = NA,linewidth = 1),
    legend.position = "right",
    legend.text = element_text(size = 25, face = "bold"),
    legend.title = element_text(size = 25, face = "bold"),
    legend.key.size = unit(1.5, "lines"),
    strip.text = element_text(face = "bold", size = 20),
    strip.background = element_blank(),
    plot.margin = margin(
      t = 10,
      r = 10,
      b = 10,
      l = 10
    )
  )



make_lollipop_plot <- function(
    plot_data,
    x_label,
    x_limits,
    show_y_axis = TRUE
) {
  
  plot_object <- ggplot(
    plot_data,
    aes(
      x = Value,
      y = Site_Years,
      colour = EcosystemType
    )
  ) +
    
    geom_segment(
      aes(
        xend = Value,
        y = 0,
        yend = Site_Years
      ),
      linewidth = 1
    ) +
    
    geom_point(
      size = 9
    ) +
    
    ggrepel::geom_text_repel(
      aes(
        label = Site_Label
      ),
      size = 8,
      fontface = "bold",
      max.overlaps = Inf,
      segment.colour = "grey50",
      box.padding = 0.5,
      point.padding = 0.25,
      min.segment.length = 0,
      show.legend = FALSE
    ) +
    
    scale_colour_manual(
      values = my_ecosystem_colors,
      drop = FALSE
    ) +
    
    scale_x_continuous(
      limits = x_limits
    ) +
    
    scale_y_continuous(
      breaks = seq(
        0,
        16,
        by = 2
      ),
      limits = c(
        0,
        14.7
      ),
      expand = expansion(
        mult = c(
          0,
          0.03
        )
      )
    ) +
    
    labs(
      x = x_label,
      y = "Number of site-years of flux observations",
      colour = "Ecosystem type"
    ) +
    
    custom_theme
  
  if (!show_y_axis) {
    
    plot_object <- plot_object +
      theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
  }
  
  return(plot_object)
}



#  Create the MAT and AI panels

p_mat <- make_lollipop_plot(
  plot_data = site_data_long %>%
    filter(
      Gradient == "MAT"
    ),
  
  x_label = "Mean Annual Temperature (°C)",
  
  x_limits = c(
    9,
    30.5
  ),
  
  show_y_axis = TRUE
)


p_ai <- make_lollipop_plot(
  plot_data = site_data_long %>%
    filter(
      Gradient == "AI"
    ),
  
  x_label = "Aridity Index (MAP/PET)",
  
  x_limits = c(
    0,
    1.55
  ),
  
  show_y_axis = FALSE
)


ecosystem_legend <- cowplot::get_legend(
  p_mat +
    
    guides(
      colour = guide_legend(
        title = "Ecosystem type",
        override.aes = list(
          size = 7,
          linewidth = 1.2
        )
      )
    ) +
    
    theme(
      legend.position = "right",
      legend.justification = c(
        0,
        1
      ),
      legend.box.just = "left",
      legend.margin = margin(
        0,
        0,
        0,
        0
      )
    )
)


site_key_plot <- cowplot::ggdraw() +
  
  cowplot::draw_label(
    "Site (letter key)",
    x = 0,
    y = 1,
    hjust = 0,
    vjust = 1,
    fontface = "bold",
    size = 22
  ) +
  
  cowplot::draw_label(
    site_key_left,
    x = 0,
    y = 0.93,
    hjust = 0,
    vjust = 1,
    size = 22,
    fontface = "bold",
    lineheight = 1.25
  ) +
  
  cowplot::draw_label(
    site_key_right,
    x = 0.45,
    y = 0.95,
    hjust = 0,
    vjust = 1,
    size = 22,
    fontface = "bold",
    lineheight = 1.25
  )


#  Combine the two main panels

main_panels <- cowplot::plot_grid(
  p_mat +
    theme(
      legend.position = "none"
    ),
  
  p_ai +
    theme(
      legend.position = "none"
    ),
  
  labels = c(
    "A",
    "B"
  ),
  
  label_size = 22,
  label_fontface = "plain",
  label_x = 0,
  label_y = 1,
  
  ncol = 2,
  align = "hv",
  axis = "tblr",
  
  rel_widths = c(
    1,
    1
  )
)


# Combine ecosystem legend and site key with a small gap
right_column_content <- cowplot::plot_grid(
  ecosystem_legend,
  NULL,              # small blank spacer
  site_key_plot,
  ncol = 1,
  align = "v",
  rel_heights = c(
    0.24,  # ecosystem legend
    0.035, # gap between the two legends
    0.725  # site-letter key
  )
)


right_column <- cowplot::plot_grid(
  NULL,
  right_column_content,
  ncol = 1,
  rel_heights = c(0.16, 0.84)
)

#  Create the final figure


final_plot <- cowplot::plot_grid(
  main_panels,
  right_column,
  
  ncol = 2,
  
  rel_widths = c(
    4.6,
    1.4
  )
)


# Display the final figure
final_plot

# Save the final plot
ggsave("latest_lollipop_LatestFinalnew.png", width = 20, height = 12, dpi = 300, bg = "white")



