#    ✓ Load library----
# library(dplyr)
library(tidyverse)
library(tidyr)
library(magrittr)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)



#    ✓ Code to return list of all subdirectory names in external data directory (using absolute file path) ----

# Define the absolute file path to the external data directory
external_data_dir <- "C:/Users/efa206/OneDrive - University of Exeter/Desktop/O2-/WITH DATA"


#    ✓ Next step load metadata file? to facilitate your own QAQC to check what proportion of site you hold data for etc.----

file_path <- "C:/workspace/Akhabue-dev/African_flux_meta_data.csv"
African_flux_meta_data <- read_csv(file_path)




# FUNCTIONS ----

#    ✓ Some sub directory have multiple files, some of which need to be merged to make a single file. write 2 functions for directories with single file and with multiple files.
#       if there is a way to have just one function that with an argument to suggest that it returns multiple files if multiple files are found in the directory.


## Function to list all subdirectories within a directory----
list_subdirectories <- function(directory) {
  dirs <- list.dirs(directory, recursive = TRUE, full.names = FALSE)
  # Filter out empty strings
  dirs <- dirs[dirs != ""]
  return(dirs)
}

subdirectories <- list_subdirectories(external_data_dir)

print(subdirectories)


## Function to load files and perform operation within directory. I have written 6 different function to suite the different kinds of data format I have. ----

###   ✓ Define a function to load files within a directory----
load_files1 <- function(directory) {
  # List files within the directory
  files <- list.files(directory, full.names = TRUE)
  # Initialize a list to store loaded data
  loaded_data <- list()
  # Load each file and store in the list
  for (file in files) {
    loaded_data[[basename(file)]] <- read_delim(file)  # Assuming CSV files, adjust as needed
  }
  return(loaded_data)
}




###   ✓ Define a function to load files within a directory----
load_files2 <- function(directory) {
  # List files within the directory
  files <- list.files(directory, full.names = TRUE)
  # Initialize a list to store loaded data
  loaded_data <- list()
  # Load each file and store in the list
  for (file in files) {
    loaded_data[[basename(file)]] <- read_delim(file, delim = ";")  # Assuming CSV files, adjust as needed
  }
  return(loaded_data)
}




###   ✓ Define a function to load files within the UG-Jin directory ----
load_files3 <- function(directory) {
  # List files within the directory
  files <- list.files(directory, full.names = TRUE)
  # Initialize a list to store loaded data
  loaded_data <- list()
  # Load each file and store in the list
  for (file in files) {
    loaded_data[[basename(file)]] <- read.csv(file)  # Assuming CSV files, adjust as needed
  }
  return(loaded_data)
}




###   ✓ Define a function to load files within the Cathedral peak directory ----
load_files4 <- function(directory) {
  # List files within the directory
  files <- list.files(directory, full.names = TRUE)
  # Initialize a list to store loaded data
  loaded_data <- list()
  # Load each file and store in the list
  for (file in files) {
    loaded_data[[basename(file)]] <- read_csv(file)  # Assuming CSV files, adjust as needed
  }
  return(loaded_data)
}




###   ✓ Define a function to load files within the ZA_Wgn directory ----
load_files5 <- function(directory) {
  # List files within the directory
  files <- list.files(directory, full.names = TRUE)
  # Initialize a list to store loaded data
  loaded_data <- list()
  # Load each file and store in the list
  for (file in files) {
    loaded_data[[basename(file)]] <- read_delim(file) %>% mutate(across(-"period end", as.numeric))  # Assuming CSV files, adjust as needed
  }
  return(loaded_data)
}



###   ✓ Define a function to load files within the ZA_Wgn_met directory ----
load_files6 <- function(directory) {
  # List files within the directory
  files <- list.files(directory, full.names = TRUE)
  # Initialize a list to store loaded data
  loaded_data <- list()
  # Load each file and store in the list
  for (file in files) {
    loaded_data[[basename(file)]] <- read_delim(file, col_names = FALSE)  # Assuming CSV files, adjust as needed
  }
  return(loaded_data)
}




###   ✓ Define a function to perform operations on loaded data----
perform_operations <- function(data) {
  # Example: Summarize loaded data
  summaries <- lapply(data, summary)
  return(summaries)
}



# Load and clean each data ----

###   ✓ GUMA_FLUX STATION - (probably add more info in this comment) ----
# Load files and perform operations within the BW-GUM subdir
bw_gum_data <- load_files1(file.path(external_data_dir, "BW-Gum"))
bw_gum_summaries <- perform_operations(bw_gum_data)

# Move bw_gum to a simpler df
BW_GUM <- (bw_gum_data[["BW_GUM_2018_2020.csv"]])


# Replace -9999.9 with NA in BW_GUM dataset
BW_GUM[BW_GUM == -9999] <- NA
BW_GUM[BW_GUM == -999900] <- NA
BW_GUM[BW_GUM == -999900.00000] <- NA

# Convert the date column to datetime objects UTC format
BW_GUM$date <- dmy_hm(BW_GUM$date)



# select the columns I need
new_BW_GUM_vars <- c("date", "H", "LE", "co2_flux")

BW_GUM_Flux<- BW_GUM %>% dplyr::select(one_of(new_BW_GUM_vars))
#View(BW_GUM_Flux)


#Change the variable names to uniform names across all sites???

BW_GUM_Flux_new <- BW_GUM_Flux %>% rename(`sensible_heat_W/m²` = `H`, `latent_heat_W/m²` = `LE`, 
                                          `co2_flux_μmol.m-2.s-1` = `co2_flux`)
#View(BW_GUM_Flux_new)


# Replace extreme values with NA in sensible_heat_W/m²
BW_GUM_Flux_new$`sensible_heat_W/m²` <- ifelse(
  BW_GUM_Flux_new$`sensible_heat_W/m²` < -1000 | BW_GUM_Flux_new$`sensible_heat_W/m²` > 1000,
  NA,
  BW_GUM_Flux_new$`sensible_heat_W/m²`
)

# Replace extreme values with NA in latent_heat_W/m²
BW_GUM_Flux_new$`latent_heat_W/m²` <- ifelse(
  BW_GUM_Flux_new$`latent_heat_W/m²` < -1000 | BW_GUM_Flux_new$`latent_heat_W/m²` > 1000,
  NA,
  BW_GUM_Flux_new$`latent_heat_W/m²`
)





# Define the indices of columns to remove keeping just the meteorological variables
columns_to_remove_MET <- c(2:12, 15:18, 23)

# Remove selected columns using negative indexing
BW_GUM_MET <- BW_GUM[, -columns_to_remove_MET]








###   ✓ NXARAGA_FLUX STATION - (probably add more info in this comment) ----
# Load files and perform operations within the BW-Nxr subdir
bw_nxr_data <- load_files1(file.path(external_data_dir, "BW-Nxr"))
bw_nxr_summaries <- perform_operations(bw_nxr_data)

# Move bw_nxr to a simpler df
BW_NXR <- (bw_nxr_data[["BW_NXA_2018_2020.csv"]])

# Replace -9999.9 with NA in BW_GUM dataset
BW_NXR[BW_NXR == -9999] <- NA
BW_NXR[BW_NXR == -999900] <- NA

# Convert the date column to datetime objects UTC format
BW_NXR$date <- dmy_hm(BW_NXR$date)


# select the columns I need
new_BW_NXR_vars <- c("date", "H", "LE", "co2_flux")

BW_NXR_Flux<- BW_NXR %>% dplyr::select(one_of(new_BW_NXR_vars))
#View(BW_NXR_Flux)


#Change the variable names to uniform names across all sites???

BW_NXR_Flux_new <- BW_NXR_Flux %>% rename(`sensible_heat_W/m²` = `H`, `latent_heat_W/m²` = `LE`, 
                                          `co2_flux_μmol.m-2.s-1` = `co2_flux`)
#View(BW_NXR_Flux_new)



# Replace extreme values with NA in sensible_heat_W/m²
BW_NXR_Flux_new$`sensible_heat_W/m²` <- ifelse(
  BW_NXR_Flux_new$`sensible_heat_W/m²` < -1000 | BW_NXR_Flux_new$`sensible_heat_W/m²` > 1000,
  NA,
  BW_NXR_Flux_new$`sensible_heat_W/m²`
)

# Replace extreme values with NA in latent_heat_W/m²
BW_NXR_Flux_new$`latent_heat_W/m²` <- ifelse(
  BW_NXR_Flux_new$`latent_heat_W/m²` < -1000 | BW_NXR_Flux_new$`latent_heat_W/m²` > 1000,
  NA,
  BW_NXR_Flux_new$`latent_heat_W/m²`
)



# Define the indices of columns to remove keeping just the meteorological variables
columns_to_remove_MET <- c(2:12, 15:18)

# Remove selected columns using negative indexing
BW_NXR_MET <- BW_NXR[, -columns_to_remove_MET]






###   ✓ TCHIZALAMOU_FLUX STATION - (probably add more info in this comment) ----
# Load files and perform operations within the CG-Tch subdir
cg_tch_data <- load_files1(file.path(external_data_dir, "CG-Tch"))
cg_tch_summaries <- perform_operations(cg_tch_data)

# Move the full obs data for cg_tch to a simpler df. Note that this cg_tch_data in the subdir contains other data like the aux met data, ERA data and the hourly, weekly, monthly, yearly data format
CG_TCH <- (cg_tch_data[["FLX_CG-Tch_FLUXNET2015_FULLSET_HH_2006-2009_1-4.csv"]])


# Convert the date column to datetime objects UTC format
CG_TCH$TIMESTAMP_START <- ymd_hm(CG_TCH$TIMESTAMP_START)


# Replace -9999.9 with NA in CG_TCH dataset
CG_TCH[CG_TCH == -9999] <- NA


# select the columns I need
new_CG_TCH_vars <- c("TIMESTAMP_START", "H_F_MDS", "H_F_MDS_QC", "LE_F_MDS", "LE_F_MDS_QC", "NEE_VUT_REF", 
                     "NEE_VUT_REF_QC", "NEE_CUT_REF", "NEE_CUT_REF_QC")

CG_TCH_Flux<- CG_TCH %>% dplyr::select(one_of(new_CG_TCH_vars))
#View(CG_TCH_Flux)


# Create new columns for each variable, retaining values only where QC is 0
CG_TCH_Measured<- CG_TCH_Flux %>%
  mutate(H_F_MDS_Measured = ifelse(H_F_MDS_QC == 0, H_F_MDS, NA),
         LE_F_MDS_Measured = ifelse(LE_F_MDS_QC == 0, LE_F_MDS, NA),
         NEE_VUT_REF_Measured = ifelse(NEE_VUT_REF_QC == 0, NEE_VUT_REF, NA),
         NEE_CUT_REF_Measured = ifelse(NEE_CUT_REF_QC == 0, NEE_CUT_REF, NA))



new_CG_TCH_vars_2 <- c("TIMESTAMP_START", "H_F_MDS_Measured", "LE_F_MDS_Measured", "NEE_VUT_REF_Measured", 
                    "NEE_CUT_REF_Measured")

CG_TCH_Flux_new<- CG_TCH_Measured %>% dplyr::select(one_of(new_CG_TCH_vars_2))
#View(CG_TCH_Flux)




#Change the variable names to uniform names across all sites???

CG_TCH_Flux_new <- CG_TCH_Flux_new %>% rename(`sensible_heat_W/m²` = `H_F_MDS_Measured`, `latent_heat_W/m²` = `LE_F_MDS_Measured`, 
                                              `v_co2_flux_μmol.m-2.s-1` = `NEE_VUT_REF_Measured`, 
                                              `c_co2_flux_μmol.m-2.s-1` = NEE_CUT_REF_Measured)

#View(CG_TCH_Flux_new)




# select the columns I need for meteorological data
new_CG_TCH_met <- c("TIMESTAMP_START", "TA_F_MDS", "TA_F_MDS_QC", "SW_IN_F_MDS", "SW_IN_F_MDS_QC", "LW_IN_ERA", "VPD_F_MDS", "VPD_F_MDS_QC",
                    "P", "WS", "RH")

CG_TCH_MET<- CG_TCH %>% dplyr::select(one_of(new_CG_TCH_met))
#View(CG_TCH_MET)



# Create new columns for each variable, retaining values only where QC is 0
CG_TCH_MET<- CG_TCH_MET %>%
  mutate(TA_F_MDS_Measured = ifelse(TA_F_MDS_QC == 0, TA_F_MDS, NA),
         SW_IN_F_MDS_Measured = ifelse(SW_IN_F_MDS_QC == 0, SW_IN_F_MDS, NA),
         VPD_F_MDS_Measured = ifelse(VPD_F_MDS_QC == 0, VPD_F_MDS, NA))
         






###   ✓ ANKASA_FLUX STATION - (probably add more info in this comment) ----
# Load files and perform operations within the GH-Ank subdir
gh_ank_data <- load_files1(file.path(external_data_dir, "GH-Ank"))
gh_ank_summaries <- perform_operations(gh_ank_data)

# Move the full obs data for gh_ank to a simpler df. Note that this gh_abk_data in the subdir contains other data like the aux met data, ERA data and the hourly, weekly, monthly, yearly data format
GH_ANK <- (gh_ank_data[["FLX_GH-Ank_FLUXNET2015_FULLSET_HH_2011-2014_1-4.csv"]])


# Convert the date column to datetime objects UTC format
GH_ANK$TIMESTAMP_START <- ymd_hm(GH_ANK$TIMESTAMP_START)

# Replace -9999.9 with NA in CG_TCH dataset
GH_ANK[GH_ANK == -9999] <- NA


# select the columns I need
new_GH_ANK_vars <- c("TIMESTAMP_START", "H_F_MDS", "H_F_MDS_QC", "LE_F_MDS", "LE_F_MDS_QC",
                     "NEE_VUT_REF", "NEE_VUT_REF_QC", "NEE_CUT_REF", "NEE_CUT_REF_QC")

GH_ANK_Flux<- GH_ANK%>% dplyr::select(one_of(new_GH_ANK_vars))
#View(GH_ANK_Flux)




# Create new columns for each variable, retaining values only where QC is 0
GH_ANK_Measured<- GH_ANK_Flux %>%
  mutate(H_F_MDS_Measured = ifelse(H_F_MDS_QC == 0, H_F_MDS, NA),
         LE_F_MDS_Measured = ifelse(LE_F_MDS_QC == 0, LE_F_MDS, NA),
         NEE_VUT_REF_Measured = ifelse(NEE_VUT_REF_QC == 0, NEE_VUT_REF, NA),
         NEE_CUT_REF_Measured = ifelse(NEE_CUT_REF_QC == 0, NEE_CUT_REF, NA))




new_GH_ANK_vars_2 <- c("TIMESTAMP_START", "H_F_MDS_Measured", "LE_F_MDS_Measured", "NEE_VUT_REF_Measured", 
                       "NEE_CUT_REF_Measured")

GH_ANK_Flux_new<- GH_ANK_Measured %>% dplyr::select(one_of(new_GH_ANK_vars_2))
#View(CG_TCH_Flux)




#Change the variable names to uniform names across all sites???
GH_ANK_Flux_new <- GH_ANK_Flux_new %>% rename(`sensible_heat_W/m²` = `H_F_MDS_Measured`, `latent_heat_W/m²` = `LE_F_MDS_Measured`, 
                                              `v_co2_flux_μmol.m-2.s-1` = `NEE_VUT_REF_Measured`, 
                                              `c_co2_flux_μmol.m-2.s-1` = NEE_CUT_REF_Measured)





# select the columns I need for meteorological data
new_GH_ANK_met <- c("TIMESTAMP_START", "TA_F_MDS", "TA_F_MDS_QC", "SW_IN_F_MDS", "SW_IN_F_MDS_QC", "VPD_F_MDS", "VPD_F_MDS_QC",
                    "LW_IN_F_MDS", "LW_IN_F_MDS_QC", "PA", "PA_ERA","P", "WS", "RH")


GH_ANK_MET<- GH_ANK %>% dplyr::select(one_of(new_GH_ANK_met))
#View(GH_ANK_MET)


# Create new columns for each variable, retaining values only where QC is 0
GH_ANK_MET <- GH_ANK_MET %>%
  mutate(TA_F_MDS_Measured = ifelse(TA_F_MDS_QC == 0, TA_F_MDS, NA),
         SW_IN_F_MDS_Measured = ifelse(SW_IN_F_MDS_QC == 0, SW_IN_F_MDS, NA),
         VPD_F_MDS_Measured = ifelse(VPD_F_MDS_QC == 0, VPD_F_MDS, NA))






###   ✓ AGOUFOU-G_FLUX STATION - (probably add more info in this comment) ----
# Load files and perform operations within the ML-AgG subdir
ml_agg_data <- load_files1(file.path(external_data_dir, "ML-AgG"))
ml_agg_summaries <- perform_operations(ml_agg_data)

# Merge all data frames in ml_agg_data row-wise
ML_AgG <- bind_rows(ml_agg_data)

# Replace -9999.9 with NA in the dataset
ML_AgG[ML_AgG == -9999.9] <- NA
ML_AgG[ML_AgG == -9999.90000] <- NA


# select the columns I need
new_ML_AgG_vars <- c("date_end_UTC", "Carbon Dioxide Flux", "Latent Heat Flux", "Sensible Heat Flux")

ML_AgG_Flux<- ML_AgG %>% dplyr::select(one_of(new_ML_AgG_vars))
#View(ML_AgG_Flux)


#Change the variable names to uniform names across all sites???
ML_AgG_Flux_new <- ML_AgG_Flux %>% rename(`sensible_heat_W/m²` = `Sensible Heat Flux`, `latent_heat_W/m²` = `Latent Heat Flux`, 
                                          `co2_flux_μmol.m-2.s-1` = `Carbon Dioxide Flux`)

#View(ML_AgG_Flux_new)

# select the columns I need
new_ML_AgG_MET <- c("date_end_UTC", "Air Temperature", "Wind Speed")

ML_AgG_MET<- ML_AgG %>% dplyr::select(one_of(new_ML_AgG_MET))
#View(ML_AgG_MET)

# load Met data gotten from meteorological station from AMMACATCH database 

ml_agg_met_data <- load_files1(file.path(external_data_dir, "ML-AgG- met"))
ml_agg_met_summaries <- perform_operations(ml_agg_met_data)

# Merge all data frames in ml_agg_met_data row-wise
ML_AgG_met_station <- bind_rows(ml_agg_met_data)


# Replace -9999.9 with NA in dataset
ML_AgG_met_station[ML_AgG_met_station == -9999.9] <- NA
ML_AgG_met_station[ML_AgG_met_station == -9999.90000] <- NA


# select the columns I need
new_ML_AgG_met_station <- c("date_end_UTC", "Air Temperature", "Air Pressure", "Incoming Shortwave Radiation")

ML_AgG_met_station<- ML_AgG_met_station %>% dplyr::select(one_of(new_ML_AgG_met_station))
#View(ML_AgG_MET)






###   ✓ WANKAMA-FALLOW_FLUX STATION - (probably add more info in this comment) ----
# Load files and perform operations within the NE-WaF subdir

ne_waf_data <- load_files2(file.path(external_data_dir, "NE-WaF"))
ne_waf_summaries <- perform_operations(ne_waf_data)

# Merge all data frames in ne_waf_data row-wise
NE_WaF <- bind_rows(ne_waf_data)



# Replace -9999.9 with NA in BJ_BIF_MET dataset
NE_WaF[NE_WaF == -9999.9] <- NA
NE_WaF[NE_WaF== -9999.900] <- NA
NE_WaF[NE_WaF== -9999.90000] <- NA


# select the columns I need
new_NE_WaF_vars <- c("date_end_UTC", "Carbon Dioxide Flux", "Latent Heat Flux", "Sensible Heat Flux")

NE_WaF_Flux<- NE_WaF %>% dplyr::select(one_of(new_NE_WaF_vars))
#View(NE_WaF_Flux)


#Change the variable names to uniform names across all sites???
NE_WaF_Flux_new <- NE_WaF_Flux %>% rename(`sensible_heat_W/m²` = `Sensible Heat Flux`, `latent_heat_W/m²` = `Latent Heat Flux`, 
                                          `co2_flux_μmol.m-2.s-1` = `Carbon Dioxide Flux`)
#View(NE_WaF_Flux_new)


# select the columns I need FOR MET
new_NE_WaF_MET <- c("date_end_UTC", "Air Temperature", "Incoming Longwave Radiation", "Incoming Shortwave Radiation", 
                    "Relative Humidity", "Wind Speed")
                    

NE_WaF_MET<- NE_WaF %>% dplyr::select(one_of(new_NE_WaF_MET))
#View(NE_WaF_MET)


# data from Jerome - Jerome sent data and including met, LAI, soil and surface fluxes
NE_WAF_MET <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/O2-/WITH DATA/NE-WAF-MET/NE_WAF_MET.csv")
NE_WAF_MET[NE_WAF_MET == -9999] <- NA

NE_WAF_MET$TIMESTAMP_START <- dmy_hm(NE_WAF_MET$TIMESTAMP_START)


###   ✓ WANKAMA-MILLET_FLUX STATION - (probably add more info in this comment) ----
# Load files and perform operations within the NE-WaM subdir
ne_wam_data <- load_files1(file.path(external_data_dir, "NE-WaM"))
ne_wam_summaries <- perform_operations(ne_wam_data)

# Merge all data frames in ne_waf_data row-wise
NE_WaM <- bind_rows(ne_wam_data)

# Replace -9999.9 with NA in NE_WAM_MET dataset
NE_WaM[NE_WaM == -9999.9] <- NA
NE_WaM[NE_WaM== -9999.9000] <- NA
NE_WaM[NE_WaM== -9999.90000] <- NA



# select the columns I need
new_NE_WaM_vars <- c("date_end_UTC", "Carbon Dioxide Flux", "Latent Heat Flux", "Sensible Heat Flux")

NE_WaM_Flux<- NE_WaM %>% dplyr::select(one_of(new_NE_WaM_vars))
#View(NE_WaM_Flux)


#Change the variable names to uniform names across all sites???
NE_WaM_Flux_new <- NE_WaM_Flux %>% rename(`sensible_heat_W/m²` = `Sensible Heat Flux`, `latent_heat_W/m²` = `Latent Heat Flux`, 
                                          `co2_flux_μmol.m-2.s-1` = `Carbon Dioxide Flux`)

#View(NE_WaM_Flux_new)



# select the columns I need FOR MET
new_NE_WaM_MET <- c("date_end_UTC", "Air Temperature", "Incoming Longwave Radiation", 
                    "Incoming Shortwave Radiation", "Relative Humidity")


NE_WaM_MET<- NE_WaM %>% dplyr::select(one_of(new_NE_WaM_MET))
#View(NE_WaM_MET)



# data from Jerome - Jerome sent data and including met, LAI, soil and surface fluxes
NE_WAM_MET <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/O2-/WITH DATA/NE-WAM-MET/NE_WAM_MET.csv")
NE_WAM_MET[NE_WAM_MET == -9999] <- NA

NE_WAM_MET$TIMESTAMP_START <- dmy_hm(NE_WAM_MET$TIMESTAMP_START)







###   ✓ DEMOKEYA_FLUX STATION - (probably add more info in this comment) ----
# Load files and perform operations within the SD_Dem subdir
sd_dem_data <- load_files1(file.path(external_data_dir, "SD-Dem"))
sd_dem_summaries <- perform_operations(sd_dem_data)

# Move the full obs data for sd_dem to a simpler df. Note that this cg_tch_data in the subdir contains other data like the aux met data, ERA data and the hourly, weekly, monthly, yearly data format
SD_Dem <- (sd_dem_data[["FLX_SD-Dem_FLUXNET2015_FULLSET_HH_2005-2009_2-4.csv"]])


# Convert the date column to datetime objects UTC format
SD_Dem$TIMESTAMP_START <- ymd_hm(SD_Dem$TIMESTAMP_START)


# Replace -9999.9 with NA in SD_DEM dataset
SD_Dem[SD_Dem == -9999] <- NA


# select the columns I need
new_SD_DEM_vars <- c("TIMESTAMP_START", "H_F_MDS", "H_F_MDS_QC", "LE_F_MDS", "LE_F_MDS_QC", "NEE_VUT_REF", 
                     "NEE_VUT_REF_QC", "NEE_CUT_REF", "NEE_CUT_REF_QC")

SD_DEM_Flux<- SD_Dem %>% dplyr::select(one_of(new_SD_DEM_vars))
#View(SD_DEM_Flux)




# Create new columns for each variable, retaining values only where QC is 0
SD_DEM_Measured<- SD_DEM_Flux %>%
  mutate(H_F_MDS_Measured = ifelse(H_F_MDS_QC == 0, H_F_MDS, NA),
         LE_F_MDS_Measured = ifelse(LE_F_MDS_QC == 0, LE_F_MDS, NA),
         NEE_VUT_REF_Measured = ifelse(NEE_VUT_REF_QC == 0, NEE_VUT_REF, NA),
         NEE_CUT_REF_Measured = ifelse(NEE_CUT_REF_QC == 0, NEE_CUT_REF, NA))



new_SD_DEM_vars_2 <- c("TIMESTAMP_START", "H_F_MDS_Measured", "LE_F_MDS_Measured", "NEE_VUT_REF_Measured", 
                       "NEE_CUT_REF_Measured")

SD_DEM_Flux_new<- SD_DEM_Measured %>% dplyr::select(one_of(new_SD_DEM_vars_2))
#View(CG_TCH_Flux)




#Change the variable names to uniform names across all sites???
SD_DEM_Flux_new <- SD_DEM_Flux_new %>% rename(`sensible_heat_W/m²` = `H_F_MDS_Measured`, `latent_heat_W/m²` = `LE_F_MDS_Measured`, 
                                              `v_co2_flux_μmol.m-2.s-1` = `NEE_VUT_REF_Measured`, 
                                              `c_co2_flux_μmol.m-2.s-1` = NEE_CUT_REF_Measured)






# select the columns I need for meteorological data
new_SD_DEM_met <- c("TIMESTAMP_START", "TA_F_MDS", "TA_F_MDS_QC", "SW_IN_F_MDS",
                    "SW_IN_F_MDS_QC", "VPD_F_MDS_QC", "VPD_F_MDS", "PA", "P", "WS", "RH")

SD_DEM_MET<- SD_Dem %>% dplyr::select(one_of(new_SD_DEM_met))
#View(SD_DEM_MET)


# Create new columns for each variable, retaining values only where QC is 0
SD_DEM_MET <- SD_DEM_MET %>%
  mutate(TA_F_MDS_Measured = ifelse(TA_F_MDS_QC == 0, TA_F_MDS, NA),
         SW_IN_F_MDS_Measured = ifelse(SW_IN_F_MDS_QC == 0, SW_IN_F_MDS, NA),
         VPD_F_MDS_Measured = ifelse(VPD_F_MDS_QC == 0, VPD_F_MDS, NA))





###   ✓ DAHRA_FLUX STATION - (probably add more info in this comment) ----
# Load files and perform operations within the SN-Dhr subdir
# this dataset is multisheet and needs to be corrected first before carrying out any operation here
# separate the sheets into 3 files and save as .csv, then load - I have done this, but the time series does not follow accordingly. there is 2010-2013 but no 2014, then 2015-2017, no 2018, then 2019-2020
sn_dhr_data <- load_files1(file.path(external_data_dir, "SN-Dhr"))
sn_dhr_summaries <- perform_operations(sn_dhr_data)

SN_Dhr <- bind_rows(sn_dhr_data)

# changing the column names of some variables 
colnames(SN_Dhr)[colnames(SN_Dhr) == "date [yyyy-mm-dd]"] <- "date_yyyymmdd" 
colnames(SN_Dhr)[colnames(SN_Dhr) == "time [HH:MM]"] <- "time_HHMM"
colnames(SN_Dhr)[colnames(SN_Dhr) == "DOY [ddd.ddd]"] <- "DOY_ddd_ddd"


# Convert the date column to the proper format
SN_Dhr$date_yyyymmdd <- dmy(SN_Dhr$date_yyyymmdd) # Assuming the date is in 'dd/mm/yyyy' format

# Convert the time column (fractional hours) to hours and minutes
SN_Dhr$hours <- floor(SN_Dhr$time_HHMM * 24)
SN_Dhr$minutes <- floor((SN_Dhr$time_HHMM * 24 - SN_Dhr$hours) * 60)

# Create a time string in HH:MM format
SN_Dhr$time_str <- sprintf("%02d:%02d", SN_Dhr$hours, SN_Dhr$minutes)

# Combine date and time into a single datetime column
SN_Dhr$DateTime <- as.POSIXct(paste(SN_Dhr$date_yyyymmdd, SN_Dhr$time_str), format="%Y-%m-%d %H:%M", tz="UTC")

# Reorder columns to ensure DateTime is the 3rd column
SN_Dhr <- SN_Dhr %>% select(date_yyyymmdd, time_HHMM, time_str, DateTime, everything())

#---#---#---#---#---#---
# After this I realized that the time interval is not a half hourly, instead a second/minute is missing. So I am rounding this up
# Adjust time intervals to ensure consistent 30-minute intervals
SN_Dhr$minutes <- round(SN_Dhr$minutes / 30) * 30

# Adjust hours if minutes overflow
SN_Dhr$hours <- SN_Dhr$hours + floor(SN_Dhr$minutes / 60)
SN_Dhr$minutes <- SN_Dhr$minutes %% 60

# Create a time string in HH:MM format
SN_Dhr$time_str2 <- sprintf("%02d:%02d", SN_Dhr$hours, SN_Dhr$minutes)

# Combine date and time into a single datetime column
SN_Dhr$DateTime2 <- as.POSIXct(paste(SN_Dhr$date_yyyymmdd, SN_Dhr$time_str2), format="%Y-%m-%d %H:%M", tz="UTC")

# Reorder columns to ensure DateTime is the 3rd column
SN_Dhr <- SN_Dhr %>% select(date_yyyymmdd, time_HHMM, time_str, time_str2, DateTime, DateTime2, everything())


# Replace -9999.9 with NA in SN_DHR dataset
SN_Dhr[SN_Dhr == -9999] <- NA
SN_Dhr[SN_Dhr == -9999.0000000] <- NA
SN_Dhr[SN_Dhr == -9.999000e+03] <- NA
SN_Dhr[SN_Dhr == -9999.000000] <- NA
SN_Dhr[SN_Dhr == -9999.00000] <- NA
SN_Dhr[SN_Dhr == -9999.0000] <- NA
#SN_Dhr[SN_Dhr == 0.000000000] <- NA
#SN_Dhr[SN_Dhr == 0.000000] <- NA    #CONSIDER PRECIPITATION OBSERVATION.There are times where P was = 0 and this assignment may impact that.



# select the columns I need
new_SN_Dhr_vars <- c("DateTime2", "co2_flux", "LE [W+1m-2]", "H [W+1m-2]")

SN_Dhr_Flux<- SN_Dhr %>% dplyr::select(one_of(new_SN_Dhr_vars))
#View(SN_Dhr_Flux)


#Change the variable names to uniform names across all sites???
SN_Dhr_Flux_new <- SN_Dhr_Flux %>% rename(`sensible_heat_W/m²` = `H [W+1m-2]`, `latent_heat_W/m²` = `LE [W+1m-2]`, 
                                          `co2_flux_μmol.m-2.s-1` = `co2_flux`)
#View(SN_Dhr_Flux_new)


# select the columns I need FOR MET
new_SN_Dhr_MET <- c("DateTime2", "Net radiation (W m-2)", "AirTemp_2.0 M (oC)", 
                    "AirHumidity_2.0M (%)", "Wind_2.0 M (m sec-1)", "Rain (mm)")


SN_Dhr_MET<- SN_Dhr %>% dplyr::select(one_of(new_SN_Dhr_MET))
#View(SN_Dhr_MET)




###   ✓ NIAKHAR_FLUX STATION - (probably add more info in this comment) ----
# Load files and perform operations within the SN-Nkr subdir
sn_nkr_data <- load_files1(file.path(external_data_dir, "SN-Nkr"))
sn_nkr_summaries <- perform_operations(sn_nkr_data)

# Merge all data frames in sn_nkr_data row-wise
SN_Nkr <- bind_rows(sn_nkr_data)

# Replace -9999.9 with NA in SN_NKR dataset
SN_Nkr[SN_Nkr == -9999.9] <- NA
SN_Nkr[SN_Nkr == -9999.90000] <- NA
SN_Nkr[SN_Nkr == -9999.9000] <- NA
SN_Nkr[SN_Nkr == -9999.900] <- NA



# select the columns I need # use the 20m add reason to summary.rmd
new_SN_Nkr_vars <- c("date_end_UTC", "Latent Heat Flux at height 20 m", "Latent Heat Flux at height 4.5 m", 
                     "Net Ecosystem Exchange of CO2 at height 20 m", "Net Ecosystem Exchange of CO2 at height 4.5 m", 
                     "Sensible Heat Flux at height 20 m", "Sensible Heat Flux at height 4.5 m")

SN_Nkr_Flux<- SN_Nkr %>% dplyr::select(one_of(new_SN_Nkr_vars))
#View(SN_Nkr_Flux)


#Change the variable names to uniform names across all sites???
SN_Nkr_Flux_new <- SN_Nkr_Flux %>% rename(`sensible_heat_20m_W/m²` = `Sensible Heat Flux at height 20 m`, 
                                          `sensible_heat_4.5m_W/m²` = `Sensible Heat Flux at height 4.5 m`,
                                          `latent_heat_20m_W/m²` = `Latent Heat Flux at height 20 m`,
                                          `latent_heat_4.5m_W/m²` = `Latent Heat Flux at height 4.5 m`, 
                                          `co2_flux_20m_μmol.m-2.s-1` = `Net Ecosystem Exchange of CO2 at height 20 m`,
                                          `co2_flux_4.5m_μmol.m-2.s-1` = `Net Ecosystem Exchange of CO2 at height 4.5 m`)

#View(SN_Nkr_Flux_new)

# This met data was from another sensor/rain gauge different from the flux tower.this was gotten from
# AMMACATCH

# Load files and perform operations within the SN-Nkr subdir
sn_nkr_met_data <- load_files1(file.path(external_data_dir, "SN-Nkr- met"))
sn_nkr_met_summaries <- perform_operations(sn_nkr_met_data)

# Merge all data frames in sn_nkr_met_data row-wise
SN_Nkr_met <- bind_rows(sn_nkr_met_data)

# Replace -9999.9 with NA in SN_NKR dataset
SN_Nkr_met[SN_Nkr_met == -9999.9] <- NA
SN_Nkr_met[SN_Nkr_met == -9999.90000] <- NA

# select the columns I need # use the 20m add reason to summary.rmd
new_SN_Nkr_met_vars <- c("date_end_UTC", "Air Pressure at height 20 m", "Air Temperature at height 2 m",
                     "Air Temperature at height 20 m", "Precipitation Amount", "Relative Humidity at height 2 m",                          
                     "Relative Humidity at height 20 m", "Wind Speed at height 20 m", 
                     "Wind Speed at height 4.5 m") 



SN_NKR_MET <- SN_Nkr_met %>% dplyr::select(one_of(new_SN_Nkr_met_vars))
#View(SN_Nkr_met_data)







###   ✓ RAGOLA_FLUX STATION - (probably add more info in this comment) ----
# Load files and perform operations within the SN-RAG subdir
sn_rag_data <- load_files1(file.path(external_data_dir, "SN-RAG"))
sn_rag_summaries <- perform_operations(sn_rag_data)

# Merge all data frames in sn_rag_data row-wise
SN_RAG <- bind_rows(sn_rag_data)

# Replace -9999.9 with NA in BJ_BIF_MET dataset
SN_RAG[SN_RAG == -9999.9] <- NA
SN_RAG[SN_RAG == -9999.90000] <- NA
SN_RAG[SN_RAG == -9999.900] <- NA



# select the columns I need
new_SN_RAG_vars <- c("date_end_UTC", "Carbon Dioxide Flux", "Latent Heat Flux", "Sensible Heat Flux")

SN_RAG_Flux<- SN_RAG %>% dplyr::select(one_of(new_SN_RAG_vars))
#View(SN_RAG_Flux)


#Change the variable names to uniform names across all sites???
SN_RAG_Flux_new <- SN_RAG_Flux %>% rename(`sensible_heat_W/m²` = `Sensible Heat Flux`, `latent_heat_W/m²` = `Latent Heat Flux`, 
                                          `co2_flux_μmol.m-2.s-1` = `Carbon Dioxide Flux`)

#View(SN_RAG_Flux_new)

# select the columns I need FOR MET
new_SN_RAG_MET <- c("date_end_UTC", "Wind Speed")


SN_RAG_MET<- SN_RAG %>% dplyr::select(one_of(new_SN_RAG_MET))
#View(SN_RAG_MET)



# This met data was from another sensor/rain gauge different from the flux tower.This was gotten from
# AMMACATCH

# Load files and perform operations within the SN-Nkr subdir
sn_rag_met_data <- load_files1(file.path(external_data_dir, "SN-RAG- met"))
sn_rag_met_summaries <- perform_operations(sn_rag_met_data)

# Merge all data frames in sn_nkr_met_data row-wise
SN_rag_met <- bind_rows(sn_rag_met_data)

# Replace -9999.9 with NA in SN_NKR dataset
SN_rag_met[SN_rag_met == -9999.9] <- NA
SN_rag_met[SN_rag_met == -9999.90000] <- NA

# select the columns I need 
new_SN_rag_met_vars <- c("date_end_UTC", "Air_temperature", "Relative_Humidity") 


SN_rag_met_data <- SN_rag_met %>% dplyr::select(one_of(new_SN_rag_met_vars))
#View(SN_Nkr_met_data)




###   ✓ JINJA_FLUX STATION - (probably add more info in this comment) ----
# Load files and perform operations within the UG-Jin subdir
# The sub dir contains both the met and daily data

ug_jin_data <- load_files3(file.path(external_data_dir, "UG-JIN"))
ug_jin_summaries <- perform_operations(ug_jin_data)

# Move the full obs data for UG_JIN to a simpler df. This include both the flux data and the meteorological data
UG_JIN <- (ug_jin_data[["UG-JinFLX03_01_12.csv"]])
UG_JIN_Met <- (ug_jin_data[["UG-JinMet03_01_12.csv"]])

UG_JIN[UG_JIN == -9999] <- NA
UG_JIN_Met[UG_JIN_Met == -9999] <- NA


# converting the date and time to usable format. first, ensure the date is in the proper format
UG_JIN$date_ddmmyy <- dmy(UG_JIN$date_ddmmyy)


# Function to convert the time format
convert_time <- function(time) {
  time <- sprintf("%04d", time) # Ensure 4 digits with leading zeros if necessary
  hours <- substr(time, 1, nchar(time) - 2)
  minutes <- substr(time, nchar(time) - 1, nchar(time))
  paste(hours, minutes, sep = ":")
}


# Apply the conversion
UG_JIN$time_hoursminutes <- sapply(UG_JIN$time_hoursminutes, convert_time)

# Combine date and time into a single datetime column
UG_JIN$DateTime <- as.POSIXct(paste(UG_JIN$date_ddmmyy, UG_JIN$time_hoursminutes), format="%Y-%m-%d %H:%M", tz="UTC")

# Reorder columns to ensure DateTime is the 3rd column
UG_JIN <- UG_JIN %>% select(1, 2, DateTime, everything())


# Filter the rows where the minute is either 00 or 30
UG_JIN <- UG_JIN %>%
  filter(format(DateTime, "%M:%S") %in% c("00:00", "30:00"))



# select the columns I need
new_UG_JIN_vars <- c("DateTime", "carbon_dioxide", "latent_heat", "sensible_heat")

UG_JIN_Flux<- UG_JIN %>% dplyr::select(one_of(new_UG_JIN_vars))
#View(UG_JIN_Flux)


#Change the variable names to uniform names across all sites???
UG_JIN_Flux_new <- UG_JIN_Flux %>% rename(`sensible_heat_W/m²` = `sensible_heat`, `latent_heat_W/m²` = `latent_heat`, 
                                          `co2_flux_μmol.m-2.s-1` = `carbon_dioxide`)
#View(UG_JIN_Flux_new)


# converting the time and date to usable format. Firstly, ensure the date is in the proper format
UG_JIN_Met$date_ddmmyy <- dmy(UG_JIN_Met$date_ddmmyy)

# Function to convert the time format
convert_time <- function(time) {
  time <- sprintf("%04d", time) # Ensure 4 digits with leading zeros if necessary
  hours <- substr(time, 1, nchar(time) - 2)
  minutes <- substr(time, nchar(time) - 1, nchar(time))
  paste(hours, minutes, sep = ":")
}

# Apply the conversion
UG_JIN_Met$time_hoursminutes <- sapply(UG_JIN_Met$time_hoursminutes, convert_time)

# Combine date and time into a single datetime column
UG_JIN_Met$DateTime <- as.POSIXct(paste(UG_JIN_Met$date_ddmmyy, UG_JIN_Met$time_hoursminutes), format="%Y-%m-%d %H:%M", tz="UTC")

# Reorder columns to ensure DateTime is the 3rd column
UG_JIN_Met <- UG_JIN_Met %>% select(1, 2, DateTime, everything())


# Filter the rows where the minute is either 00 or 30
UG_JIN_Met <- UG_JIN_Met %>%
  filter(format(DateTime, "%M:%S") %in% c("00:00", "30:00"))



# select the columns I need for MET data
new_UG_JIN_Met_vars <- c("DateTime", "precipitation_mm", "global_or_short_wave_incoming_radiation_W_m.2",                
                         "long_wave_incoming_radiation_W_m.2", "air_temperature", 
                         "pressure", "relative_humidity", "Wind_horizontal_speed_m_sec.1")



UG_JIN_MET<- UG_JIN_Met %>% dplyr::select(one_of(new_UG_JIN_Met_vars))
#View(UG_JIN_MET_Data)






###   ✓ CATHEDRAL-PEAK_FLUX STATION - (probably add more info in this comment) ----
# Load files and perform operations within the ZA-XxD-Cathedral subdir
# This data is multisheet and that needs to be fixed.the time series does not also follow because there was a gap year.

# Merge all data frames in cathedral peak row-wise
# Before I bind this, I need to change some variable names to match ✓
# Also, the 2014 data has lots of mismatched variable and not all variables were listed ✓
# I will send an email to the data owners to ask for more info and clarity ✓
# cath <- bind_rows(cathedral_peak_data) ✓


cathedral_peak_data <- load_files4(file.path(external_data_dir, "ZA-Cath"))
cathedral_peak_summaries <- perform_operations(cathedral_peak_data)


# load 2016-date data
cath_2016 <- bind_rows(cathedral_peak_data[["Cath_peak_2016_1.csv"]], 
                    cathedral_peak_data[["Cath_peak_2016_2.csv"]], 
                    cathedral_peak_data[["Cath_peak_2016_3.csv"]], 
                    cathedral_peak_data[["Cath_peak_2016_4.csv"]], 
                    cathedral_peak_data[["Cath_peak_2016_5.csv"]], 
                    cathedral_peak_data[["Cath_peak_2016_6.csv"]], 
                    cathedral_peak_data[["Cath_peak_2016_7.csv"]])


# Convert the date column to datetime objects UTC format
cath_2016$TIMESTAMP <- dmy_hm(cath_2016$TIMESTAMP)


# Replace NaN with NA
cath_2016 <- cath_2016 %>%
  mutate(across(everything(), ~replace(., is.nan(.), NA)))

# Replace Inf and -Inf with NA
cath_2016 <- cath_2016 %>%
  mutate(across(everything(), ~replace(., is.infinite(.), NA)))


# selection of variables for 2016
new_ZA_Cath_vars_2016 <- c("TIMESTAMP", "Fc_molar", "H", "LE", "TA_1_1_1", "RH_1_1_1", "PA", 
                           "wnd_spd", "SW_IN", "LW_IN", "VPD")

ZA_Cath_Flux_2016 <- cath_2016 %>% dplyr::select(one_of(new_ZA_Cath_vars_2016))


#select the variables for flux
ZA_cath_flux_var <- c("TIMESTAMP", "Fc_molar", "H", "LE")
ZA_cath_flux <- ZA_Cath_Flux_2016 %>% dplyr::select(one_of(ZA_cath_flux_var))


#Change the variable names to uniform names across all sites???
ZA_cath_Flux_new <- ZA_cath_flux %>% rename(`sensible_heat_W/m²` = `H`, `latent_heat_W/m²` = `LE`, 
                                            `co2_flux_μmol.m-2.s-1` =`Fc_molar`)



# Replace extreme values with NA in sensible_heat_W/m²
ZA_cath_Flux_new$`sensible_heat_W/m²` <- ifelse(
  ZA_cath_Flux_new$`sensible_heat_W/m²` < -1000 | ZA_cath_Flux_new$`sensible_heat_W/m²` > 1000,
  NA,
  ZA_cath_Flux_new$`sensible_heat_W/m²`
)

# Replace extreme values with NA in latent_heat_W/m²
ZA_cath_Flux_new$`latent_heat_W/m²` <- ifelse(
  ZA_cath_Flux_new$`latent_heat_W/m²` < -1000 | ZA_cath_Flux_new$`latent_heat_W/m²` > 1000,
  NA,
  ZA_cath_Flux_new$`latent_heat_W/m²`
)


# select variables for meteorological data 
new_ZA_Cath_met <- c("TIMESTAMP", "TA_1_1_1", "RH_1_1_1", "PA", "wnd_spd", "SW_IN", "LW_IN", "VPD")
ZA_CATH_MET <- ZA_Cath_Flux_2016 %>% dplyr::select(one_of(new_ZA_Cath_met))







 ### this is for 2014 data. I am no longer combining this data with the above data, as I have decided to consider both data
 ### separately and move forward with the 2016-date data. there might be opportunity to work with the other data in the future or not.
 ### load 2014 data


    cath_2014 <- (cathedral_peak_data[["Cath_peak_2014-2015.csv"]])

    cath_2014$TIMESTAMP <- dmy_hm(cath_2014$TIMESTAMP)


      # Replace NaN with NA
    cath_2014 <- cath_2014 %>%
     mutate(across(everything(), ~replace(., is.nan(.), NA)))

      # Replace Inf and -Inf with NA
    cath_2014 <- cath_2014 %>%
     mutate(across(everything(), ~replace(., is.infinite(.), NA)))


    
    # converting CO₂ flux from mg/(m² s) to µmol/(m² s)
    cath_2014 <- cath_2014 %>%
      mutate(Fc_molar = Fc_wpl * (1000 / 44.01))
    

      # select the columns I need for 2014
    new_ZA_Cath_vars_2014 <- c("TIMESTAMP", "wnd_spd", "Fc_molar", "LE_wpl", 
                           "Hc", "amb_tmpr_Avg", "RH_tmpr_rh_mean")

    ZA_Cath_data_2014 <- cath_2014 %>% dplyr::select(one_of(new_ZA_Cath_vars_2014))
    
      
    
    #Change the variable names to uniform names across all sites???
    ZA_Cath_data_2014 <- ZA_Cath_data_2014 %>% rename(`co2_flux_μmol.m-2.s-1` = `Fc_molar`, `latent_heat_W/m²` = `LE_wpl`, 
                                                      `sensible_heat_W/m²` = `Hc`, TA_1_1_1 = `amb_tmpr_Avg`, 
                                                      RH_1_1_1 = `RH_tmpr_rh_mean`)


    
    #select the variables for flux
    ZA_cath_flux_var_2 <- c("TIMESTAMP", "co2_flux_μmol.m-2.s-1", "sensible_heat_W/m²", "latent_heat_W/m²")
    ZA_Cath_Flux_2014 <- ZA_Cath_data_2014 %>% dplyr::select(one_of(ZA_cath_flux_var_2))
  


    # Replace extreme values with NA in sensible_heat_W/m²
    ZA_Cath_Flux_2014$`sensible_heat_W/m²` <- ifelse(
      ZA_Cath_Flux_2014$`sensible_heat_W/m²` < -1000 | ZA_Cath_Flux_2014$`sensible_heat_W/m²` > 1000,
      NA,
      ZA_Cath_Flux_2014$`sensible_heat_W/m²`
    )
    
    # Replace extreme values with NA in latent_heat_W/m²
    ZA_Cath_Flux_2014$`latent_heat_W/m²` <- ifelse(
      ZA_Cath_Flux_2014$`latent_heat_W/m²` < -1000 | ZA_Cath_Flux_2014$`latent_heat_W/m²` > 1000,
      NA,
      ZA_Cath_Flux_2014$`latent_heat_W/m²`
    )
    
    
    
    #select the variables for met
      ZA_cath_met_var_2 <- c("TIMESTAMP", "wnd_spd", "TA_1_1_1", "RH_1_1_1")
    ZA_CATH_MET_2014 <-  ZA_Cath_data_2014 %>% dplyr::select(one_of(ZA_cath_met_var_2))
    
    
    
    
    
    
    

###   ✓ SKUKUZA_FLUX STATION - (probably add more info in this comment) ----
# Load files and perform operations within the ZA-Kru subdir
za_kru_data <- load_files1(file.path(external_data_dir, "ZA-Kru"))
za_kru_summaries <- perform_operations(za_kru_data)

# Move the full obs data for ZA_Kru to a simpler df. Note that this ZA_kru in the subdir contains other data like the aux met data, ERA data and the hourly, weekly, monthly, yearly data format
ZA_Kru <- (za_kru_data[["FLX_ZA-Kru_FLUXNET2015_FULLSET_HH_2000-2013_1-4.csv"]])


# Convert the date column to datetime objects UTC format
ZA_Kru$TIMESTAMP_START <- ymd_hm(ZA_Kru$TIMESTAMP_START)


# Replace -9999.9 with NA in SD_DEM dataset
ZA_Kru[ZA_Kru== -9999] <- NA


# select the columns I need
new_ZA_Kru_vars <- c("TIMESTAMP_START", "H_F_MDS", "H_F_MDS_QC", "LE_F_MDS", "LE_F_MDS_QC",
                     "NEE_VUT_REF", "NEE_VUT_REF_QC", "NEE_CUT_REF", "NEE_CUT_REF_QC")

ZA_Kru_Flux<- ZA_Kru %>% dplyr::select(one_of(new_ZA_Kru_vars))
#View(ZA_Kru_Flux)


# Create new columns for each variable, retaining values only where QC is 0
ZA_Kru_Measured<- ZA_Kru_Flux %>%
  mutate(H_F_MDS_Measured = ifelse(H_F_MDS_QC == 0, H_F_MDS, NA),
         LE_F_MDS_Measured = ifelse(LE_F_MDS_QC == 0, LE_F_MDS, NA),
         NEE_VUT_REF_Measured = ifelse(NEE_VUT_REF_QC == 0, NEE_VUT_REF, NA),
         NEE_CUT_REF_Measured = ifelse(NEE_CUT_REF_QC == 0, NEE_CUT_REF, NA))



new_ZA_Kru_vars_2 <- c("TIMESTAMP_START", "H_F_MDS_Measured", "LE_F_MDS_Measured", "NEE_VUT_REF_Measured", 
                       "NEE_CUT_REF_Measured")

ZA_Kru_Flux_new<- ZA_Kru_Measured %>% dplyr::select(one_of(new_ZA_Kru_vars_2))
#View(ZA_KRU_Flux)


#Change the variable names to uniform names across all sites???
ZA_Kru_Flux_new <- ZA_Kru_Flux_new %>% rename(`sensible_heat_W/m²` = `H_F_MDS_Measured`, `latent_heat_W/m²` = `LE_F_MDS_Measured`, 
                                              `v_co2_flux_μmol.m-2.s-1` = `NEE_VUT_REF_Measured`, 
                                              `c_co2_flux_μmol.m-2.s-1` = NEE_CUT_REF_Measured)



#View(ZA_KRU_Flux_new)

# select the columns I need for meteorological data
new_ZA_Kru_met <- c("TIMESTAMP_START", "TA_F_MDS", "TA_F_MDS_QC", "SW_IN_F_MDS", "SW_IN_F_MDS_QC",
                    "LW_IN_F_MDS", "LW_IN_F_MDS_QC", "PA", "PA_ERA", "P", "WS", "RH", "VPD_F_MDS", "VPD_F_MDS_QC")

ZA_Kru_MET<- ZA_Kru %>% dplyr::select(one_of(new_ZA_Kru_met))
#View(ZA_Kru_MET)

# Create new columns for each variable, retaining values only where QC is 0
ZA_KRU_MET<- ZA_Kru_MET %>%
  mutate(TA_F_MDS_Measured = ifelse(TA_F_MDS_QC == 0, TA_F_MDS, NA),
         SW_IN_F_MDS_Measured = ifelse(SW_IN_F_MDS_QC == 0, SW_IN_F_MDS, NA),
         LW_IN_F_MDS_Measured = ifelse(LW_IN_F_MDS_QC == 0, LW_IN_F_MDS, NA),
         VPD_F_MDS_Measured = ifelse(VPD_F_MDS_QC == 0, VPD_F_MDS, NA))







###   ✓ WELGEGUND_FLUX STATION - (probably add more info in this comment) ----
# Load files and perform operations within the ZA-Wgn subdir ✓
# there is an issue with this data (part 1 and 2 of 201105), sort this out manually and combine ✓
# I haven't combined this yet because all files (119) are not in particular regular. ✓
# 67 of them have 24 more variables (columns). ✓

za_wgn_data <- load_files5(file.path(external_data_dir, "ZA-Wgn"))
za_wgn_summaries <- perform_operations(za_wgn_data)

# Merge all data frames in sn_rag_data row-wise
ZA_Wgn <- bind_rows(za_wgn_data)


# Replace -9999.9 with NA in ZA_WGN dataset
ZA_Wgn[ZA_Wgn== -999.000] <- NA
ZA_Wgn[ZA_Wgn== -9.99] <- NA


colnames(ZA_Wgn)[colnames(ZA_Wgn) == "period end"] <- "period_end"

# select the columns I need
new_ZA_Wgn_vars <- c("period_end", "CO2 mole flux density: Fs/Mc [umol m-2 s-1 ?]", 
                     "Webb corrected CO2 flux: FSW [mg m-2 s-1]", "latent heat flux: FL [W m-2]", 
                     "sensible heat flux: FH [W m-2]") #CO2 mass flux density: Fs/1000 [mg m-2 s-2]

ZA_Wgn_Flux<- ZA_Wgn %>% dplyr::select(one_of(new_ZA_Wgn_vars))
#View(ZA_Wgn_Flux)


#Change the variable names to uniform names across all sites???
ZA_Wgn_Flux_new <- ZA_Wgn_Flux %>% rename(`sensible_heat_W/m²` = `sensible heat flux: FH [W m-2]`, `latent_heat_W/m²` = `latent heat flux: FL [W m-2]`,
                                          `co2_flux_μmol.m-2.s-1` = `CO2 mole flux density: Fs/Mc [umol m-2 s-1 ?]`)
                                                    



# Remove the row with the specific DateTime value
ZA_Wgn_Flux_new <- ZA_Wgn_Flux_new %>%
  filter(period_end != as.POSIXct("2018-04-15 05:07:00", tz = "UTC"))

# Remove all rows that have duplicate DateTime values
ZA_Wgn_Flux_new <- ZA_Wgn_Flux_new %>%
  group_by(period_end) %>%
  filter(n() == 1) %>%
  ungroup()



# Replace extreme values with NA in sensible_heat_W/m²
ZA_Wgn_Flux_new$`sensible_heat_W/m²` <- ifelse(
  ZA_Wgn_Flux_new$`sensible_heat_W/m²` < -1000 | ZA_Wgn_Flux_new$`sensible_heat_W/m²` > 1000,
  NA,
  ZA_Wgn_Flux_new$`sensible_heat_W/m²`
)

# Replace extreme values with NA in latent_heat_W/m²
ZA_Wgn_Flux_new$`latent_heat_W/m²` <- ifelse(
  ZA_Wgn_Flux_new$`latent_heat_W/m²` < -1000 | ZA_Wgn_Flux_new$`latent_heat_W/m²` > 1000,
  NA,
  ZA_Wgn_Flux_new$`latent_heat_W/m²`
)





# select the columns I need FOR MET that are from the EC instrument
new_ZA_Wgn_MET <-c("period_end", "wind speed [m/s]", "avg(Ts) [C]", "relative humidity: 100*RH [%]", "avg(LiP) [kPa]",
                   "vapour pressure deficit: VPD/100 [hPa]")


ZA_WGN_MET<- ZA_Wgn %>% dplyr::select(one_of(new_ZA_Wgn_MET))
#View(ZA_Wgn_MET)


# For the meteorological dataset sent along this flux data. This is a 15min data and from a different instrumentation from the EC
za_wgn_met_instr <- load_files6(file.path(external_data_dir, "ZA-Wgn- met"))
za_wgn_met_instr_all <- bind_rows(za_wgn_met_instr)

# Define the new column names
new_col_names <- c("date_time", "O_3 [ppb]", "SO_2 [ppb]", "NO [ppb]","NO_x [ppb]",
                   "CO [ppb]", "Global radiation [W/m2]", "Temperature [C]", "RH [%]", "Wind speed [m/s]",
                   "Wind direction [deg]", "Precipitation [mm/h]", "Pressure [hPa]",
                   "Potential temperature gradient [K/m] (T7.5-T2.5)/5 m", "PM1 [µg/m3]", "PM2.5 [µg/m3]",
                   "PM10 [µg/m3]", "Black carbon concentration [µg/m3]", "Black carbon concentration corrected for filter change artefact [µg/m3]",
                   "Scattering 450nm [Mm^-1]", "Scattering 525nm [Mm^-1]", "Scattering 635nm [Mm^-1]",
                   "Global up [W/m2]", "Global down [W/m2]", "PPFD up [umol/m2/s]", "PPFD down [umol/m2/s]", "NET [W/m2]",
                   "soil T -50 profile [C]", "soil T -20 profile [C]", "soil T -5 profile [C]", "soil T -5 single [C]",
                   "Theta -50 profile [0-1] soil moisture", "Theta -20 profile [0-1] soil moisture",
                   "Theta -5 profile [0-1] soil moisture", "Theta -5 single [0-1] soil moisture", "Soil heat flux [w/m]", 
                   "Soil moisture 10cm", "Soil moisture 20cm", "Soil moisture 30cm", "Soil moisture 40cm",
                   "Soil moisture 60cm", "Soil moisture 100cm", "Total radiation [W/m2]", "Diffuse radiation [W/m2]")


# Assign the new column names to the first 44 columns of the dataset
colnames(za_wgn_met_instr_all)[1:44] <- new_col_names

za_wgn_met_instr_all$date_time <- ymd_hms(za_wgn_met_instr_all$date_time)

# Replace -9999.9 with NA in ZA_WGN dataset
za_wgn_met_instr_all[za_wgn_met_instr_all== -999] <- NA
za_wgn_met_instr_all[za_wgn_met_instr_all== -999.000] <- NA
za_wgn_met_instr_all[za_wgn_met_instr_all== -999.0000] <- NA
za_wgn_met_instr_all[za_wgn_met_instr_all== -999.00000] <- NA
za_wgn_met_instr_all[za_wgn_met_instr_all== -999.000000] <- NA


# select the columns I need FOR MET that are from the met station instrument
new_za_Wgn_met_instr_all <-c("date_time", "Global radiation [W/m2]", "RH [%]", "Pressure [hPa]", "Temperature [C]", 
                             "Wind speed [m/s]", "Precipitation [mm/h]", "Global down [W/m2]")


ZA_WGN_MET_instr<- za_wgn_met_instr_all %>% dplyr::select(one_of(new_za_Wgn_met_instr_all))
#View(ZA_WGN_MET_instr)






###   ✓ MONGU_FLUX STATION - (probably add more info in this comment) ----
# Load files and perform operations within the ZM-Mon subdir
zm_mon_data <- load_files1(file.path(external_data_dir, "ZM-Mon"))
zm_mon_summaries <- perform_operations(zm_mon_data)

# Move the full obs data for ZM_mon to a simpler df. Note that this ZM_mon in the subdir contains other data like the aux met data, ERA data and the hourly, weekly, monthly, yearly data format
ZM_Mon <- (zm_mon_data[["FLX_ZM-Mon_FLUXNET2015_FULLSET_HH_2000-2009_2-4.csv"]])


# Convert the date column to datetime objects UTC format
ZM_Mon$TIMESTAMP_START <- ymd_hm(ZM_Mon$TIMESTAMP_START)

# Replace -9999.9 with NA in ZM_Mon dataset
ZM_Mon[ZM_Mon == -9999] <- NA


# select the columns I need
new_ZM_Mon_vars <- c("TIMESTAMP_START", "H_F_MDS", "H_F_MDS_QC", "LE_F_MDS", "LE_F_MDS_QC", "NEE_VUT_REF", 
                     "NEE_VUT_REF_QC", "NEE_CUT_REF", "NEE_CUT_REF_QC")

ZM_Mon_Flux<- ZM_Mon %>% dplyr::select(one_of(new_ZM_Mon_vars))
#View(ZM_Mon_Flux)


# Create new columns for each variable, retaining values only where QC is 0
ZM_Mon_Measured<- ZM_Mon_Flux %>%
  mutate(H_F_MDS_Measured = ifelse(H_F_MDS_QC == 0, H_F_MDS, NA),
         LE_F_MDS_Measured = ifelse(LE_F_MDS_QC == 0, LE_F_MDS, NA),
         NEE_VUT_REF_Measured = ifelse(NEE_VUT_REF_QC == 0, NEE_VUT_REF, NA),
         NEE_CUT_REF_Measured = ifelse(NEE_CUT_REF_QC == 0, NEE_CUT_REF, NA))




new_ZM_Mon_vars_2 <- c("TIMESTAMP_START", "H_F_MDS_Measured", "LE_F_MDS_Measured", "NEE_VUT_REF_Measured", 
                       "NEE_CUT_REF_Measured")

ZM_Mon_Flux_new<- ZM_Mon_Measured %>% dplyr::select(one_of(new_ZM_Mon_vars_2))
#View(CG_TCH_Flux)




#Change the variable names to uniform names across all sites???
ZM_Mon_Flux_new <- ZM_Mon_Flux_new %>% rename(`sensible_heat_W/m²` = `H_F_MDS_Measured`, `latent_heat_W/m²` = `LE_F_MDS_Measured`, 
                                              `v_co2_flux_μmol.m-2.s-1` = `NEE_VUT_REF_Measured`, 
                                              `c_co2_flux_μmol.m-2.s-1` = NEE_CUT_REF_Measured)





# select the columns I need for meteorological data
new_ZM_Mon_met <- c("TIMESTAMP_START", "TA_F_MDS", "TA_F_MDS_QC", "SW_IN_F_MDS", "SW_IN_F_MDS_QC", 
                    "LW_IN_F_MDS", "LW_IN_F_MDS_QC", "PA", "PA_ERA", "P", "WS", "RH", "VPD_F_MDS", "VPD_F_MDS_QC")

ZM_MON_MET<- ZM_Mon %>% dplyr::select(one_of(new_ZM_Mon_met))
#View(ZM_Mon_MET)

# Create new columns for each variable, retaining values only where QC is 0
ZM_MON_MET<- ZM_MON_MET %>%
  mutate(TA_F_MDS_Measured = ifelse(TA_F_MDS_QC == 0, TA_F_MDS, NA),
         SW_IN_F_MDS_Measured = ifelse(SW_IN_F_MDS_QC == 0, SW_IN_F_MDS, NA),
         LW_IN_F_MDS_Measured = ifelse(LW_IN_F_MDS_QC == 0, LW_IN_F_MDS, NA),
         VPD_F_MDS_Measured = ifelse(VPD_F_MDS_QC == 0, VPD_F_MDS, NA))

      
