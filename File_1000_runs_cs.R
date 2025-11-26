# ----------- Packages to load ---------------
library(ncdf4)
library(tidyverse)
library(lubridate)
library(gridExtra)
library(patchwork)
library(dplyr)
library(ggplot2)



# ---- Define some functions for converting JULES NetCDF files to dataframe ----

#' Get start time of NetCDF file.
#'
#' @param ncd Either a netcdf file or an open ncdf4  object
#'
#' @return A POSIXct value defining the start time of the ncd4 dataset.
ncd_start <- function(ncd){
  if (inherits(ncd, "character")){
    ncd <- nc_open(ncd)
    on.exit(nc_close(ncd))
  }
  # get the time units
  time_units <- ncdf4::ncatt_get(ncd, "time_bounds")$units
  
  # message what they are
  message(paste0("The ncdf time units are:\n",
                 time_units))
  
  # extract the start datetime from the units - then use {lubridate} to convert to datetime
  gsub("[^0-9:-]", " ", time_units) |>
    trimws() |>
    ymd_hms()
}

#' Convert the Netcdf to a dataframe with all the contained data.
#'
#' @param ncd a netcdf file
#' @param label optional - default NULL. A character (length 1) to assign a label
#' to all values in the returned tibble
#'
#' @return A tibble with columns
ncd_to_df <- function(ncd, label=NULL) {
  #open the netcdf file - close when the function finishes.
  ncd <- nc_open(ncd)
  on.exit(nc_close(ncd))
  
  
  start_date <- ncd_start(ncd)
  
  # function to convert multi-dimensional vars to dataframe
  multi_dim_to_df <- function(x, .name){
    x <- as_tibble(t(x),
                   .name_repair="unique")
    rename_with(x, ~paste(.name, str_replace(colnames(x), "...", ""), sep="_"))
  }
  
  # main variable converter function calls multi dim function if has > 1 dimension
  vars_to_df <- function(var){
    
    vals <- ncvar_get(ncd, var)
    
    if (length(dim(vals))>1) {
      df <- suppressMessages(multi_dim_to_df(vals, var))
    } else {
      df <- tibble(!!var := (vals))
    }
    return(df)
  }
  
  # which variables to get as columns in the df - remove lat long because these
  # are just single values maybe better as attributes...
  var_names <- setdiff(names(ncd$var), c("latitude", "longitude"))
  
  # this maps the var_names over the function above and combines the results as
  # as a single dataframe
  
  ncd_df <- purrr::map(var_names, vars_to_df) |>
    purrr::reduce(dplyr::bind_cols) |>
    dplyr::mutate(across(c("time_bounds_1", "time_bounds_2"),
                         function(x) start_date + x))
  # adds a character label column - might be useful if merging different data and
  # you need a grouping variable or something.
  if (!is.null(label)){
    ncd_df <- ncd_df |>
      mutate(label=label)
  }
  
  # at lat long as attributes - access them with `attributes(x)`
  attr(ncd_df, "lat") <- ncvar_get(ncd,"latitude")
  attr(ncd_df, "long") <- ncvar_get(ncd,"longitude")
  
  return(ncd_df)
}






# for BW_GUM ----
# ----- load JULES NetCDF as dataframes -----------

# run the above functions to convert ncdf files to dataframes.
ncd_file_BW_GUM <- "C:/Users/efa206/OneDrive - University of Exeter/Desktop/JULES_Output/File_1000/part6_BW_GUM-JULES_vn7.4.D.nc"
def_BW_GUM <- ncd_to_df(ncd_file_BW_GUM, label="BW_GUM")

# now all data matrices in the netcdf file are converted to columns in the dataframe
print(def_BW_GUM)

# extract the lat long values if required.
attributes(def_BW_GUM)$lat
attributes(def_BW_GUM)$long



#plot ----
cs_BW_GUM <- def_BW_GUM %>%
  select(starts_with("cs_"), datetime = time_bounds_1) %>%
  mutate(file = "Part1") %>%
  pivot_longer(cols = starts_with("cs_"), names_to = "layer", values_to = "cs") %>%
  mutate(layer = gsub("cs_", "Layer ", layer))



# Plot
ggplot(cs_BW_GUM, aes(x = datetime, y = cs, color = file)) +
  geom_line() +
  facet_wrap(~layer, scales = "free_y") +
  labs(title = "Soil Carbon (cs) over Time by Soil Layer",
       x = "Datetime",
       y = "Soil Carbon (kg C m⁻²)"
       ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "none")








# run the above functions to convert ncdf files to dataframes.
ncd_file_BW_NXR <- "C:/Users/efa206/OneDrive - University of Exeter/Desktop/JULES_Output/File_1000/part6_BW_NXR-JULES_vn7.4.D.nc"
def_BW_NXR <- ncd_to_df(ncd_file_BW_NXR, label="BW_NXR")

# now all data matrices in the netcdf file are converted to columns in the dataframe
print(def_BW_NXR)

# extract the lat long values if required.
attributes(def_BW_NXR)$lat
attributes(def_BW_NXR)$long
















# for BW_NXR ----
# ----- load JULES NetCDF as dataframes -----------
# run the above functions to convert ncdf files to dataframes.
ncd_file_BW_NXR <- "C:/Users/efa206/OneDrive - University of Exeter/Desktop/JULES_Output/File_1000/part6_BW_NXR-JULES_vn7.4.D.nc"
def_BW_NXR <- ncd_to_df(ncd_file_BW_NXR, label="BW_NXR")

# now all data matrices in the netcdf file are converted to columns in the dataframe
print(def_BW_NXR)

# extract the lat long values if required.
attributes(def_BW_NXR)$lat
attributes(def_BW_NXR)$long



#plot ----
cs_BW_NXR <- def_BW_NXR %>%
  select(starts_with("cs_"), datetime = time_bounds_1) %>%
  mutate(file = "Part1") %>%
  pivot_longer(cols = starts_with("cs_"), names_to = "layer", values_to = "cs") %>%
  mutate(layer = gsub("cs_", "Layer ", layer))



# Plot
ggplot(cs_BW_NXR, aes(x = datetime, y = cs, color = file)) +
  geom_line() +
  facet_wrap(~layer, scales = "free_y") +
  labs(title = "Soil Carbon (cs) over Time by Soil Layer",
       x = "Datetime",
       y = "Soil Carbon (kg C m⁻²)"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "none")






# for CG_TCH ----
# ----- load JULES NetCDF as dataframes -----------
# run the above functions to convert ncdf files to dataframes.
ncd_file_CG_TCH <- "C:/Users/efa206/OneDrive - University of Exeter/Desktop/JULES_Output/File_1000/part6_CG_TCH-JULES_vn7.4.D.nc"
def_CG_TCH <- ncd_to_df(ncd_file_CG_TCH, label="CG_TCH")

# now all data matrices in the netcdf file are converted to columns in the dataframe
print(def_CG_TCH)

# extract the lat long values if required.
attributes(def_CG_TCH)$lat
attributes(def_CG_TCH)$long



#plot ----
cs_CG_TCH <- def_CG_TCH %>%
  select(starts_with("cs_"), datetime = time_bounds_1) %>%
  mutate(file = "Part1") %>%
  pivot_longer(cols = starts_with("cs_"), names_to = "layer", values_to = "cs") %>%
  mutate(layer = gsub("cs_", "Layer ", layer))



# Plot
ggplot(cs_CG_TCH, aes(x = datetime, y = cs, color = file)) +
  geom_line() +
  facet_wrap(~layer, scales = "free_y") +
  labs(title = "Soil Carbon (cs) over Time by Soil Layer",
       x = "Datetime",
       y = "Soil Carbon (kg C m⁻²)"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "none")






# for GH_ANK ----
# ----- load JULES NetCDF as dataframes -----------
# run the above functions to convert ncdf files to dataframes.
ncd_file_GH_ANK <- "C:/Users/efa206/OneDrive - University of Exeter/Desktop/JULES_Output/File_1000/part6_GH_ANK-JULES_vn7.4.D.nc"
def_GH_ANK <- ncd_to_df(ncd_file_GH_ANK, label="GH_ANK")

# now all data matrices in the netcdf file are converted to columns in the dataframe
print(def_GH_ANK)

# extract the lat long values if required.
attributes(def_GH_ANK)$lat
attributes(def_GH_ANK)$long



#plot ----
cs_GH_ANK <- def_GH_ANK %>%
  select(starts_with("cs_"), datetime = time_bounds_1) %>%
  mutate(file = "Part1") %>%
  pivot_longer(cols = starts_with("cs_"), names_to = "layer", values_to = "cs") %>%
  mutate(layer = gsub("cs_", "Layer ", layer))



# Plot
ggplot(cs_GH_ANK, aes(x = datetime, y = cs, color = file)) +
  geom_line() +
  facet_wrap(~layer, scales = "free_y") +
  labs(title = "Soil Carbon (cs) over Time by Soil Layer",
       x = "Datetime",
       y = "Soil Carbon (kg C m⁻²)"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "none")






# for ML_AGG ----
# ----- load JULES NetCDF as dataframes -----------
# run the above functions to convert ncdf files to dataframes.
ncd_file_ML_AGG <- "C:/Users/efa206/OneDrive - University of Exeter/Desktop/JULES_Output/File_1000/part6_ML_AGG-JULES_vn7.4.D.nc"
def_ML_AGG <- ncd_to_df(ncd_file_ML_AGG, label="ML_AGG")

# now all data matrices in the netcdf file are converted to columns in the dataframe
print(def_ML_AGG)

# extract the lat long values if required.
attributes(def_ML_AGG)$lat
attributes(def_ML_AGG)$long



#plot ----
cs_ML_AGG <- def_ML_AGG %>%
  select(starts_with("cs_"), datetime = time_bounds_1) %>%
  mutate(file = "Part1") %>%
  pivot_longer(cols = starts_with("cs_"), names_to = "layer", values_to = "cs") %>%
  mutate(layer = gsub("cs_", "Layer ", layer))



# Plot
ggplot(cs_ML_AGG, aes(x = datetime, y = cs, color = file)) +
  geom_line() +
  facet_wrap(~layer, scales = "free_y") +
  labs(title = "Soil Carbon (cs) over Time by Soil Layer",
       x = "Datetime",
       y = "Soil Carbon (kg C m⁻²)"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "none")






# for NE_WAF ----
# ----- load JULES NetCDF as dataframes -----------
# run the above functions to convert ncdf files to dataframes.
ncd_file_NE_WAF <- "C:/Users/efa206/OneDrive - University of Exeter/Desktop/JULES_Output/File_1000/part6_NE_WAF-JULES_vn7.4.D.nc"
def_NE_WAF <- ncd_to_df(ncd_file_NE_WAF, label="NE_WAF")

# now all data matrices in the netcdf file are converted to columns in the dataframe
print(def_NE_WAF)

# extract the lat long values if required.
attributes(def_NE_WAF)$lat
attributes(def_NE_WAF)$long



#plot ----
cs_NE_WAF <- def_NE_WAF %>%
  select(starts_with("cs_"), datetime = time_bounds_1) %>%
  mutate(file = "Part1") %>%
  pivot_longer(cols = starts_with("cs_"), names_to = "layer", values_to = "cs") %>%
  mutate(layer = gsub("cs_", "Layer ", layer))



# Plot
ggplot(cs_NE_WAF, aes(x = datetime, y = cs, color = file)) +
  geom_line() +
  facet_wrap(~layer, scales = "free_y") +
  labs(title = "Soil Carbon (cs) over Time by Soil Layer",
       x = "Datetime",
       y = "Soil Carbon (kg C m⁻²)"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "none")






# for NE_WAM ----
# ----- load JULES NetCDF as dataframes -----------
# run the above functions to convert ncdf files to dataframes.
ncd_file_NE_WAM <- "C:/Users/efa206/OneDrive - University of Exeter/Desktop/JULES_Output/File_1000/part6_NE_WAM-JULES_vn7.4.D.nc"
def_NE_WAM <- ncd_to_df(ncd_file_NE_WAM, label="NE_WAM")

# now all data matrices in the netcdf file are converted to columns in the dataframe
print(def_NE_WAM)

# extract the lat long values if required.
attributes(def_NE_WAM)$lat
attributes(def_NE_WAM)$long



#plot ----
cs_NE_WAM <- def_NE_WAM %>%
  select(starts_with("cs_"), datetime = time_bounds_1) %>%
  mutate(file = "Part1") %>%
  pivot_longer(cols = starts_with("cs_"), names_to = "layer", values_to = "cs") %>%
  mutate(layer = gsub("cs_", "Layer ", layer))



# Plot
ggplot(cs_NE_WAM, aes(x = datetime, y = cs, color = file)) +
  geom_line() +
  facet_wrap(~layer, scales = "free_y") +
  labs(title = "Soil Carbon (cs) over Time by Soil Layer",
       x = "Datetime",
       y = "Soil Carbon (kg C m⁻²)"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "none")






# for SD_DEM ----
# ----- load JULES NetCDF as dataframes -----------
# run the above functions to convert ncdf files to dataframes.
ncd_file_SD_DEM <- "C:/Users/efa206/OneDrive - University of Exeter/Desktop/JULES_Output/File_1000/part6_SD_DEM-JULES_vn7.4.D.nc"
def_SD_DEM <- ncd_to_df(ncd_file_SD_DEM, label="SD_DEM")

# now all data matrices in the netcdf file are converted to columns in the dataframe
print(def_SD_DEM)

# extract the lat long values if required.
attributes(def_SD_DEM)$lat
attributes(def_SD_DEM)$long



#plot ----
cs_SD_DEM <- def_SD_DEM %>%
  select(starts_with("cs_"), datetime = time_bounds_1) %>%
  mutate(file = "Part1") %>%
  pivot_longer(cols = starts_with("cs_"), names_to = "layer", values_to = "cs") %>%
  mutate(layer = gsub("cs_", "Layer ", layer))



# Plot
ggplot(cs_SD_DEM, aes(x = datetime, y = cs, color = file)) +
  geom_line() +
  facet_wrap(~layer, scales = "free_y") +
  labs(title = "Soil Carbon (cs) over Time by Soil Layer",
       x = "Datetime",
       y = "Soil Carbon (kg C m⁻²)"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "none")








# for SN_DHR ----
# ----- load JULES NetCDF as dataframes -----------
# run the above functions to convert ncdf files to dataframes.
ncd_file_SN_DHR <- "C:/Users/efa206/OneDrive - University of Exeter/Desktop/JULES_Output/File_1000/part6_SN_DHR-JULES_vn7.4.D.nc"
def_SN_DHR <- ncd_to_df(ncd_file_SN_DHR, label="SN_DHR")

# now all data matrices in the netcdf file are converted to columns in the dataframe
print(def_SN_DHR)

# extract the lat long values if required.
attributes(def_SN_DHR)$lat
attributes(def_SN_DHR)$long



#plot ----
cs_SN_DHR <- def_SN_DHR %>%
  select(starts_with("cs_"), datetime = time_bounds_1) %>%
  mutate(file = "Part1") %>%
  pivot_longer(cols = starts_with("cs_"), names_to = "layer", values_to = "cs") %>%
  mutate(layer = gsub("cs_", "Layer ", layer))



# Plot
ggplot(cs_SN_DHR, aes(x = datetime, y = cs, color = file)) +
  geom_line() +
  facet_wrap(~layer, scales = "free_y") +
  labs(title = "Soil Carbon (cs) over Time by Soil Layer",
       x = "Datetime",
       y = "Soil Carbon (kg C m⁻²)"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "none")






# for SN_NKR ----
# ----- load JULES NetCDF as dataframes -----------
# run the above functions to convert ncdf files to dataframes.
ncd_file_SN_NKR <- "C:/Users/efa206/OneDrive - University of Exeter/Desktop/JULES_Output/File_1000/part6_SN_NKR-JULES_vn7.4.D.nc"
def_SN_NKR <- ncd_to_df(ncd_file_SN_NKR, label="SN_NKR")

# now all data matrices in the netcdf file are converted to columns in the dataframe
print(def_SN_NKR)

# extract the lat long values if required.
attributes(def_SN_NKR)$lat
attributes(def_SN_NKR)$long



#plot ----
cs_SN_NKR <- def_SN_NKR %>%
  select(starts_with("cs_"), datetime = time_bounds_1) %>%
  mutate(file = "Part1") %>%
  pivot_longer(cols = starts_with("cs_"), names_to = "layer", values_to = "cs") %>%
  mutate(layer = gsub("cs_", "Layer ", layer))



# Plot
ggplot(cs_SN_NKR, aes(x = datetime, y = cs, color = file)) +
  geom_line() +
  facet_wrap(~layer, scales = "free_y") +
  labs(title = "Soil Carbon (cs) over Time by Soil Layer",
       x = "Datetime",
       y = "Soil Carbon (kg C m⁻²)"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "none")






# for SN_RAG ----
# ----- load JULES NetCDF as dataframes -----------
# run the above functions to convert ncdf files to dataframes.
ncd_file_SN_RAG <- "C:/Users/efa206/OneDrive - University of Exeter/Desktop/JULES_Output/File_1000/part6_SN_RAG-JULES_vn7.4.D.nc"
def_SN_RAG <- ncd_to_df(ncd_file_SN_RAG, label="SN_RAG")

# now all data matrices in the netcdf file are converted to columns in the dataframe
print(def_SN_RAG)

# extract the lat long values if required.
attributes(def_SN_RAG)$lat
attributes(def_SN_RAG)$long



#plot ----
cs_SN_RAG <- def_SN_RAG %>%
  select(starts_with("cs_"), datetime = time_bounds_1) %>%
  mutate(file = "Part1") %>%
  pivot_longer(cols = starts_with("cs_"), names_to = "layer", values_to = "cs") %>%
  mutate(layer = gsub("cs_", "Layer ", layer))



# Plot
ggplot(cs_SN_RAG, aes(x = datetime, y = cs, color = file)) +
  geom_line() +
  facet_wrap(~layer, scales = "free_y") +
  labs(title = "Soil Carbon (cs) over Time by Soil Layer",
       x = "Datetime",
       y = "Soil Carbon (kg C m⁻²)"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "none")






# for UG_JIN ----
# ----- load JULES NetCDF as dataframes -----------
# run the above functions to convert ncdf files to dataframes.
ncd_file_UG_JIN <- "C:/Users/efa206/OneDrive - University of Exeter/Desktop/JULES_Output/File_1000/part6_UG_JIN-JULES_vn7.4.D.nc"
def_UG_JIN <- ncd_to_df(ncd_file_UG_JIN, label="UG_JIN")

# now all data matrices in the netcdf file are converted to columns in the dataframe
print(def_UG_JIN)

# extract the lat long values if required.
attributes(def_UG_JIN)$lat
attributes(def_UG_JIN)$long



#plot ----
cs_UG_JIN <- def_UG_JIN %>%
  select(starts_with("cs_"), datetime = time_bounds_1) %>%
  mutate(file = "Part1") %>%
  pivot_longer(cols = starts_with("cs_"), names_to = "layer", values_to = "cs") %>%
  mutate(layer = gsub("cs_", "Layer ", layer))



# Plot
ggplot(cs_UG_JIN, aes(x = datetime, y = cs, color = file)) +
  geom_line() +
  facet_wrap(~layer, scales = "free_y") +
  labs(title = "Soil Carbon (cs) over Time by Soil Layer",
       x = "Datetime",
       y = "Soil Carbon (kg C m⁻²)"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "none")






# for ZA_CATH_14 ----
# ----- load JULES NetCDF as dataframes -----------
# run the above functions to convert ncdf files to dataframes.
ncd_file_ZA_CATH_14 <- "C:/Users/efa206/OneDrive - University of Exeter/Desktop/JULES_Output/File_1000/part6_ZA_CATH_14-JULES_vn7.4.D.nc"
def_ZA_CATH_14 <- ncd_to_df(ncd_file_ZA_CATH_14, label="ZA_CATH_14")

# now all data matrices in the netcdf file are converted to columns in the dataframe
print(def_ZA_CATH_14)

# extract the lat long values if required.
attributes(def_ZA_CATH_14)$lat
attributes(def_ZA_CATH_14)$long



#plot ----
cs_ZA_CATH_14 <- def_ZA_CATH_14 %>%
  select(starts_with("cs_"), datetime = time_bounds_1) %>%
  mutate(file = "Part1") %>%
  pivot_longer(cols = starts_with("cs_"), names_to = "layer", values_to = "cs") %>%
  mutate(layer = gsub("cs_", "Layer ", layer))



# Plot
ggplot(cs_ZA_CATH_14, aes(x = datetime, y = cs, color = file)) +
  geom_line() +
  facet_wrap(~layer, scales = "free_y") +
  labs(title = "Soil Carbon (cs) over Time by Soil Layer",
       x = "Datetime",
       y = "Soil Carbon (kg C m⁻²)"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "none")






# for ZA_CATH_16 ----
# ----- load JULES NetCDF as dataframes -----------
# run the above functions to convert ncdf files to dataframes.
ncd_file_ZA_CATH_16 <- "C:/Users/efa206/OneDrive - University of Exeter/Desktop/JULES_Output/File_1000/part6_ZA_CATH_16-JULES_vn7.4.D.nc"
def_ZA_CATH_16 <- ncd_to_df(ncd_file_ZA_CATH_16, label="ZA_CATH_16")

# now all data matrices in the netcdf file are converted to columns in the dataframe
print(def_ZA_CATH_16)

# extract the lat long values if required.
attributes(def_ZA_CATH_16)$lat
attributes(def_ZA_CATH_16)$long



#plot ----
cs_ZA_CATH_16 <- def_ZA_CATH_16 %>%
  select(starts_with("cs_"), datetime = time_bounds_1) %>%
  mutate(file = "Part1") %>%
  pivot_longer(cols = starts_with("cs_"), names_to = "layer", values_to = "cs") %>%
  mutate(layer = gsub("cs_", "Layer ", layer))



# Plot
ggplot(cs_ZA_CATH_16, aes(x = datetime, y = cs, color = file)) +
  geom_line() +
  facet_wrap(~layer, scales = "free_y") +
  labs(title = "Soil Carbon (cs) over Time by Soil Layer",
       x = "Datetime",
       y = "Soil Carbon (kg C m⁻²)"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "none")






# for ZA_KRU ----
# ----- load JULES NetCDF as dataframes -----------
# run the above functions to convert ncdf files to dataframes.
ncd_file_ZA_KRU <- "C:/Users/efa206/OneDrive - University of Exeter/Desktop/JULES_Output/File_1000/part6_ZA_KRU-JULES_vn7.4.D.nc"
def_ZA_KRU <- ncd_to_df(ncd_file_ZA_KRU, label="ZA_KRU")

# now all data matrices in the netcdf file are converted to columns in the dataframe
print(def_ZA_KRU)

# extract the lat long values if required.
attributes(def_ZA_KRU)$lat
attributes(def_ZA_KRU)$long



#plot ----
cs_ZA_KRU <- def_ZA_KRU %>%
  select(starts_with("cs_"), datetime = time_bounds_1) %>%
  mutate(file = "Part1") %>%
  pivot_longer(cols = starts_with("cs_"), names_to = "layer", values_to = "cs") %>%
  mutate(layer = gsub("cs_", "Layer ", layer))



# Plot
ggplot(cs_ZA_KRU, aes(x = datetime, y = cs, color = file)) +
  geom_line() +
  facet_wrap(~layer, scales = "free_y") +
  labs(title = "Soil Carbon (cs) over Time by Soil Layer",
       x = "Datetime",
       y = "Soil Carbon (kg C m⁻²)"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "none")






# for ZA_WGN ----
# ----- load JULES NetCDF as dataframes -----------
# run the above functions to convert ncdf files to dataframes.
ncd_file_ZA_WGN <- "C:/Users/efa206/OneDrive - University of Exeter/Desktop/JULES_Output/File_1000/part6_ZA_WGN-JULES_vn7.4.D.nc"
def_ZA_WGN <- ncd_to_df(ncd_file_ZA_WGN, label="ZA_WGN")

# now all data matrices in the netcdf file are converted to columns in the dataframe
print(def_ZA_WGN)

# extract the lat long values if required.
attributes(def_ZA_WGN)$lat
attributes(def_ZA_WGN)$long



#plot ----
cs_ZA_WGN <- def_ZA_WGN %>%
  select(starts_with("cs_"), datetime = time_bounds_1) %>%
  mutate(file = "Part1") %>%
  pivot_longer(cols = starts_with("cs_"), names_to = "layer", values_to = "cs") %>%
  mutate(layer = gsub("cs_", "Layer ", layer))



# Plot
ggplot(cs_ZA_WGN, aes(x = datetime, y = cs, color = file)) +
  geom_line() +
  facet_wrap(~layer, scales = "free_y") +
  labs(title = "Soil Carbon (cs) over Time by Soil Layer",
       x = "Datetime",
       y = "Soil Carbon (kg C m⁻²)"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "none")






# for ZM_MON ----
# ----- load JULES NetCDF as dataframes -----------
# run the above functions to convert ncdf files to dataframes.
ncd_file_ZM_MON <- "C:/Users/efa206/OneDrive - University of Exeter/Desktop/JULES_Output/File_1000/part6_ZM_MON-JULES_vn7.4.D.nc"
def_ZM_MON <- ncd_to_df(ncd_file_ZM_MON, label="ZM_MON")

# now all data matrices in the netcdf file are converted to columns in the dataframe
print(def_ZM_MON)

# extract the lat long values if required.
attributes(def_ZM_MON)$lat
attributes(def_ZM_MON)$long



#plot ----
cs_ZM_MON <- def_ZM_MON %>%
  select(starts_with("cs_"), datetime = time_bounds_1) %>%
  mutate(file = "Part1") %>%
  pivot_longer(cols = starts_with("cs_"), names_to = "layer", values_to = "cs") %>%
  mutate(layer = gsub("cs_", "Layer ", layer))



# Plot
ggplot(cs_ZM_MON, aes(x = datetime, y = cs, color = file)) +
  geom_line() +
  facet_wrap(~layer, scales = "free_y") +
  labs(title = "Soil Carbon (cs) over Time by Soil Layer",
       x = "Datetime",
       y = "Soil Carbon (kg C m⁻²)"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "none")


