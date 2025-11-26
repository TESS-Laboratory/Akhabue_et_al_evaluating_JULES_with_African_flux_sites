# load library ----
library(tidyverse)
library(tidyr)
library(magrittr)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)


# change column name
# change datetime format
# do conversion
# interpolation - half hourly
# plot interpolated and original
# calculate VPD


# for BJ_BEF  ----



# for BW_GUM  ----

BW_GUM_Solarad <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/SolarRad/ERA5_export_BW_GUM_SSR.csv")
# Renaming columns in the BW_GUM_Solar_rad data frame
colnames(BW_GUM_Solarad)[colnames(BW_GUM_Solarad) == "system.index"] <- "DateTime"
colnames(BW_GUM_Solarad)[colnames(BW_GUM_Solarad) == "mean"] <- "ERA5_SSR_J/M2"



BW_GUM_Dewpoint <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Dewpoint/ERA5_export_dewpoint_BW_GUM.csv")
#Renaming columns in the BW_GUM_dew_point data frame
colnames(BW_GUM_Dewpoint)[colnames(BW_GUM_Dewpoint) == "system.index"] <- "DateTime"
colnames(BW_GUM_Dewpoint)[colnames(BW_GUM_Dewpoint) == "mean"] <- "ERA5_Dewpoint_temp_K"



BW_GUM_Temp <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Temp/ERA5_export_temp_BW_GUM.csv")
# Renaming columns in the BW_GUM_Temp data frame
colnames(BW_GUM_Temp)[colnames(BW_GUM_Temp) == "system.index"] <- "DateTime"
colnames(BW_GUM_Temp)[colnames(BW_GUM_Temp) == "mean"] <- "ERA5_Temp_K"



BW_GUM_LWR <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/ThermalRad/ERA5_export_BW_GUM_LWR.csv")
# Renaming columns in the BW_GUM_Temp data frame
colnames(BW_GUM_LWR)[colnames(BW_GUM_LWR) == "system.index"] <- "DateTime"
colnames(BW_GUM_LWR)[colnames(BW_GUM_LWR) == "mean"] <- "ERA5_LWR_J/M2"



BW_GUM_Pressure <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Pressure/ERA5_export_BW_GUM_Pressure.csv")
# Renaming columns in the BW_GUM_Temp data frame
colnames(BW_GUM_Pressure)[colnames(BW_GUM_Pressure) == "system.index"] <- "DateTime"
colnames(BW_GUM_Pressure)[colnames(BW_GUM_Pressure) == "mean"] <- "ERA5_Pressure_Pa"



BW_GUM_U_Wind <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/U_Wind/ERA5_export_BW_GUM_U_Wind.csv")
# Renaming columns in the BW_GUM_Temp data frame
colnames(BW_GUM_U_Wind)[colnames(BW_GUM_U_Wind) == "system.index"] <- "DateTime"
colnames(BW_GUM_U_Wind)[colnames(BW_GUM_U_Wind) == "mean"] <- "ERA5_U_Wind_m/s"



BW_GUM_V_Wind <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/V_Wind/ERA5_export_BW_GUM_V_Wind.csv")
# Renaming columns in the BW_GUM_Temp data frame
colnames(BW_GUM_V_Wind)[colnames(BW_GUM_V_Wind) == "system.index"] <- "DateTime"
colnames(BW_GUM_V_Wind)[colnames(BW_GUM_V_Wind) == "mean"] <- "ERA5_V_Wind_m/s"



BW_GUM_Precip <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Rainfall/ERA5_export_BW_GUM_Precipitation.csv")
# Renaming columns in the BW_GUM_Temp data frame
colnames(BW_GUM_Precip)[colnames(BW_GUM_Precip) == "system.index"] <- "DateTime"
colnames(BW_GUM_Precip)[colnames(BW_GUM_Precip) == "mean"] <- "ERA5_Precip_m"




# List of datasets to combine
datasets <- list(BW_GUM_Solarad, BW_GUM_LWR, BW_GUM_Temp, BW_GUM_Dewpoint, BW_GUM_Pressure, BW_GUM_Precip, BW_GUM_U_Wind, BW_GUM_V_Wind)

# Combine all datasets in the list by "DateTime"
BW_GUM_Combined <- Reduce(function(x, y) merge(x, y, by = "DateTime", all = TRUE), datasets)


# Select columns you want to keep
BW_GUM_ERA5_MET <- BW_GUM_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
BW_GUM_ERA5_MET$DateTime <- as.POSIXct(substr(BW_GUM_ERA5_MET$DateTime, 1, 15), 
                                       format = "%Y%m%dT%H", tz = "UTC")





#Convert variables to appropriate units 

#Convert ERA5_SSR_J/M2 from J/m² to W/m²
BW_GUM_ERA5_MET$`ERA5_SSR_W/M2` <- BW_GUM_ERA5_MET$`ERA5_SSR_J/M2` / 3600


#Convert ERA5_LWR_J/M2 from J/m² to W/m²
BW_GUM_ERA5_MET$`ERA5_LWR_W/M2` <- BW_GUM_ERA5_MET$`ERA5_LWR_J/M2` / 3600


# Convert ERA5_Dewpoint_temp_K from Kelvin to Celsius
BW_GUM_ERA5_MET$ERA5_Dewpoint_temp_C <- BW_GUM_ERA5_MET$ERA5_Dewpoint_temp_K - 273.15


# Convert ERA5_Temp_K from Kelvin to Celsius
BW_GUM_ERA5_MET$ERA5_Temp_C <- BW_GUM_ERA5_MET$ERA5_Temp_K - 273.15


# Convert from m to mm
BW_GUM_ERA5_MET$ERA5_Precip_mm <- BW_GUM_ERA5_MET$ERA5_Precip_m * 1000


# convert from mm to kg m-2 s-1
BW_GUM_ERA5_MET$`ERA5_Precip_kg/m2/s` <- BW_GUM_ERA5_MET$ERA5_Precip_mm / 3600


# Calculate wind speed using the U & V components
BW_GUM_ERA5_MET$`ERA5_Wind_Speed_m/s` <- sqrt(BW_GUM_ERA5_MET$`ERA5_U_Wind_m/s`^2 + BW_GUM_ERA5_MET$`ERA5_V_Wind_m/s`^2)



# Convert Pressure from Pa to hPa
BW_GUM_ERA5_MET <- BW_GUM_ERA5_MET %>%
  mutate(ERA5_Pressure_hPa = ERA5_Pressure_Pa / 100)

# Calculate Actual Vapor Pressure (e) in hPa using Dew Point Temperature in Celsius
BW_GUM_ERA5_MET <- BW_GUM_ERA5_MET %>%
  mutate(e_hPa = 6.112 * exp((17.67 * ERA5_Dewpoint_temp_C) / (ERA5_Dewpoint_temp_C + 243.5)))

# Calculate Specific Humidity (q) in kg/kg
BW_GUM_ERA5_MET <- BW_GUM_ERA5_MET %>%
  mutate(`ERA5_Specific_humidity_kg/kg` = (0.622 * e_hPa) / (ERA5_Pressure_hPa - (0.378 * e_hPa)))





# Interpolate data to get half hourly data'

# Step 1: Generate half-hourly timestamps
time_half_hourly <- seq(from = min(BW_GUM_ERA5_MET$DateTime), 
                        to = max(BW_GUM_ERA5_MET$DateTime), by = "30 mins")

# Step 2: Interpolate each variable to the half-hourly time step

# For Temperature (C)
temperature_half_hourly <- approx(x = BW_GUM_ERA5_MET$DateTime, 
                                  y = BW_GUM_ERA5_MET$ERA5_Temp_C, 
                                  xout = time_half_hourly)$y


# For Temperature (K)
temperature_K_half_hourly <- approx(x = BW_GUM_ERA5_MET$DateTime, 
                                    y = BW_GUM_ERA5_MET$ERA5_Temp_K, 
                                    xout = time_half_hourly)$y

# For Dewpoint Temperature 
dewpoint_half_hourly <- approx(x = BW_GUM_ERA5_MET$DateTime, 
                               y = BW_GUM_ERA5_MET$ERA5_Dewpoint_temp_C, 
                               xout = time_half_hourly)$y

# For Solar Surface Radiation (ERAS_SSR_W/M2)
ssr_half_hourly <- approx(x = BW_GUM_ERA5_MET$DateTime, 
                          y = BW_GUM_ERA5_MET$`ERA5_SSR_W/M2`, 
                          xout = time_half_hourly)$y

# For thermal radiation (ERA5_LWR_W/M2)
lwr_half_hourly <- approx(x = BW_GUM_ERA5_MET$DateTime,
                          y = BW_GUM_ERA5_MET$`ERA5_LWR_W/M2`,
                          xout = time_half_hourly)$y

# For wind speed
wndspd_half_hourly <- approx(x = BW_GUM_ERA5_MET$DateTime,
                             y = BW_GUM_ERA5_MET$`ERA5_Wind_Speed_m/s`,
                             xout = time_half_hourly)$y

# For pressure (Pa)
pressure_Pa_half_hourly <- approx(x = BW_GUM_ERA5_MET$DateTime,
                                  y = BW_GUM_ERA5_MET$`ERA5_Pressure_Pa`,
                                  xout = time_half_hourly)$y

# For pressure (hpa)
pressure_hPa_half_hourly <- approx(x = BW_GUM_ERA5_MET$DateTime,
                                   y = BW_GUM_ERA5_MET$`ERA5_Pressure_hPa`,
                                   xout = time_half_hourly)$y

# For specific humidity
specificHumidity_half_hourly <- approx(x = BW_GUM_ERA5_MET$DateTime,
                                       y = BW_GUM_ERA5_MET$`ERA5_Specific_humidity_kg/kg`,
                                       xout = time_half_hourly)$y


# For AVP
e_hpa_half_hourly <- approx(x = BW_GUM_ERA5_MET$DateTime,
                            y = BW_GUM_ERA5_MET$e_hPa,
                            xout = time_half_hourly)$y


# For precipitation
precipitation_mm_half_hourly <- approx(x = BW_GUM_ERA5_MET$DateTime,
                                       y = BW_GUM_ERA5_MET$ERA5_Precip_mm,
                                       xout = time_half_hourly)$y


# For precipitation kg/m2/s
precipitation_kg_half_hourly <- approx(x = BW_GUM_ERA5_MET$DateTime,
                                       y = BW_GUM_ERA5_MET$`ERA5_Precip_kg/m2/s`,
                                       xout = time_half_hourly)$y


# Step 3: Combine the interpolated data into a new dataframe
BW_GUM_ERA5_MET_HH <- data.frame(
  DateTime = time_half_hourly,
  ERA5_Temp_C = temperature_half_hourly,
  ERA5_Temp_K = temperature_K_half_hourly,
  ERA5_Dewpoint_temp_C = dewpoint_half_hourly,
  `ERA5_SSR_W/M2` = ssr_half_hourly,
  `ERA5_LWR_W/M2` = lwr_half_hourly,
  `ERA5_Specific_humidity_kg/kg` = specificHumidity_half_hourly,
  `ERA5_Pressure_Pa` = pressure_Pa_half_hourly,
  `ERA5_Pressure_hPa` = pressure_hPa_half_hourly,
  `ERA5_Wind_Speed_m/s` = wndspd_half_hourly,
  ERA5_Precip_mm = precipitation_mm_half_hourly,
  `ERA5_Precip_kg/m2/s`= precipitation_kg_half_hourly
)





# Step 4: plot hourly vs interpolated for QC

ggplot() +
  geom_line(data = BW_GUM_ERA5_MET, aes(x = DateTime, y = ERA5_Temp_C), color = "red", size = 1.2) +
  geom_line(data = BW_GUM_ERA5_MET_HH, aes(x = DateTime, y = ERA5_Temp_C), color = "blue", linetype = "dashed", size = 1) +
  labs(title = "Hourly vs Half-Hourly Interpolated Temperature",
       x = "Date", y = "Temperature (C)") +
  theme_minimal()



# Subset the data for a specific time range that exists in the dataset (e.g., one day)
subset_data_hourly <- subset(BW_GUM_ERA5_MET, DateTime >= as.POSIXct("2018-11-12 00:00:00") & DateTime <= as.POSIXct("2018-11-13 00:00:00"))
subset_data_half_hourly <- subset(BW_GUM_ERA5_MET_HH, DateTime >= as.POSIXct("2018-11-12 00:00:00") & DateTime <= as.POSIXct("2018-11-13 00:00:00"))

# Plot the subsetted data
ggplot() +
  geom_point(data = subset_data_hourly, aes(x = DateTime, y = ERA5_Temp_C), color = "red", size = 1.5) +
  geom_point(data = subset_data_half_hourly, aes(x = DateTime, y = ERA5_Temp_C), color = "blue", size = 1) +
  labs(title = "Hourly vs Half-Hourly Interpolated Temperature (Zoomed In)",
       x = "Time", y = "Temperature (°C)") +
  theme_minimal()






# Calculating VPD using land air temperature and dew point temperature 
# Constants
Pmsl <- 1013.25  # mean sea level pressure in hPa
Z_m <- 981.66    # altitude in meters (as provided)

# Calculate VPD for ERA5-Land
calculate_vpd <- function(temp_C, dewpoint_temp_C, Z_m) {
  # Calculate Pmst (equation 6)
  Pmst <- Pmsl * ((temp_C + 273.16) / (temp_C + 273.16 + 0.0065 * Z_m))^5.625
  
  # Corrected formula for fw (equation 5)
  fw <- 1 + 7 * 10^(-4) + 3.46 * 10^(-6) * Pmst
  
  # Calculate SVP (equation 3)
  SVP <- 6.112 * fw * exp((17.67 * temp_C) / (temp_C + 243.5))
  
  # Calculate AVP (equation 4)
  AVP <- 6.112 * fw * exp((17.67 * dewpoint_temp_C) / (dewpoint_temp_C + 243.5))
  
  # Calculate VPD (equation 2)
  VPD <- SVP - AVP
  
  return(VPD)
}

# Apply VPD calculation to the dataset
BW_GUM_ERA5_MET_HH <- BW_GUM_ERA5_MET_HH %>%
  mutate(VPD_hPa = calculate_vpd(ERA5_Temp_C, ERA5_Dewpoint_temp_C, Z_m))







# for BW_NXR  ----

BW_NXR_Solarad <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/SolarRad/ERA5_export_BW_NXR_SSR.csv")
# Renaming columns in the BW_NXR_Solar_rad data frame
colnames(BW_NXR_Solarad)[colnames(BW_NXR_Solarad) == "system.index"] <- "DateTime"
colnames(BW_NXR_Solarad)[colnames(BW_NXR_Solarad) == "mean"] <- "ERA5_SSR_J/M2"



BW_NXR_Dewpoint <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Dewpoint/ERA5_export_dewpoint_BW_NXR.csv")
#Renaming columns in the BW_NXR_dew_point data frame
colnames(BW_NXR_Dewpoint)[colnames(BW_NXR_Dewpoint) == "system.index"] <- "DateTime"
colnames(BW_NXR_Dewpoint)[colnames(BW_NXR_Dewpoint) == "mean"] <- "ERA5_Dewpoint_temp_K"



BW_NXR_Temp <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Temp/ERA5_export_temp_BW_NXR.csv")
# Renaming columns in the BW_NXR_Temp data frame
colnames(BW_NXR_Temp)[colnames(BW_NXR_Temp) == "system.index"] <- "DateTime"
colnames(BW_NXR_Temp)[colnames(BW_NXR_Temp) == "mean"] <- "ERA5_Temp_K"



BW_NXR_LWR <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/ThermalRad/ERA5_export_BW_NXR_LWR.csv")
# Renaming columns in the BW_NXR_Temp data frame
colnames(BW_NXR_LWR)[colnames(BW_NXR_LWR) == "system.index"] <- "DateTime"
colnames(BW_NXR_LWR)[colnames(BW_NXR_LWR) == "mean"] <- "ERA5_LWR_J/M2"



BW_NXR_Pressure <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Pressure/ERA5_export_BW_NXR_Pressure.csv")
# Renaming columns in the BW_NXR_Temp data frame
colnames(BW_NXR_Pressure)[colnames(BW_NXR_Pressure) == "system.index"] <- "DateTime"
colnames(BW_NXR_Pressure)[colnames(BW_NXR_Pressure) == "mean"] <- "ERA5_Pressure_Pa"



BW_NXR_U_Wind <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/U_Wind/ERA5_export_BW_NXR_U_Wind.csv")
# Renaming columns in the BW_NXR_Temp data frame
colnames(BW_NXR_U_Wind)[colnames(BW_NXR_U_Wind) == "system.index"] <- "DateTime"
colnames(BW_NXR_U_Wind)[colnames(BW_NXR_U_Wind) == "mean"] <- "ERA5_U_Wind_m/s"



BW_NXR_V_Wind <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/V_Wind/ERA5_export_BW_NXR_V_Wind.csv")
# Renaming columns in the BW_NXR_Temp data frame
colnames(BW_NXR_V_Wind)[colnames(BW_NXR_V_Wind) == "system.index"] <- "DateTime"
colnames(BW_NXR_V_Wind)[colnames(BW_NXR_V_Wind) == "mean"] <- "ERA5_V_Wind_m/s"


BW_NXR_Precip <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Rainfall/ERA5_export_BW_NXR_Precipitation.csv")
# Renaming columns in the BW_NXR_Temp data frame
colnames(BW_NXR_Precip)[colnames(BW_NXR_Precip) == "system.index"] <- "DateTime"
colnames(BW_NXR_Precip)[colnames(BW_NXR_Precip) == "mean"] <- "ERA5_Precip_m"


# List of datasets to combine
datasets <- list(BW_NXR_Solarad, BW_NXR_LWR, BW_NXR_Temp, BW_NXR_Dewpoint, BW_NXR_Pressure, BW_NXR_Precip, BW_NXR_U_Wind, BW_NXR_V_Wind)

# Combine all datasets in the list by "DateTime"
BW_NXR_Combined <- Reduce(function(x, y) merge(x, y, by = "DateTime", all = TRUE), datasets)



# Select columns you want to keep
BW_NXR_ERA5_MET <- BW_NXR_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
BW_NXR_ERA5_MET$DateTime <- as.POSIXct(substr(BW_NXR_ERA5_MET$DateTime, 1, 15), 
                                       format = "%Y%m%dT%H", tz = "UTC")





#Convert variables to appropriate units 

#Convert ERA5_SSR_J/M2 from J/m² to W/m²
BW_NXR_ERA5_MET$`ERA5_SSR_W/M2` <- BW_NXR_ERA5_MET$`ERA5_SSR_J/M2` / 3600


#Convert ERA5_LWR_J/M2 from J/m² to W/m²
BW_NXR_ERA5_MET$`ERA5_LWR_W/M2` <- BW_NXR_ERA5_MET$`ERA5_LWR_J/M2` / 3600


# Convert ERA5_Dewpoint_temp_K from Kelvin to Celsius
BW_NXR_ERA5_MET$ERA5_Dewpoint_temp_C <- BW_NXR_ERA5_MET$ERA5_Dewpoint_temp_K - 273.15


# Convert ERA5_Temp_K from Kelvin to Celsius
BW_NXR_ERA5_MET$ERA5_Temp_C <- BW_NXR_ERA5_MET$ERA5_Temp_K - 273.15


# Convert from m to mm
BW_NXR_ERA5_MET$ERA5_Precip_mm <- BW_NXR_ERA5_MET$ERA5_Precip_m * 1000


# convert from mm to kg m-2 s-1
BW_NXR_ERA5_MET$`ERA5_Precip_kg/m2/s` <- BW_NXR_ERA5_MET$ERA5_Precip_mm / 3600


# Calculate wind speed using the U & V components
BW_NXR_ERA5_MET$`ERA5_Wind_Speed_m/s` <- sqrt(BW_NXR_ERA5_MET$`ERA5_U_Wind_m/s`^2 + BW_NXR_ERA5_MET$`ERA5_V_Wind_m/s`^2)




# Convert Pressure from Pa to hPa
BW_NXR_ERA5_MET <- BW_NXR_ERA5_MET %>%
  mutate(ERA5_Pressure_hPa = ERA5_Pressure_Pa / 100)

# Calculate Actual Vapor Pressure (e) in hPa using Dew Point Temperature in Celsius
BW_NXR_ERA5_MET <- BW_NXR_ERA5_MET %>%
  mutate(e_hPa = 6.112 * exp((17.67 * ERA5_Dewpoint_temp_C) / (ERA5_Dewpoint_temp_C + 243.5)))

# Calculate Specific Humidity (q) in kg/kg
BW_NXR_ERA5_MET <- BW_NXR_ERA5_MET %>%
  mutate(`ERA5_Specific_humidity_kg/kg` = (0.622 * e_hPa) / (ERA5_Pressure_hPa - (0.378 * e_hPa)))






# Interpolate data to get half hourly data'

# Step 1: Generate half-hourly timestamps
time_half_hourly <- seq(from = min(BW_NXR_ERA5_MET$DateTime), 
                        to = max(BW_NXR_ERA5_MET$DateTime), by = "30 mins")

# Step 2: Interpolate each variable to the half-hourly time step

# For Temperature (C)
temperature_half_hourly <- approx(x = BW_NXR_ERA5_MET$DateTime, 
                                  y = BW_NXR_ERA5_MET$ERA5_Temp_C, 
                                  xout = time_half_hourly)$y

# For Temperature (K)
temperature_K_half_hourly <- approx(x = BW_NXR_ERA5_MET$DateTime, 
                                    y = BW_NXR_ERA5_MET$ERA5_Temp_K, 
                                    xout = time_half_hourly)$y

# For Dewpoint Temperature 
dewpoint_half_hourly <- approx(x = BW_NXR_ERA5_MET$DateTime, 
                               y = BW_NXR_ERA5_MET$ERA5_Dewpoint_temp_C, 
                               xout = time_half_hourly)$y

# For Solar Surface Radiation (ERAS_SSR_W/M2)
ssr_half_hourly <- approx(x = BW_NXR_ERA5_MET$DateTime, 
                          y = BW_NXR_ERA5_MET$`ERA5_SSR_W/M2`, 
                          xout = time_half_hourly)$y

# For thermal radiation (ERA5_LWR_W/M2)
lwr_half_hourly <- approx(x = BW_NXR_ERA5_MET$DateTime,
                          y = BW_NXR_ERA5_MET$`ERA5_LWR_W/M2`,
                          xout = time_half_hourly)$y

# For wind speed
wndspd_half_hourly <- approx(x = BW_NXR_ERA5_MET$DateTime,
                             y = BW_NXR_ERA5_MET$`ERA5_Wind_Speed_m/s`,
                             xout = time_half_hourly)$y

# For pressure (Pa)
pressure_Pa_half_hourly <- approx(x = BW_NXR_ERA5_MET$DateTime,
                                  y = BW_NXR_ERA5_MET$`ERA5_Pressure_Pa`,
                                  xout = time_half_hourly)$y

# For pressure (hpa)
pressure_hPa_half_hourly <- approx(x = BW_NXR_ERA5_MET$DateTime,
                                   y = BW_NXR_ERA5_MET$`ERA5_Pressure_hPa`,
                                   xout = time_half_hourly)$y

# For specific humidity
specificHumidity_half_hourly <- approx(x = BW_NXR_ERA5_MET$DateTime,
                                       y = BW_NXR_ERA5_MET$`ERA5_Specific_humidity_kg/kg`,
                                       xout = time_half_hourly)$y


# For specific humidity
specificHumidity_half_hourly <- approx(x = BW_NXR_ERA5_MET$DateTime,
                                       y = BW_NXR_ERA5_MET$`ERA5_Specific_humidity_kg/kg`,
                                       xout = time_half_hourly)$y


# For precipitation kg/m2/s
precipitation_kg_half_hourly <- approx(x = BW_NXR_ERA5_MET$DateTime,
                                       y = BW_NXR_ERA5_MET$`ERA5_Precip_kg/m2/s`,
                                       xout = time_half_hourly)$y

# For precipitation
precipitation_mm_half_hourly <- approx(x = BW_NXR_ERA5_MET$DateTime,
                                       y = BW_NXR_ERA5_MET$ERA5_Precip_mm,
                                       xout = time_half_hourly)$y




# Step 3: Combine the interpolated data into a new dataframe
BW_NXR_ERA5_MET_HH <- data.frame(
  DateTime = time_half_hourly,
  ERA5_Temp_C = temperature_half_hourly,
  ERA5_Temp_K = temperature_K_half_hourly,
  ERA5_Dewpoint_temp_C = dewpoint_half_hourly,
  `ERA5_SSR_W/M2` = ssr_half_hourly,
  `ERA5_LWR_W/M2` = lwr_half_hourly,
  `ERA5_Specific_humidity_kg/kg` = specificHumidity_half_hourly,
  `ERA5_Pressure_Pa` = pressure_Pa_half_hourly,
  `ERA5_Pressure_hPa` = pressure_hPa_half_hourly,
  `ERA5_Wind_Speed_m/s` = wndspd_half_hourly,
  ERA5_Precip_mm = precipitation_mm_half_hourly,
  `ERA5_Precip_kg/m2/s`= precipitation_kg_half_hourly
)








# Step 4: plot hourly vs interpolated for QC

ggplot() +
  geom_line(data = BW_NXR_ERA5_MET, aes(x = DateTime, y = ERA5_Temp_C), color = "red", size = 1.2) +
  geom_line(data = BW_NXR_ERA5_MET_HH, aes(x = DateTime, y = ERA5_Temp_C), color = "blue", linetype = "dashed", size = 1) +
  labs(title = "Hourly vs Half-Hourly Interpolated Temperature",
       x = "Date", y = "Temperature (K)") +
  theme_minimal()



# Subset the data for a specific time range that exists in the dataset (e.g., one day)
subset_data_hourly <- subset(BW_NXR_ERA5_MET, DateTime >= as.POSIXct("2018-01-01 00:00:00") & DateTime <= as.POSIXct("2018-01-02 00:00:00"))
subset_data_half_hourly <- subset(BW_NXR_ERA5_MET_HH, DateTime >= as.POSIXct("2018-01-01 00:00:00") & DateTime <= as.POSIXct("2018-01-02 00:00:00"))

# Plot the subsetted data
ggplot() +
  geom_point(data = subset_data_hourly, aes(x = DateTime, y = ERA5_Temp_C), color = "red", size = 1.5) +
  geom_point(data = subset_data_half_hourly, aes(x = DateTime, y = ERA5_Temp_C), color = "blue", size = 1) +
  labs(title = "Hourly vs Half-Hourly Interpolated Temperature (Zoomed In)",
       x = "Time", y = "Temperature (°C)") +
  theme_minimal()






# Calculating VPD using land air temperature and dew point temperature 
# Constants
Pmsl <- 1013.25  # mean sea level pressure in hPa
Z_m <- 950.71    # altitude in meters (as provided)

# Calculate VPD for ERA5-Land
calculate_vpd <- function(temp_C, dewpoint_temp_C, Z_m) {
  # Calculate Pmst (equation 6)
  Pmst <- Pmsl * ((temp_C + 273.16) / (temp_C + 273.16 + 0.0065 * Z_m))^5.625
  
  # Corrected formula for fw (equation 5)
  fw <- 1 + 7 * 10^(-4) + 3.46 * 10^(-6) * Pmst
  
  # Calculate SVP (equation 3)
  SVP <- 6.112 * fw * exp((17.67 * temp_C) / (temp_C + 243.5))
  
  # Calculate AVP (equation 4)
  AVP <- 6.112 * fw * exp((17.67 * dewpoint_temp_C) / (dewpoint_temp_C + 243.5))
  
  # Calculate VPD (equation 2)
  VPD <- SVP - AVP
  
  return(VPD)
}

# Apply VPD calculation to the dataset
BW_NXR_ERA5_MET_HH <- BW_NXR_ERA5_MET_HH %>%
  mutate(VPD_hPa = calculate_vpd(ERA5_Temp_C, ERA5_Dewpoint_temp_C, Z_m))







# for CG_TCH  ----

CG_TCH_Solarad <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/SolarRad/ERA5_export_CG_TCH_SSR.csv")
# Renaming columns in the CG_TCH_Solar_rad data frame
colnames(CG_TCH_Solarad)[colnames(CG_TCH_Solarad) == "system.index"] <- "DateTime"
colnames(CG_TCH_Solarad)[colnames(CG_TCH_Solarad) == "mean"] <- "ERA5_SSR_J/M2"



CG_TCH_Dewpoint <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Dewpoint/ERA5_export_dewpoint_CG_TCH.csv")
#Renaming columns in the CG_TCH_dew_point data frame
colnames(CG_TCH_Dewpoint)[colnames(CG_TCH_Dewpoint) == "system.index"] <- "DateTime"
colnames(CG_TCH_Dewpoint)[colnames(CG_TCH_Dewpoint) == "mean"] <- "ERA5_Dewpoint_temp_K"



CG_TCH_Temp <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Temp/ERA5_export_temp_CG_TCH.csv")
# Renaming columns in the CG_TCH_Temp data frame
colnames(CG_TCH_Temp)[colnames(CG_TCH_Temp) == "system.index"] <- "DateTime"
colnames(CG_TCH_Temp)[colnames(CG_TCH_Temp) == "mean"] <- "ERA5_Temp_K"



CG_TCH_LWR <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/ThermalRad/ERA5_export_CG_TCH_LWR.csv")
# Renaming columns in the CG_TCH_Temp data frame
colnames(CG_TCH_LWR)[colnames(CG_TCH_LWR) == "system.index"] <- "DateTime"
colnames(CG_TCH_LWR)[colnames(CG_TCH_LWR) == "mean"] <- "ERA5_LWR_J/M2"



CG_TCH_Pressure <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Pressure/ERA5_export_CG_TCH_Pressure.csv")
# Renaming columns in the CG_TCH_Temp data frame
colnames(CG_TCH_Pressure)[colnames(CG_TCH_Pressure) == "system.index"] <- "DateTime"
colnames(CG_TCH_Pressure)[colnames(CG_TCH_Pressure) == "mean"] <- "ERA5_Pressure_Pa"



CG_TCH_U_Wind <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/U_Wind/ERA5_export_CG_TCH_U_Wind.csv")
# Renaming columns in the CG_TCH_Temp data frame
colnames(CG_TCH_U_Wind)[colnames(CG_TCH_U_Wind) == "system.index"] <- "DateTime"
colnames(CG_TCH_U_Wind)[colnames(CG_TCH_U_Wind) == "mean"] <- "ERA5_U_Wind_m/s"



CG_TCH_V_Wind <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/V_Wind/ERA5_export_CG_TCH_V_Wind.csv")
# Renaming columns in the CG_TCH_Temp data frame
colnames(CG_TCH_V_Wind)[colnames(CG_TCH_V_Wind) == "system.index"] <- "DateTime"
colnames(CG_TCH_V_Wind)[colnames(CG_TCH_V_Wind) == "mean"] <- "ERA5_V_Wind_m/s"



CG_TCH_Precip <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Rainfall/ERA5_export_CG_TCH_Precipitation.csv")
# Renaming columns in the CG_TCH_Temp data frame
colnames(CG_TCH_Precip)[colnames(CG_TCH_Precip) == "system.index"] <- "DateTime"
colnames(CG_TCH_Precip)[colnames(CG_TCH_Precip) == "mean"] <- "ERA5_Precip_m"


# List of datasets to combine
datasets <- list(CG_TCH_Solarad, CG_TCH_LWR, CG_TCH_Temp, CG_TCH_Dewpoint, CG_TCH_Pressure, CG_TCH_U_Wind, CG_TCH_V_Wind, CG_TCH_Precip)

# Combine all datasets in the list by "DateTime"
CG_TCH_Combined <- Reduce(function(x, y) merge(x, y, by = "DateTime", all = TRUE), datasets)


# Select columns you want to keep
CG_TCH_ERA5_MET <- CG_TCH_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
CG_TCH_ERA5_MET$DateTime <- as.POSIXct(substr(CG_TCH_ERA5_MET$DateTime, 1, 15), 
                                       format = "%Y%m%dT%H", tz = "UTC")




#Convert variables to appropriate units 

#Convert ERA5_SSR_J/M2 from J/m² to W/m²
CG_TCH_ERA5_MET$`ERA5_SSR_W/M2` <- CG_TCH_ERA5_MET$`ERA5_SSR_J/M2` / 3600


#Convert ERA5_LWR_J/M2 from J/m² to W/m²
CG_TCH_ERA5_MET$`ERA5_LWR_W/M2` <- CG_TCH_ERA5_MET$`ERA5_LWR_J/M2` / 3600


# Convert ERA5_Dewpoint_temp_K from Kelvin to Celsius
CG_TCH_ERA5_MET$ERA5_Dewpoint_temp_C <- CG_TCH_ERA5_MET$ERA5_Dewpoint_temp_K - 273.15


# Convert ERA5_Temp_K from Kelvin to Celsius
CG_TCH_ERA5_MET$ERA5_Temp_C <- CG_TCH_ERA5_MET$ERA5_Temp_K - 273.15


# Convert from m to mm
CG_TCH_ERA5_MET$ERA5_Precip_mm <- CG_TCH_ERA5_MET$ERA5_Precip_m * 1000


# convert from mm to kg m-2 s-1
CG_TCH_ERA5_MET$`ERA5_Precip_kg/m2/s` <- CG_TCH_ERA5_MET$ERA5_Precip_mm / 3600



# Calculate wind speed using the U & V components
CG_TCH_ERA5_MET$`ERA5_Wind_Speed_m/s` <- sqrt(CG_TCH_ERA5_MET$`ERA5_U_Wind_m/s`^2 + CG_TCH_ERA5_MET$`ERA5_V_Wind_m/s`^2)



# Convert Pressure from Pa to hPa
CG_TCH_ERA5_MET <- CG_TCH_ERA5_MET %>%
  mutate(ERA5_Pressure_hPa = ERA5_Pressure_Pa / 100)

# Calculate Actual Vapor Pressure (e) in hPa using Dew Point Temperature in Celsius
CG_TCH_ERA5_MET <- CG_TCH_ERA5_MET %>%
  mutate(e_hPa = 6.112 * exp((17.67 * ERA5_Dewpoint_temp_C) / (ERA5_Dewpoint_temp_C + 243.5)))

# Calculate Specific Humidity (q) in kg/kg
CG_TCH_ERA5_MET <- CG_TCH_ERA5_MET %>%
  mutate(`ERA5_Specific_humidity_kg/kg` = (0.622 * e_hPa) / (ERA5_Pressure_hPa - (0.378 * e_hPa)))





# Interpolate data to get half hourly data'

# Step 1: Generate half-hourly timestamps
time_half_hourly <- seq(from = min(CG_TCH_ERA5_MET$DateTime), 
                        to = max(CG_TCH_ERA5_MET$DateTime), by = "30 mins")

# Step 2: Interpolate each variable to the half-hourly time step

# For Temperature (C)
temperature_half_hourly <- approx(x = CG_TCH_ERA5_MET$DateTime, 
                                  y = CG_TCH_ERA5_MET$ERA5_Temp_C, 
                                  xout = time_half_hourly)$y

# For Temperature (K)
temperature_K_half_hourly <- approx(x = CG_TCH_ERA5_MET$DateTime, 
                                    y = CG_TCH_ERA5_MET$ERA5_Temp_K, 
                                    xout = time_half_hourly)$y

# For Dewpoint Temperature 
dewpoint_half_hourly <- approx(x = CG_TCH_ERA5_MET$DateTime, 
                               y = CG_TCH_ERA5_MET$ERA5_Dewpoint_temp_C, 
                               xout = time_half_hourly)$y

# For Solar Surface Radiation (ERAS_SSR_W/M2)
ssr_half_hourly <- approx(x = CG_TCH_ERA5_MET$DateTime, 
                          y = CG_TCH_ERA5_MET$`ERA5_SSR_W/M2`, 
                          xout = time_half_hourly)$y


# For thermal radiation (ERA5_LWR_W/M2)
lwr_half_hourly <- approx(x = CG_TCH_ERA5_MET$DateTime,
                          y = CG_TCH_ERA5_MET$`ERA5_LWR_W/M2`,
                          xout = time_half_hourly)$y

# For wind speed
wndspd_half_hourly <- approx(x = CG_TCH_ERA5_MET$DateTime,
                             y = CG_TCH_ERA5_MET$`ERA5_Wind_Speed_m/s`,
                             xout = time_half_hourly)$y

# For pressure (Pa)
pressure_Pa_half_hourly <- approx(x = CG_TCH_ERA5_MET$DateTime,
                                  y = CG_TCH_ERA5_MET$`ERA5_Pressure_Pa`,
                                  xout = time_half_hourly)$y

# For pressure (hpa)
pressure_hPa_half_hourly <- approx(x = CG_TCH_ERA5_MET$DateTime,
                                   y = CG_TCH_ERA5_MET$`ERA5_Pressure_hPa`,
                                   xout = time_half_hourly)$y

# For specific humidity
specificHumidity_half_hourly <- approx(x = CG_TCH_ERA5_MET$DateTime,
                                       y = CG_TCH_ERA5_MET$`ERA5_Specific_humidity_kg/kg`,
                                       xout = time_half_hourly)$y

# For precipitation
precipitation_mm_half_hourly <- approx(x = CG_TCH_ERA5_MET$DateTime,
                                       y = CG_TCH_ERA5_MET$ERA5_Precip_mm,
                                       xout = time_half_hourly)$y

# For precipitation kg/m2/s
precipitation_kg_half_hourly <- approx(x = CG_TCH_ERA5_MET$DateTime,
                                       y = CG_TCH_ERA5_MET$`ERA5_Precip_kg/m2/s`,
                                       xout = time_half_hourly)$y


# Step 3: Combine the interpolated data into a new dataframe
CG_TCH_ERA5_MET_HH <- data.frame(
  DateTime = time_half_hourly,
  ERA5_Temp_C = temperature_half_hourly,
  ERA5_Temp_K = temperature_K_half_hourly,
  ERA5_Dewpoint_temp_C = dewpoint_half_hourly,
  `ERA5_SSR_W/M2` = ssr_half_hourly,
  `ERA5_LWR_W/M2` = lwr_half_hourly,
  `ERA5_Specific_humidity_kg/kg` = specificHumidity_half_hourly,
  `ERA5_Pressure_Pa` = pressure_Pa_half_hourly,
  `ERA5_Pressure_hPa` = pressure_hPa_half_hourly,
  `ERA5_Wind_Speed_m/s` = wndspd_half_hourly,
  ERA5_Precip_mm = precipitation_mm_half_hourly,
  `ERA5_Precip_kg/m2/s`= precipitation_kg_half_hourly
)






# Step 4: plot hourly vs interpolated for QC

ggplot() +
  geom_line(data = CG_TCH_ERA5_MET, aes(x = DateTime, y = ERA5_Temp_C), color = "red", size = 1.2) +
  geom_line(data = CG_TCH_ERA5_MET_HH, aes(x = DateTime, y = ERA5_Temp_C), color = "blue", linetype = "dashed", size = 1) +
  labs(title = "Hourly vs Half-Hourly Interpolated Temperature",
       x = "Date", y = "Temperature (C)") +
  theme_minimal()



# Subset the data for a specific time range that exists in the dataset (e.g., one day)
subset_data_hourly <- subset(CG_TCH_ERA5_MET, DateTime >= as.POSIXct("2006-01-01 00:00:00") & DateTime <= as.POSIXct("2006-01-02 00:00:00"))
subset_data_half_hourly <- subset(CG_TCH_ERA5_MET_HH, DateTime >= as.POSIXct("2006-01-01 00:00:00") & DateTime <= as.POSIXct("2006-01-02 00:00:00"))

# Plot the subsetted data
ggplot() +
  geom_point(data = subset_data_hourly, aes(x = DateTime, y = ERA5_Temp_C), color = "red", size = 1.5) +
  geom_point(data = subset_data_half_hourly, aes(x = DateTime, y = ERA5_Temp_C), color = "blue", size = 1) +
  labs(title = "Hourly vs Half-Hourly Interpolated Temperature (Zoomed In)",
       x = "Time", y = "Temperature (°C)") +
  theme_minimal()





# Calculating VPD using land air temperature and dew point temperature 
# Constants
Pmsl <- 1013.25  # mean sea level pressure in hPa
Z_m <- 87.54    # altitude in meters (as provided)

# Calculate VPD for ERA5-Land
calculate_vpd <- function(temp_C, dewpoint_temp_C, Z_m) {
  # Calculate Pmst (equation 6)
  Pmst <- Pmsl * ((temp_C + 273.16) / (temp_C + 273.16 + 0.0065 * Z_m))^5.625
  
  # Corrected formula for fw (equation 5)
  fw <- 1 + 7 * 10^(-4) + 3.46 * 10^(-6) * Pmst
  
  # Calculate SVP (equation 3)
  SVP <- 6.112 * fw * exp((17.67 * temp_C) / (temp_C + 243.5))
  
  # Calculate AVP (equation 4)
  AVP <- 6.112 * fw * exp((17.67 * dewpoint_temp_C) / (dewpoint_temp_C + 243.5))
  
  # Calculate VPD (equation 2)
  VPD <- SVP - AVP
  
  return(VPD)
}

# Apply VPD calculation to the dataset
CG_TCH_ERA5_MET_HH <- CG_TCH_ERA5_MET_HH %>%
  mutate(VPD_hPa = calculate_vpd(ERA5_Temp_C, ERA5_Dewpoint_temp_C, Z_m))








# for GH_ANK  ----

GH_ANK_Solarad <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/SolarRad/ERA5_export_GH_ANK_SSR.csv")
# Renaming columns in the GH_ANK_Solar_rad data frame
colnames(GH_ANK_Solarad)[colnames(GH_ANK_Solarad) == "system.index"] <- "DateTime"
colnames(GH_ANK_Solarad)[colnames(GH_ANK_Solarad) == "mean"] <- "ERA5_SSR_J/M2"



GH_ANK_Dewpoint <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Dewpoint/ERA5_export_dewpoint_GH_ANK.csv")
#Renaming columns in the GH_ANK_dew_point data frame
colnames(GH_ANK_Dewpoint)[colnames(GH_ANK_Dewpoint) == "system.index"] <- "DateTime"
colnames(GH_ANK_Dewpoint)[colnames(GH_ANK_Dewpoint) == "mean"] <- "ERA5_Dewpoint_temp_K"



GH_ANK_Temp <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Temp/ERA5_export_temp_GH_ANK.csv")
# Renaming columns in the GH_ANK_Temp data frame
colnames(GH_ANK_Temp)[colnames(GH_ANK_Temp) == "system.index"] <- "DateTime"
colnames(GH_ANK_Temp)[colnames(GH_ANK_Temp) == "mean"] <- "ERA5_Temp_K"



GH_ANK_LWR <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/ThermalRad/ERA5_export_GH_ANK_LWR.csv")
# Renaming columns in the GH_ANK_Temp data frame
colnames(GH_ANK_LWR)[colnames(GH_ANK_LWR) == "system.index"] <- "DateTime"
colnames(GH_ANK_LWR)[colnames(GH_ANK_LWR) == "mean"] <- "ERA5_LWR_J/M2"



GH_ANK_Pressure <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Pressure/ERA5_export_GH_ANK_Pressure.csv")
# Renaming columns in the GH_ANK_Temp data frame
colnames(GH_ANK_Pressure)[colnames(GH_ANK_Pressure) == "system.index"] <- "DateTime"
colnames(GH_ANK_Pressure)[colnames(GH_ANK_Pressure) == "mean"] <- "ERA5_Pressure_Pa"



GH_ANK_U_Wind <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/U_Wind/ERA5_export_GH_ANK_U_Wind.csv")
# Renaming columns in the GH_ANK_Temp data frame
colnames(GH_ANK_U_Wind)[colnames(GH_ANK_U_Wind) == "system.index"] <- "DateTime"
colnames(GH_ANK_U_Wind)[colnames(GH_ANK_U_Wind) == "mean"] <- "ERA5_U_Wind_m/s"



GH_ANK_V_Wind <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/V_Wind/ERA5_export_GH_ANK_V_Wind.csv")
# Renaming columns in the GH_ANK_Temp data frame
colnames(GH_ANK_V_Wind)[colnames(GH_ANK_V_Wind) == "system.index"] <- "DateTime"
colnames(GH_ANK_V_Wind)[colnames(GH_ANK_V_Wind) == "mean"] <- "ERA5_V_Wind_m/s"



GH_ANK_Precip <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Rainfall/ERA5_export_GH_ANK_Precipitation.csv")
# Renaming columns in the GH_ANK_Temp data frame
colnames(GH_ANK_Precip)[colnames(GH_ANK_Precip) == "system.index"] <- "DateTime"
colnames(GH_ANK_Precip)[colnames(GH_ANK_Precip) == "mean"] <- "ERA5_Precip_m"




# List of datasets to combine
datasets <- list(GH_ANK_Solarad, GH_ANK_LWR, GH_ANK_Temp, GH_ANK_Dewpoint, GH_ANK_Pressure, GH_ANK_Precip, GH_ANK_U_Wind, GH_ANK_V_Wind)

# Combine all datasets in the list by "DateTime"
GH_ANK_Combined <- Reduce(function(x, y) merge(x, y, by = "DateTime", all = TRUE), datasets)



# Select columns you want to keep
GH_ANK_ERA5_MET <- GH_ANK_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
GH_ANK_ERA5_MET$DateTime <- as.POSIXct(substr(GH_ANK_ERA5_MET$DateTime, 1, 15), 
                                       format = "%Y%m%dT%H", tz = "UTC")



#Convert variables to appropriate units 

#Convert ERA5_SSR_J/M2 from J/m² to W/m²
GH_ANK_ERA5_MET$`ERA5_SSR_W/M2` <- GH_ANK_ERA5_MET$`ERA5_SSR_J/M2` / 3600


#Convert ERA5_LWR_J/M2 from J/m² to W/m²
GH_ANK_ERA5_MET$`ERA5_LWR_W/M2` <- GH_ANK_ERA5_MET$`ERA5_LWR_J/M2` / 3600


# Convert ERA5_Dewpoint_temp_K from Kelvin to Celsius
GH_ANK_ERA5_MET$ERA5_Dewpoint_temp_C <- GH_ANK_ERA5_MET$ERA5_Dewpoint_temp_K - 273.15


# Convert ERA5_Temp_K from Kelvin to Celsius
GH_ANK_ERA5_MET$ERA5_Temp_C <- GH_ANK_ERA5_MET$ERA5_Temp_K - 273.15


# Convert from m to mm
GH_ANK_ERA5_MET$ERA5_Precip_mm <- GH_ANK_ERA5_MET$ERA5_Precip_m * 1000


# convert from mm to kg m-2 s-1
GH_ANK_ERA5_MET$`ERA5_Precip_kg/m2/s` <- GH_ANK_ERA5_MET$ERA5_Precip_mm / 3600


# Calculate wind speed using the U & V components
GH_ANK_ERA5_MET$`ERA5_Wind_Speed_m/s` <- sqrt(GH_ANK_ERA5_MET$`ERA5_U_Wind_m/s`^2 + GH_ANK_ERA5_MET$`ERA5_V_Wind_m/s`^2)



# Convert Pressure from Pa to hPa
GH_ANK_ERA5_MET <- GH_ANK_ERA5_MET %>%
  mutate(ERA5_Pressure_hPa = ERA5_Pressure_Pa / 100)

# Calculate Actual Vapor Pressure (e) in hPa using Dew Point Temperature in Celsius
GH_ANK_ERA5_MET <- GH_ANK_ERA5_MET %>%
  mutate(e_hPa = 6.112 * exp((17.67 * ERA5_Dewpoint_temp_C) / (ERA5_Dewpoint_temp_C + 243.5)))

# Calculate Specific Humidity (q) in kg/kg
GH_ANK_ERA5_MET <- GH_ANK_ERA5_MET %>%
  mutate(`ERA5_Specific_humidity_kg/kg` = (0.622 * e_hPa) / (ERA5_Pressure_hPa - (0.378 * e_hPa)))


# Interpolate data to get half hourly data'

# Step 1: Generate half-hourly timestamps
time_half_hourly <- seq(from = min(GH_ANK_ERA5_MET$DateTime), 
                        to = max(GH_ANK_ERA5_MET$DateTime), by = "30 mins")

# Step 2: Interpolate each variable to the half-hourly time step

# For Temperature (C)
temperature_half_hourly <- approx(x = GH_ANK_ERA5_MET$DateTime, 
                                  y = GH_ANK_ERA5_MET$ERA5_Temp_C, 
                                  xout = time_half_hourly)$y

# For Temperature (K)
temperature_K_half_hourly <- approx(x = GH_ANK_ERA5_MET$DateTime, 
                                    y = GH_ANK_ERA5_MET$ERA5_Temp_K, 
                                    xout = time_half_hourly)$y

# For Dewpoint Temperature 
dewpoint_half_hourly <- approx(x = GH_ANK_ERA5_MET$DateTime, 
                               y = GH_ANK_ERA5_MET$ERA5_Dewpoint_temp_C, 
                               xout = time_half_hourly)$y

# For Solar Surface Radiation (ERAS_SSR_W/M2)
ssr_half_hourly <- approx(x = GH_ANK_ERA5_MET$DateTime, 
                          y = GH_ANK_ERA5_MET$`ERA5_SSR_W/M2`, 
                          xout = time_half_hourly)$y

# For thermal radiation (ERA5_LWR_W/M2)
lwr_half_hourly <- approx(x = GH_ANK_ERA5_MET$DateTime,
                          y = GH_ANK_ERA5_MET$`ERA5_LWR_W/M2`,
                          xout = time_half_hourly)$y

# For wind speed
wndspd_half_hourly <- approx(x = GH_ANK_ERA5_MET$DateTime,
                             y = GH_ANK_ERA5_MET$`ERA5_Wind_Speed_m/s`,
                             xout = time_half_hourly)$y

# For pressure (Pa)
pressure_Pa_half_hourly <- approx(x = GH_ANK_ERA5_MET$DateTime,
                                  y = GH_ANK_ERA5_MET$`ERA5_Pressure_Pa`,
                                  xout = time_half_hourly)$y

# For pressure (hpa)
pressure_hPa_half_hourly <- approx(x = GH_ANK_ERA5_MET$DateTime,
                                   y = GH_ANK_ERA5_MET$`ERA5_Pressure_hPa`,
                                   xout = time_half_hourly)$y

# For specific humidity
specificHumidity_half_hourly <- approx(x = GH_ANK_ERA5_MET$DateTime,
                                       y = GH_ANK_ERA5_MET$`ERA5_Specific_humidity_kg/kg`,
                                       xout = time_half_hourly)$y

# For precipitation
precipitation_mm_half_hourly <- approx(x = GH_ANK_ERA5_MET$DateTime,
                                       y = GH_ANK_ERA5_MET$ERA5_Precip_mm,
                                       xout = time_half_hourly)$y

# For precipitation kg/m2/s
precipitation_kg_half_hourly <- approx(x = GH_ANK_ERA5_MET$DateTime,
                                       y = GH_ANK_ERA5_MET$`ERA5_Precip_kg/m2/s`,
                                       xout = time_half_hourly)$y


# Step 3: Combine the interpolated data into a new dataframe
GH_ANK_ERA5_MET_HH <- data.frame(
  DateTime = time_half_hourly,
  ERA5_Temp_C = temperature_half_hourly,
  ERA5_Temp_K = temperature_K_half_hourly,
  ERA5_Dewpoint_temp_C = dewpoint_half_hourly,
  `ERA5_SSR_W/M2` = ssr_half_hourly,
  `ERA5_LWR_W/M2` = lwr_half_hourly,
  `ERA5_Specific_humidity_kg/kg` = specificHumidity_half_hourly,
  `ERA5_Pressure_Pa` = pressure_Pa_half_hourly,
  `ERA5_Pressure_hPa` = pressure_hPa_half_hourly,
  `ERA5_Wind_Speed_m/s` = wndspd_half_hourly,
  ERA5_Precip_mm = precipitation_mm_half_hourly,
  `ERA5_Precip_kg/m2/s`= precipitation_kg_half_hourly
)




# Step 4: plot hourly vs interpolated for QC

ggplot() +
  geom_line(data = GH_ANK_ERA5_MET, aes(x = DateTime, y = ERA5_Temp_C), color = "red", size = 1.2) +
  geom_line(data = GH_ANK_ERA5_MET_HH, aes(x = DateTime, y = ERA5_Temp_C), color = "blue", linetype = "dashed", size = 1) +
  labs(title = "Hourly vs Half-Hourly Interpolated Temperature",
       x = "Date", y = "Temperature (C)") +
  theme_minimal()



# Subset the data for a specific time range that exists in the dataset (e.g., one day)
subset_data_hourly <- subset(GH_ANK_ERA5_MET, DateTime >= as.POSIXct("2011-01-01 00:00:00") & DateTime <= as.POSIXct("2011-01-02 00:00:00"))
subset_data_half_hourly <- subset(GH_ANK_ERA5_MET_HH, DateTime >= as.POSIXct("2011-01-01 00:00:00") & DateTime <= as.POSIXct("2011-01-02 00:00:00"))

# Plot the subsetted data
ggplot() +
  geom_point(data = subset_data_hourly, aes(x = DateTime, y = ERA5_Temp_C), color = "red", size = 1.5) +
  geom_point(data = subset_data_half_hourly, aes(x = DateTime, y = ERA5_Temp_C), color = "blue", size = 1) +
  labs(title = "Hourly vs Half-Hourly Interpolated Temperature (Zoomed In)",
       x = "Time", y = "Temperature (°C)") +
  theme_minimal()





# Calculating VPD using land air temperature and dew point temperature 
# Constants
Pmsl <- 1013.25  # mean sea level pressure in hPa
Z_m <- 121    # altitude in meters (as provided)

# Calculate VPD for ERA5-Land
calculate_vpd <- function(temp_C, dewpoint_temp_C, Z_m) {
  # Calculate Pmst (equation 6)
  Pmst <- Pmsl * ((temp_C + 273.16) / (temp_C + 273.16 + 0.0065 * Z_m))^5.625
  
  # Corrected formula for fw (equation 5)
  fw <- 1 + 7 * 10^(-4) + 3.46 * 10^(-6) * Pmst
  
  # Calculate SVP (equation 3)
  SVP <- 6.112 * fw * exp((17.67 * temp_C) / (temp_C + 243.5))
  
  # Calculate AVP (equation 4)
  AVP <- 6.112 * fw * exp((17.67 * dewpoint_temp_C) / (dewpoint_temp_C + 243.5))
  
  # Calculate VPD (equation 2)
  VPD <- SVP - AVP
  
  return(VPD)
}

# Apply VPD calculation to the dataset
GH_ANK_ERA5_MET_HH <- GH_ANK_ERA5_MET_HH %>%
  mutate(VPD_hPa = calculate_vpd(ERA5_Temp_C, ERA5_Dewpoint_temp_C, Z_m))






# for ML_AGG  ----

ML_AGG_Solarad <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/SolarRad/ERA5_export_ML_AGG_SSR.csv")
# Renaming columns in the ML_AGG_Solar_rad data frame
colnames(ML_AGG_Solarad)[colnames(ML_AGG_Solarad) == "system.index"] <- "DateTime"
colnames(ML_AGG_Solarad)[colnames(ML_AGG_Solarad) == "mean"] <- "ERA5_SSR_J/M2"



ML_AGG_Dewpoint <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Dewpoint/ERA5_export_dewpoint_ML_AGG.csv")
#Renaming columns in the ML_AGG_dew_point data frame
colnames(ML_AGG_Dewpoint)[colnames(ML_AGG_Dewpoint) == "system.index"] <- "DateTime"
colnames(ML_AGG_Dewpoint)[colnames(ML_AGG_Dewpoint) == "mean"] <- "ERA5_Dewpoint_temp_K"



ML_AGG_Temp <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Temp/ERA5_export_temp_ML_AGG.csv")
# Renaming columns in the ML_AGG_Temp data frame
colnames(ML_AGG_Temp)[colnames(ML_AGG_Temp) == "system.index"] <- "DateTime"
colnames(ML_AGG_Temp)[colnames(ML_AGG_Temp) == "mean"] <- "ERA5_Temp_K"


ML_AGG_LWR <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/ThermalRad/ERA5_export_ML_AGG_LWR.csv")
# Renaming columns in the ML_AGG_Temp data frame
colnames(ML_AGG_LWR)[colnames(ML_AGG_LWR) == "system.index"] <- "DateTime"
colnames(ML_AGG_LWR)[colnames(ML_AGG_LWR) == "mean"] <- "ERA5_LWR_J/M2"



ML_AGG_Pressure <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Pressure/ERA5_export_ML_AGG_Pressure.csv")
# Renaming columns in the ML_AGG_Temp data frame
colnames(ML_AGG_Pressure)[colnames(ML_AGG_Pressure) == "system.index"] <- "DateTime"
colnames(ML_AGG_Pressure)[colnames(ML_AGG_Pressure) == "mean"] <- "ERA5_Pressure_Pa"



ML_AGG_U_Wind <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/U_Wind/ERA5_export_ML_AGG_U_Wind.csv")
# Renaming columns in the ML_AGG_Temp data frame
colnames(ML_AGG_U_Wind)[colnames(ML_AGG_U_Wind) == "system.index"] <- "DateTime"
colnames(ML_AGG_U_Wind)[colnames(ML_AGG_U_Wind) == "mean"] <- "ERA5_U_Wind_m/s"



ML_AGG_V_Wind <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/V_Wind/ERA5_export_ML_AGG_V_Wind.csv")
# Renaming columns in the ML_AGG_Temp data frame
colnames(ML_AGG_V_Wind)[colnames(ML_AGG_V_Wind) == "system.index"] <- "DateTime"
colnames(ML_AGG_V_Wind)[colnames(ML_AGG_V_Wind) == "mean"] <- "ERA5_V_Wind_m/s"



ML_AGG_Precip <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Rainfall/ERA5_export_ML_AGG_Precipitation.csv")
# Renaming columns in the ML_AGG_Temp data frame
colnames(ML_AGG_Precip)[colnames(ML_AGG_Precip) == "system.index"] <- "DateTime"
colnames(ML_AGG_Precip)[colnames(ML_AGG_Precip) == "mean"] <- "ERA5_Precip_m"



# List of datasets to combine
datasets <- list(ML_AGG_Solarad, ML_AGG_LWR, ML_AGG_Temp, ML_AGG_Dewpoint, ML_AGG_Pressure, ML_AGG_Precip, ML_AGG_U_Wind, ML_AGG_V_Wind)

# Combine all datasets in the list by "DateTime"
ML_AGG_Combined <- Reduce(function(x, y) merge(x, y, by = "DateTime", all = TRUE), datasets)



# Select columns you want to keep
ML_AGG_ERA5_MET <- ML_AGG_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
ML_AGG_ERA5_MET$DateTime <- as.POSIXct(substr(ML_AGG_ERA5_MET$DateTime, 1, 15), 
                                       format = "%Y%m%dT%H", tz = "UTC")






#Convert variables to appropriate units 

#Convert ERA5_SSR_J/M2 from J/m² to W/m²
ML_AGG_ERA5_MET$`ERA5_SSR_W/M2` <- ML_AGG_ERA5_MET$`ERA5_SSR_J/M2` / 3600


#Convert ERA5_LWR_J/M2 from J/m² to W/m²
ML_AGG_ERA5_MET$`ERA5_LWR_W/M2` <- ML_AGG_ERA5_MET$`ERA5_LWR_J/M2` / 3600


# Convert ERA5_Dewpoint_temp_K from Kelvin to Celsius
ML_AGG_ERA5_MET$ERA5_Dewpoint_temp_C <- ML_AGG_ERA5_MET$ERA5_Dewpoint_temp_K - 273.15


# Convert ERA5_Temp_K from Kelvin to Celsius
ML_AGG_ERA5_MET$ERA5_Temp_C <- ML_AGG_ERA5_MET$ERA5_Temp_K - 273.15


# Convert from m to mm
ML_AGG_ERA5_MET$ERA5_Precip_mm <- ML_AGG_ERA5_MET$ERA5_Precip_m * 1000

# convert from mm to kg m-2 s-1
ML_AGG_ERA5_MET$`ERA5_Precip_kg/m2/s` <- ML_AGG_ERA5_MET$ERA5_Precip_mm / 3600


# Calculate wind speed using the U & V components
ML_AGG_ERA5_MET$`ERA5_Wind_Speed_m/s` <- sqrt(ML_AGG_ERA5_MET$`ERA5_U_Wind_m/s`^2 + ML_AGG_ERA5_MET$`ERA5_V_Wind_m/s`^2)



# Convert Pressure from Pa to hPa
ML_AGG_ERA5_MET <- ML_AGG_ERA5_MET %>%
  mutate(ERA5_Pressure_hPa = ERA5_Pressure_Pa / 100)

# Calculate Actual Vapor Pressure (e) in hPa using Dew Point Temperature in Celsius
ML_AGG_ERA5_MET <- ML_AGG_ERA5_MET %>%
  mutate(e_hPa = 6.112 * exp((17.67 * ERA5_Dewpoint_temp_C) / (ERA5_Dewpoint_temp_C + 243.5)))

# Calculate Specific Humidity (q) in kg/kg
ML_AGG_ERA5_MET <- ML_AGG_ERA5_MET %>%
  mutate(`ERA5_Specific_humidity_kg/kg` = (0.622 * e_hPa) / (ERA5_Pressure_hPa - (0.378 * e_hPa)))




# Interpolate data to get half hourly data'

# Step 1: Generate half-hourly timestamps
# I did a 15mins interpolation instead becasue of the nature of the local observation
time_half_hourly <- seq(from = min(ML_AGG_ERA5_MET$DateTime), 
                        to = max(ML_AGG_ERA5_MET$DateTime), by = "30 mins")

# Step 2: Interpolate each variable to the half-hourly time step

# For Temperature (C)
temperature_half_hourly <- approx(x = ML_AGG_ERA5_MET$DateTime, 
                                  y = ML_AGG_ERA5_MET$ERA5_Temp_C, 
                                  xout = time_half_hourly)$y

# For Temperature (K)
temperature_K_half_hourly <- approx(x = ML_AGG_ERA5_MET$DateTime, 
                                    y = ML_AGG_ERA5_MET$ERA5_Temp_K, 
                                    xout = time_half_hourly)$y

# For Dewpoint Temperature 
dewpoint_half_hourly <- approx(x = ML_AGG_ERA5_MET$DateTime, 
                               y = ML_AGG_ERA5_MET$ERA5_Dewpoint_temp_C, 
                               xout = time_half_hourly)$y

# For Solar Surface Radiation (ERAS_SSR_W/M2)
ssr_half_hourly <- approx(x = ML_AGG_ERA5_MET$DateTime, 
                          y = ML_AGG_ERA5_MET$`ERA5_SSR_W/M2`, 
                          xout = time_half_hourly)$y

# For thermal radiation (ERA5_LWR_W/M2)
lwr_half_hourly <- approx(x = ML_AGG_ERA5_MET$DateTime,
                          y = ML_AGG_ERA5_MET$`ERA5_LWR_W/M2`,
                          xout = time_half_hourly)$y

# For wind speed
wndspd_half_hourly <- approx(x = ML_AGG_ERA5_MET$DateTime,
                             y = ML_AGG_ERA5_MET$`ERA5_Wind_Speed_m/s`,
                             xout = time_half_hourly)$y

# For pressure (Pa)
pressure_Pa_half_hourly <- approx(x = ML_AGG_ERA5_MET$DateTime,
                                  y = ML_AGG_ERA5_MET$`ERA5_Pressure_Pa`,
                                  xout = time_half_hourly)$y

# For pressure (hpa)
pressure_hPa_half_hourly <- approx(x = ML_AGG_ERA5_MET$DateTime,
                                   y = ML_AGG_ERA5_MET$`ERA5_Pressure_hPa`,
                                   xout = time_half_hourly)$y

# For specific humidity
specificHumidity_half_hourly <- approx(x = ML_AGG_ERA5_MET$DateTime,
                                       y = ML_AGG_ERA5_MET$`ERA5_Specific_humidity_kg/kg`,
                                       xout = time_half_hourly)$y

# For precipitation
precipitation_mm_half_hourly <- approx(x = ML_AGG_ERA5_MET$DateTime,
                                       y = ML_AGG_ERA5_MET$ERA5_Precip_mm,
                                       xout = time_half_hourly)$y

# For precipitation kg/m2/s
precipitation_kg_half_hourly <- approx(x = ML_AGG_ERA5_MET$DateTime,
                                       y = ML_AGG_ERA5_MET$`ERA5_Precip_kg/m2/s`,
                                       xout = time_half_hourly)$y


# Step 3: Combine the interpolated data into a new dataframe
ML_AGG_ERA5_MET_HH <- data.frame(
  DateTime = time_half_hourly,
  ERA5_Temp_C = temperature_half_hourly,
  ERA5_Temp_K = temperature_K_half_hourly,
  ERA5_Dewpoint_temp_C = dewpoint_half_hourly,
  `ERA5_SSR_W/M2` = ssr_half_hourly,
  `ERA5_LWR_W/M2` = lwr_half_hourly,
  `ERA5_Specific_humidity_kg/kg` = specificHumidity_half_hourly,
  `ERA5_Pressure_Pa` = pressure_Pa_half_hourly,
  `ERA5_Pressure_hPa` = pressure_hPa_half_hourly,
  `ERA5_Wind_Speed_m/s` = wndspd_half_hourly,
  ERA5_Precip_mm = precipitation_mm_half_hourly,
  `ERA5_Precip_kg/m2/s`= precipitation_kg_half_hourly
)





# Step 4: plot hourly vs interpolated for QC

ggplot() +
  geom_line(data = ML_AGG_ERA5_MET, aes(x = DateTime, y = ERA5_Temp_C), color = "red", size = 1.2) +
  geom_line(data = ML_AGG_ERA5_MET_HH, aes(x = DateTime, y = ERA5_Temp_C), color = "blue", linetype = "dashed", size = 1) +
  labs(title = "Hourly vs Half-Hourly Interpolated Temperature",
       x = "Date", y = "Temperature (K)") +
  theme_minimal()



# Subset the data for a specific time range that exists in the dataset (e.g., one day)
subset_data_hourly <- subset(ML_AGG_ERA5_MET, DateTime >= as.POSIXct("2007-03-28 00:00:00") & DateTime <= as.POSIXct("2007-03-29 00:00:00"))
subset_data_half_hourly <- subset(ML_AGG_ERA5_MET_HH, DateTime >= as.POSIXct("2007-03-28 00:00:00") & DateTime <= as.POSIXct("2007-03-29 00:00:00"))

# Plot the subsetted data
ggplot() +
  geom_point(data = subset_data_hourly, aes(x = DateTime, y = ERA5_Temp_C), color = "red", size = 1.5) +
  geom_point(data = subset_data_half_hourly, aes(x = DateTime, y = ERA5_Temp_C), color = "blue", size = 1) +
  labs(title = "Hourly vs Half-Hourly Interpolated Temperature (Zoomed In)",
       x = "Time", y = "Temperature (°C)") +
  theme_minimal()






# Calculating VPD using land air temperature and dew point temperature 
# Constants
Pmsl <- 1013.25  # mean sea level pressure in hPa
Z_m <- 314   # altitude in meters (as provided)

# Calculate VPD for ERA5-Land
calculate_vpd <- function(temp_C, dewpoint_temp_C, Z_m) {
  # Calculate Pmst (equation 6)
  Pmst <- Pmsl * ((temp_C + 273.16) / (temp_C + 273.16 + 0.0065 * Z_m))^5.625
  
  # Corrected formula for fw (equation 5)
  fw <- 1 + 7 * 10^(-4) + 3.46 * 10^(-6) * Pmst
  
  # Calculate SVP (equation 3)
  SVP <- 6.112 * fw * exp((17.67 * temp_C) / (temp_C + 243.5))
  
  # Calculate AVP (equation 4)
  AVP <- 6.112 * fw * exp((17.67 * dewpoint_temp_C) / (dewpoint_temp_C + 243.5))
  
  # Calculate VPD (equation 2)
  VPD <- SVP - AVP
  
  return(VPD)
}

# Apply VPD calculation to the dataset
ML_AGG_ERA5_MET_HH <- ML_AGG_ERA5_MET_HH %>%
  mutate(VPD_hPa = calculate_vpd(ERA5_Temp_C, ERA5_Dewpoint_temp_C, Z_m))



# Convert "DateTime" to a POSIXct object if it's not already
# ML_AGG_ERA5_MET_HH$DateTime <- as.POSIXct(ML_AGG_ERA5_MET_HH$DateTime)

# Filter rows where the minutes are 15 or 45
# ML_AGG_ERA5_MET_HH <- ML_AGG_ERA5_MET_HH[format(ML_AGG_ERA5_MET_HH$DateTime, "%M") %in% c("15", "45"), ]





# for NE_WAF  ----

NE_WAF_Solarad <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/SolarRad/ERA5_export_NE_WAF_SSR.csv")
# Renaming columns in the NE_WAF_Solar_rad data frame
colnames(NE_WAF_Solarad)[colnames(NE_WAF_Solarad) == "system.index"] <- "DateTime"
colnames(NE_WAF_Solarad)[colnames(NE_WAF_Solarad) == "mean"] <- "ERA5_SSR_J/M2"



NE_WAF_Dewpoint <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Dewpoint/ERA5_export_dewpoint_NE_WAF.csv")
#Renaming columns in the NE_WAF_dew_point data frame
colnames(NE_WAF_Dewpoint)[colnames(NE_WAF_Dewpoint) == "system.index"] <- "DateTime"
colnames(NE_WAF_Dewpoint)[colnames(NE_WAF_Dewpoint) == "mean"] <- "ERA5_Dewpoint_temp_K"



NE_WAF_Temp <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Temp/ERA5_export_temp_NE_WAF.csv")
# Renaming columns in the NE_WAF_Temp data frame
colnames(NE_WAF_Temp)[colnames(NE_WAF_Temp) == "system.index"] <- "DateTime"
colnames(NE_WAF_Temp)[colnames(NE_WAF_Temp) == "mean"] <- "ERA5_Temp_K"


NE_WAF_LWR <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/ThermalRad/ERA5_export_NE_WAF_LWR.csv")
# Renaming columns in the NE_WAF_Temp data frame
colnames(NE_WAF_LWR)[colnames(NE_WAF_LWR) == "system.index"] <- "DateTime"
colnames(NE_WAF_LWR)[colnames(NE_WAF_LWR) == "mean"] <- "ERA5_LWR_J/M2"



NE_WAF_Pressure <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Pressure/ERA5_export_NE_WAF_Pressure.csv")
# Renaming columns in the NE_WAF_Temp data frame
colnames(NE_WAF_Pressure)[colnames(NE_WAF_Pressure) == "system.index"] <- "DateTime"
colnames(NE_WAF_Pressure)[colnames(NE_WAF_Pressure) == "mean"] <- "ERA5_Pressure_Pa"



NE_WAF_U_Wind <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/U_Wind/ERA5_export_NE_WAF_U_Wind.csv")
# Renaming columns in the NE_WAF_Temp data frame
colnames(NE_WAF_U_Wind)[colnames(NE_WAF_U_Wind) == "system.index"] <- "DateTime"
colnames(NE_WAF_U_Wind)[colnames(NE_WAF_U_Wind) == "mean"] <- "ERA5_U_Wind_m/s"



NE_WAF_V_Wind <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/V_Wind/ERA5_export_NE_WAF_V_Wind.csv")
# Renaming columns in the NE_WAF_Temp data frame
colnames(NE_WAF_V_Wind)[colnames(NE_WAF_V_Wind) == "system.index"] <- "DateTime"
colnames(NE_WAF_V_Wind)[colnames(NE_WAF_V_Wind) == "mean"] <- "ERA5_V_Wind_m/s"



NE_WAF_Precip <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Rainfall/ERA5_export_NE_WAF_Precipitation.csv")
# Renaming columns in the NE_WAF_Temp data frame
colnames(NE_WAF_Precip)[colnames(NE_WAF_Precip) == "system.index"] <- "DateTime"
colnames(NE_WAF_Precip)[colnames(NE_WAF_Precip) == "mean"] <- "ERA5_Precip_m"



# List of datasets to combine
datasets <- list(NE_WAF_Solarad, NE_WAF_LWR, NE_WAF_Temp, NE_WAF_Dewpoint, NE_WAF_Pressure, NE_WAF_Precip, NE_WAF_U_Wind, NE_WAF_V_Wind)

# Combine all datasets in the list by "DateTime"
NE_WAF_Combined <- Reduce(function(x, y) merge(x, y, by = "DateTime", all = TRUE), datasets)




# Select columns you want to keep
NE_WAF_ERA5_MET <- NE_WAF_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
NE_WAF_ERA5_MET$DateTime <- as.POSIXct(substr(NE_WAF_ERA5_MET$DateTime, 1, 15), 
                                       format = "%Y%m%dT%H", tz = "UTC")





#Convert variables to appropriate units 

#Convert ERA5_SSR_J/M2 from J/m² to W/m²
NE_WAF_ERA5_MET$`ERA5_SSR_W/M2` <- NE_WAF_ERA5_MET$`ERA5_SSR_J/M2` / 3600


#Convert ERA5_LWR_J/M2 from J/m² to W/m²
NE_WAF_ERA5_MET$`ERA5_LWR_W/M2` <- NE_WAF_ERA5_MET$`ERA5_LWR_J/M2` / 3600


# Convert ERA5_Dewpoint_temp_K from Kelvin to Celsius
NE_WAF_ERA5_MET$ERA5_Dewpoint_temp_C <- NE_WAF_ERA5_MET$ERA5_Dewpoint_temp_K - 273.15


# Convert ERA5_Temp_K from Kelvin to Celsius
NE_WAF_ERA5_MET$ERA5_Temp_C <- NE_WAF_ERA5_MET$ERA5_Temp_K - 273.15


# Convert from m to mm
NE_WAF_ERA5_MET$ERA5_Precip_mm <- NE_WAF_ERA5_MET$ERA5_Precip_m * 1000


# convert from mm to kg m-2 s-1
NE_WAF_ERA5_MET$`ERA5_Precip_kg/m2/s` <- NE_WAF_ERA5_MET$ERA5_Precip_mm / 3600


# Calculate wind speed using the U & V components
NE_WAF_ERA5_MET$`ERA5_Wind_Speed_m/s` <- sqrt(NE_WAF_ERA5_MET$`ERA5_U_Wind_m/s`^2 + NE_WAF_ERA5_MET$`ERA5_V_Wind_m/s`^2)



# Convert Pressure from Pa to hPa
NE_WAF_ERA5_MET <- NE_WAF_ERA5_MET %>%
  mutate(ERA5_Pressure_hPa = ERA5_Pressure_Pa / 100)

# Calculate Actual Vapor Pressure (e) in hPa using Dew Point Temperature in Celsius
NE_WAF_ERA5_MET <- NE_WAF_ERA5_MET %>%
  mutate(e_hPa = 6.112 * exp((17.67 * ERA5_Dewpoint_temp_C) / (ERA5_Dewpoint_temp_C + 243.5)))

# Calculate Specific Humidity (q) in kg/kg
NE_WAF_ERA5_MET <- NE_WAF_ERA5_MET %>%
  mutate(`ERA5_Specific_humidity_kg/kg` = (0.622 * e_hPa) / (ERA5_Pressure_hPa - (0.378 * e_hPa)))




# Interpolate data to get half hourly data'

# Step 1: Generate half-hourly timestamps
time_half_hourly <- seq(from = min(NE_WAF_ERA5_MET$DateTime), 
                        to = max(NE_WAF_ERA5_MET$DateTime), by = "30 mins")

# Step 2: Interpolate each variable to the half-hourly time step

# For Temperature (C)
temperature_half_hourly <- approx(x = NE_WAF_ERA5_MET$DateTime, 
                                  y = NE_WAF_ERA5_MET$ERA5_Temp_C, 
                                  xout = time_half_hourly)$y

# For Temperature (K)
temperature_K_half_hourly <- approx(x = NE_WAF_ERA5_MET$DateTime, 
                                    y = NE_WAF_ERA5_MET$ERA5_Temp_K, 
                                    xout = time_half_hourly)$y

# For Dewpoint Temperature 
dewpoint_half_hourly <- approx(x = NE_WAF_ERA5_MET$DateTime, 
                               y = NE_WAF_ERA5_MET$ERA5_Dewpoint_temp_C, 
                               xout = time_half_hourly)$y

# For Solar Surface Radiation (ERAS_SSR_W/M2)
ssr_half_hourly <- approx(x = NE_WAF_ERA5_MET$DateTime, 
                          y = NE_WAF_ERA5_MET$`ERA5_SSR_W/M2`, 
                          xout = time_half_hourly)$y

# For thermal radiation (ERA5_LWR_W/M2)
lwr_half_hourly <- approx(x = NE_WAF_ERA5_MET$DateTime,
                          y = NE_WAF_ERA5_MET$`ERA5_LWR_W/M2`,
                          xout = time_half_hourly)$y

# For wind speed
wndspd_half_hourly <- approx(x = NE_WAF_ERA5_MET$DateTime,
                             y = NE_WAF_ERA5_MET$`ERA5_Wind_Speed_m/s`,
                             xout = time_half_hourly)$y

# For pressure (Pa)
pressure_Pa_half_hourly <- approx(x = NE_WAF_ERA5_MET$DateTime,
                                  y = NE_WAF_ERA5_MET$`ERA5_Pressure_Pa`,
                                  xout = time_half_hourly)$y

# For pressure (hpa)
pressure_hPa_half_hourly <- approx(x = NE_WAF_ERA5_MET$DateTime,
                                   y = NE_WAF_ERA5_MET$`ERA5_Pressure_hPa`,
                                   xout = time_half_hourly)$y

# For specific humidity
specificHumidity_half_hourly <- approx(x = NE_WAF_ERA5_MET$DateTime,
                                       y = NE_WAF_ERA5_MET$`ERA5_Specific_humidity_kg/kg`,
                                       xout = time_half_hourly)$y

# For precipitation
precipitation_mm_half_hourly <- approx(x = NE_WAF_ERA5_MET$DateTime,
                                       y = NE_WAF_ERA5_MET$ERA5_Precip_mm,
                                       xout = time_half_hourly)$y

# For precipitation kg/m2/s
precipitation_kg_half_hourly <- approx(x = NE_WAF_ERA5_MET$DateTime,
                                       y = NE_WAF_ERA5_MET$`ERA5_Precip_kg/m2/s`,
                                       xout = time_half_hourly)$y


# Step 3: Combine the interpolated data into a new dataframe
NE_WAF_ERA5_MET_HH <- data.frame(
  DateTime = time_half_hourly,
  ERA5_Temp_C = temperature_half_hourly,
  ERA5_Temp_K = temperature_K_half_hourly,
  ERA5_Dewpoint_temp_C = dewpoint_half_hourly,
  `ERA5_SSR_W/M2` = ssr_half_hourly,
  `ERA5_LWR_W/M2` = lwr_half_hourly,
  `ERA5_Specific_humidity_kg/kg` = specificHumidity_half_hourly,
  `ERA5_Pressure_Pa` = pressure_Pa_half_hourly,
  `ERA5_Pressure_hPa` = pressure_hPa_half_hourly,
  `ERA5_Wind_Speed_m/s` = wndspd_half_hourly,
  ERA5_Precip_mm = precipitation_mm_half_hourly,
  `ERA5_Precip_kg/m2/s`= precipitation_kg_half_hourly
)



# Step 4: plot hourly vs interpolated for QC

ggplot() +
  geom_line(data = NE_WAF_ERA5_MET, aes(x = DateTime, y = ERA5_Temp_C), color = "red", size = 1.2) +
  geom_line(data = NE_WAF_ERA5_MET_HH, aes(x = DateTime, y = ERA5_Temp_C), color = "blue", linetype = "dashed", size = 1) +
  labs(title = "Hourly vs Half-Hourly Interpolated Temperature",
       x = "Date", y = "Temperature (K)") +
  theme_minimal()



# Subset the data for a specific time range that exists in the dataset (e.g., one day)
subset_data_hourly <- subset(NE_WAF_ERA5_MET, DateTime >= as.POSIXct("2005-11-11 00:00:00") & DateTime <= as.POSIXct("2005-11-12 00:00:00"))
subset_data_half_hourly <- subset(NE_WAF_ERA5_MET_HH, DateTime >= as.POSIXct("2005-11-11 00:00:00") & DateTime <= as.POSIXct("2005-11-12 00:00:00"))

# Plot the subsetted data
ggplot() +
  geom_point(data = subset_data_hourly, aes(x = DateTime, y = ERA5_Temp_C), color = "red", size = 1.5) +
  geom_point(data = subset_data_half_hourly, aes(x = DateTime, y = ERA5_Temp_C), color = "blue", size = 1) +
  labs(title = "Hourly vs Half-Hourly Interpolated Temperature (Zoomed In)",
       x = "Time", y = "Temperature (°C)") +
  theme_minimal()





# Calculating VPD using land air temperature and dew point temperature 
# Constants
Pmsl <- 1013.25  # mean sea level pressure in hPa
Z_m <- 235    # altitude in meters (as provided)

# Calculate VPD for ERA5-Land
calculate_vpd <- function(temp_C, dewpoint_temp_C, Z_m) {
  # Calculate Pmst (equation 6)
  Pmst <- Pmsl * ((temp_C + 273.16) / (temp_C + 273.16 + 0.0065 * Z_m))^5.625
  
  # Corrected formula for fw (equation 5)
  fw <- 1 + 7 * 10^(-4) + 3.46 * 10^(-6) * Pmst
  
  # Calculate SVP (equation 3)
  SVP <- 6.112 * fw * exp((17.67 * temp_C) / (temp_C + 243.5))
  
  # Calculate AVP (equation 4)
  AVP <- 6.112 * fw * exp((17.67 * dewpoint_temp_C) / (dewpoint_temp_C + 243.5))
  
  # Calculate VPD (equation 2)
  VPD <- SVP - AVP
  
  return(VPD)
}

# Apply VPD calculation to the dataset
NE_WAF_ERA5_MET_HH <- NE_WAF_ERA5_MET_HH %>%
  mutate(VPD_hPa = calculate_vpd(ERA5_Temp_C, ERA5_Dewpoint_temp_C, Z_m))






# for NE_WAM  ----

NE_WAM_Solarad <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/SolarRad/ERA5_export_NE_WAM_SSR.csv")
# Renaming columns in the NE_WAM_Solar_rad data frame
colnames(NE_WAM_Solarad)[colnames(NE_WAM_Solarad) == "system.index"] <- "DateTime"
colnames(NE_WAM_Solarad)[colnames(NE_WAM_Solarad) == "mean"] <- "ERA5_SSR_J/M2"



NE_WAM_Dewpoint <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Dewpoint/ERA5_export_dewpoint_NE_WAM.csv")
#Renaming columns in the NE_WAM_dew_point data frame
colnames(NE_WAM_Dewpoint)[colnames(NE_WAM_Dewpoint) == "system.index"] <- "DateTime"
colnames(NE_WAM_Dewpoint)[colnames(NE_WAM_Dewpoint) == "mean"] <- "ERA5_Dewpoint_temp_K"



NE_WAM_Temp <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Temp/ERA5_export_temp_NE_WAM.csv")
# Renaming columns in the NE_WAM_Temp data frame
colnames(NE_WAM_Temp)[colnames(NE_WAM_Temp) == "system.index"] <- "DateTime"
colnames(NE_WAM_Temp)[colnames(NE_WAM_Temp) == "mean"] <- "ERA5_Temp_K"


NE_WAM_LWR <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/ThermalRad/ERA5_export_NE_WAM_LWR.csv")
# Renaming columns in the NE_WAM_Temp data frame
colnames(NE_WAM_LWR)[colnames(NE_WAM_LWR) == "system.index"] <- "DateTime"
colnames(NE_WAM_LWR)[colnames(NE_WAM_LWR) == "mean"] <- "ERA5_LWR_J/M2"



NE_WAM_Pressure <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Pressure/ERA5_export_NE_WAM_Pressure.csv")
# Renaming columns in the NE_WAM_Temp data frame
colnames(NE_WAM_Pressure)[colnames(NE_WAM_Pressure) == "system.index"] <- "DateTime"
colnames(NE_WAM_Pressure)[colnames(NE_WAM_Pressure) == "mean"] <- "ERA5_Pressure_Pa"



NE_WAM_U_Wind <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/U_Wind/ERA5_export_NE_WAM_U_Wind.csv")
# Renaming columns in the NE_WAM_Temp data frame
colnames(NE_WAM_U_Wind)[colnames(NE_WAM_U_Wind) == "system.index"] <- "DateTime"
colnames(NE_WAM_U_Wind)[colnames(NE_WAM_U_Wind) == "mean"] <- "ERA5_U_Wind_m/s"



NE_WAM_V_Wind <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/V_Wind/ERA5_export_NE_WAM_V_Wind.csv")
# Renaming columns in the NE_WAM_Temp data frame
colnames(NE_WAM_V_Wind)[colnames(NE_WAM_V_Wind) == "system.index"] <- "DateTime"
colnames(NE_WAM_V_Wind)[colnames(NE_WAM_V_Wind) == "mean"] <- "ERA5_V_Wind_m/s"



NE_WAM_Precip <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Rainfall/ERA5_export_NE_WAM_Precipitation.csv")
# Renaming columns in the NE_WAM_Temp data frame
colnames(NE_WAM_Precip)[colnames(NE_WAM_Precip) == "system.index"] <- "DateTime"
colnames(NE_WAM_Precip)[colnames(NE_WAM_Precip) == "mean"] <- "ERA5_Precip_m"



# List of datasets to combine
datasets <- list(NE_WAM_Solarad, NE_WAM_LWR, NE_WAM_Temp, NE_WAM_Dewpoint, NE_WAM_Pressure, NE_WAM_Precip, NE_WAM_U_Wind, NE_WAM_V_Wind)

# Combine all datasets in the list by "DateTime"
NE_WAM_Combined <- Reduce(function(x, y) merge(x, y, by = "DateTime", all = TRUE), datasets)




# Select columns you want to keep
NE_WAM_ERA5_MET <- NE_WAM_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
NE_WAM_ERA5_MET$DateTime <- as.POSIXct(substr(NE_WAM_ERA5_MET$DateTime, 1, 15), 
                                       format = "%Y%m%dT%H", tz = "UTC")







#Convert variables to appropriate units 

#Convert ERA5_SSR_J/M2 from J/m² to W/m²
NE_WAM_ERA5_MET$`ERA5_SSR_W/M2` <- NE_WAM_ERA5_MET$`ERA5_SSR_J/M2` / 3600


#Convert ERA5_LWR_J/M2 from J/m² to W/m²
NE_WAM_ERA5_MET$`ERA5_LWR_W/M2` <- NE_WAM_ERA5_MET$`ERA5_LWR_J/M2` / 3600


# Convert ERA5_Dewpoint_temp_K from Kelvin to Celsius
NE_WAM_ERA5_MET$ERA5_Dewpoint_temp_C <- NE_WAM_ERA5_MET$ERA5_Dewpoint_temp_K - 273.15


# Convert ERA5_Temp_K from Kelvin to Celsius
NE_WAM_ERA5_MET$ERA5_Temp_C <- NE_WAM_ERA5_MET$ERA5_Temp_K - 273.15


# Convert from m to mm
NE_WAM_ERA5_MET$ERA5_Precip_mm <- NE_WAM_ERA5_MET$ERA5_Precip_m * 1000


# convert from mm to kg m-2 s-1
NE_WAM_ERA5_MET$`ERA5_Precip_kg/m2/s` <- NE_WAM_ERA5_MET$ERA5_Precip_mm / 3600


# Calculate wind speed using the U & V components
NE_WAM_ERA5_MET$`ERA5_Wind_Speed_m/s` <- sqrt(NE_WAM_ERA5_MET$`ERA5_U_Wind_m/s`^2 + NE_WAM_ERA5_MET$`ERA5_V_Wind_m/s`^2)



# Convert Pressure from Pa to hPa
NE_WAM_ERA5_MET <- NE_WAM_ERA5_MET %>%
  mutate(ERA5_Pressure_hPa = ERA5_Pressure_Pa / 100)

# Calculate Actual Vapor Pressure (e) in hPa using Dew Point Temperature in Celsius
NE_WAM_ERA5_MET <- NE_WAM_ERA5_MET %>%
  mutate(e_hPa = 6.112 * exp((17.67 * ERA5_Dewpoint_temp_C) / (ERA5_Dewpoint_temp_C + 243.5)))

# Calculate Specific Humidity (q) in kg/kg
NE_WAM_ERA5_MET <- NE_WAM_ERA5_MET %>%
  mutate(`ERA5_Specific_humidity_kg/kg` = (0.622 * e_hPa) / (ERA5_Pressure_hPa - (0.378 * e_hPa)))



# Interpolate data to get half hourly data'

# Step 1: Generate half-hourly timestamps
time_half_hourly <- seq(from = min(NE_WAM_ERA5_MET$DateTime), 
                        to = max(NE_WAM_ERA5_MET$DateTime), by = "30 mins")

# Step 2: Interpolate each variable to the half-hourly time step

# For Temperature (C)
temperature_half_hourly <- approx(x = NE_WAM_ERA5_MET$DateTime, 
                                  y = NE_WAM_ERA5_MET$ERA5_Temp_C, 
                                  xout = time_half_hourly)$y

# For Temperature (K)
temperature_K_half_hourly <- approx(x = NE_WAM_ERA5_MET$DateTime, 
                                    y = NE_WAM_ERA5_MET$ERA5_Temp_K, 
                                    xout = time_half_hourly)$y

# For Dewpoint Temperature 
dewpoint_half_hourly <- approx(x = NE_WAM_ERA5_MET$DateTime, 
                               y = NE_WAM_ERA5_MET$ERA5_Dewpoint_temp_C, 
                               xout = time_half_hourly)$y

# For Solar Surface Radiation (ERAS_SSR_W/M2)
ssr_half_hourly <- approx(x = NE_WAM_ERA5_MET$DateTime, 
                          y = NE_WAM_ERA5_MET$`ERA5_SSR_W/M2`, 
                          xout = time_half_hourly)$y

# For thermal radiation (ERA5_LWR_W/M2)
lwr_half_hourly <- approx(x = NE_WAM_ERA5_MET$DateTime,
                          y = NE_WAM_ERA5_MET$`ERA5_LWR_W/M2`,
                          xout = time_half_hourly)$y

# For wind speed
wndspd_half_hourly <- approx(x = NE_WAM_ERA5_MET$DateTime,
                             y = NE_WAM_ERA5_MET$`ERA5_Wind_Speed_m/s`,
                             xout = time_half_hourly)$y

# For pressure (Pa)
pressure_Pa_half_hourly <- approx(x = NE_WAM_ERA5_MET$DateTime,
                                  y = NE_WAM_ERA5_MET$`ERA5_Pressure_Pa`,
                                  xout = time_half_hourly)$y

# For pressure (hpa)
pressure_hPa_half_hourly <- approx(x = NE_WAM_ERA5_MET$DateTime,
                                   y = NE_WAM_ERA5_MET$`ERA5_Pressure_hPa`,
                                   xout = time_half_hourly)$y

# For specific humidity
specificHumidity_half_hourly <- approx(x = NE_WAM_ERA5_MET$DateTime,
                                       y = NE_WAM_ERA5_MET$`ERA5_Specific_humidity_kg/kg`,
                                       xout = time_half_hourly)$y

# For precipitation
precipitation_mm_half_hourly <- approx(x = NE_WAM_ERA5_MET$DateTime,
                                       y = NE_WAM_ERA5_MET$ERA5_Precip_mm,
                                       xout = time_half_hourly)$y

# For precipitation kg/m2/s
precipitation_kg_half_hourly <- approx(x = NE_WAM_ERA5_MET$DateTime,
                                       y = NE_WAM_ERA5_MET$`ERA5_Precip_kg/m2/s`,
                                       xout = time_half_hourly)$y


# Step 3: Combine the interpolated data into a new dataframe
NE_WAM_ERA5_MET_HH <- data.frame(
  DateTime = time_half_hourly,
  ERA5_Temp_C = temperature_half_hourly,
  ERA5_Temp_K = temperature_K_half_hourly,
  ERA5_Dewpoint_temp_C = dewpoint_half_hourly,
  `ERA5_SSR_W/M2` = ssr_half_hourly,
  `ERA5_LWR_W/M2` = lwr_half_hourly,
  `ERA5_Specific_humidity_kg/kg` = specificHumidity_half_hourly,
  `ERA5_Pressure_Pa` = pressure_Pa_half_hourly,
  `ERA5_Pressure_hPa` = pressure_hPa_half_hourly,
  `ERA5_Wind_Speed_m/s` = wndspd_half_hourly,
  ERA5_Precip_mm = precipitation_mm_half_hourly,
  `ERA5_Precip_kg/m2/s`= precipitation_kg_half_hourly
)






# Step 4: plot hourly vs interpolated for QC

ggplot() +
  geom_line(data = NE_WAM_ERA5_MET, aes(x = DateTime, y = ERA5_Temp_C), color = "red", size = 1.2) +
  geom_line(data = NE_WAM_ERA5_MET_HH, aes(x = DateTime, y = ERA5_Temp_C), color = "blue", linetype = "dashed", size = 1) +
  labs(title = "Hourly vs Half-Hourly Interpolated Temperature",
       x = "Date", y = "Temperature (K)") +
  theme_minimal()



# Subset the data for a specific time range that exists in the dataset (e.g., one day)
subset_data_hourly <- subset(NE_WAM_ERA5_MET, DateTime >= as.POSIXct("2005-11-11 00:00:00") & DateTime <= as.POSIXct("2005-11-12 00:00:00"))
subset_data_half_hourly <- subset(NE_WAM_ERA5_MET_HH, DateTime >= as.POSIXct("2005-11-11 00:00:00") & DateTime <= as.POSIXct("2005-11-12 00:00:00"))

# Plot the subsetted data
ggplot() +
  geom_point(data = subset_data_hourly, aes(x = DateTime, y = ERA5_Temp_C), color = "red", size = 1.5) +
  geom_point(data = subset_data_half_hourly, aes(x = DateTime, y = ERA5_Temp_C), color = "blue", size = 1) +
  labs(title = "Hourly vs Half-Hourly Interpolated Temperature (Zoomed In)",
       x = "Time", y = "Temperature (°C)") +
  theme_minimal()





# Calculating VPD using land air temperature and dew point temperature 
# Constants
Pmsl <- 1013.25  # mean sea level pressure in hPa
Z_m <- 244   # altitude in meters (as provided)

# Calculate VPD for ERA5-Land
calculate_vpd <- function(temp_C, dewpoint_temp_C, Z_m) {
  # Calculate Pmst (equation 6)
  Pmst <- Pmsl * ((temp_C + 273.16) / (temp_C + 273.16 + 0.0065 * Z_m))^5.625
  
  # Corrected formula for fw (equation 5)
  fw <- 1 + 7 * 10^(-4) + 3.46 * 10^(-6) * Pmst
  
  # Calculate SVP (equation 3)
  SVP <- 6.112 * fw * exp((17.67 * temp_C) / (temp_C + 243.5))
  
  # Calculate AVP (equation 4)
  AVP <- 6.112 * fw * exp((17.67 * dewpoint_temp_C) / (dewpoint_temp_C + 243.5))
  
  # Calculate VPD (equation 2)
  VPD <- SVP - AVP
  
  return(VPD)
}

# Apply VPD calculation to the dataset
NE_WAM_ERA5_MET_HH <- NE_WAM_ERA5_MET_HH %>%
  mutate(VPD_hPa = calculate_vpd(ERA5_Temp_C, ERA5_Dewpoint_temp_C, Z_m))




# for SD_DEM  ----

SD_DEM_Solarad <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/SolarRad/ERA5_export_SD_DEM_SSR.csv")
# Renaming columns in the SD_DEM_Solar_rad data frame
colnames(SD_DEM_Solarad)[colnames(SD_DEM_Solarad) == "system.index"] <- "DateTime"
colnames(SD_DEM_Solarad)[colnames(SD_DEM_Solarad) == "mean"] <- "ERA5_SSR_J/M2"



SD_DEM_Dewpoint <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Dewpoint/ERA5_export_dewpoint_SD_DEM.csv")
#Renaming columns in the SD_DEM_dew_point data frame
colnames(SD_DEM_Dewpoint)[colnames(SD_DEM_Dewpoint) == "system.index"] <- "DateTime"
colnames(SD_DEM_Dewpoint)[colnames(SD_DEM_Dewpoint) == "mean"] <- "ERA5_Dewpoint_temp_K"



SD_DEM_Temp <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Temp/ERA5_export_temp_SD_DEM.csv")
# Renaming columns in the SD_DEM_Temp data frame
colnames(SD_DEM_Temp)[colnames(SD_DEM_Temp) == "system.index"] <- "DateTime"
colnames(SD_DEM_Temp)[colnames(SD_DEM_Temp) == "mean"] <- "ERA5_Temp_K"


SD_DEM_LWR <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/ThermalRad/ERA5_export_SD_DEM_LWR.csv")
# Renaming columns in the SD_DEM_Temp data frame
colnames(SD_DEM_LWR)[colnames(SD_DEM_LWR) == "system.index"] <- "DateTime"
colnames(SD_DEM_LWR)[colnames(SD_DEM_LWR) == "mean"] <- "ERA5_LWR_J/M2"



SD_DEM_Pressure <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Pressure/ERA5_export_SD_DEM_Pressure.csv")
# Renaming columns in the SD_DEM_Temp data frame
colnames(SD_DEM_Pressure)[colnames(SD_DEM_Pressure) == "system.index"] <- "DateTime"
colnames(SD_DEM_Pressure)[colnames(SD_DEM_Pressure) == "mean"] <- "ERA5_Pressure_Pa"



SD_DEM_U_Wind <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/U_Wind/ERA5_export_SD_DEM_U_Wind.csv")
# Renaming columns in the SD_DEM_Temp data frame
colnames(SD_DEM_U_Wind)[colnames(SD_DEM_U_Wind) == "system.index"] <- "DateTime"
colnames(SD_DEM_U_Wind)[colnames(SD_DEM_U_Wind) == "mean"] <- "ERA5_U_Wind_m/s"



SD_DEM_V_Wind <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/V_Wind/ERA5_export_SD_DEM_V_Wind.csv")
# Renaming columns in the SD_DEM_Temp data frame
colnames(SD_DEM_V_Wind)[colnames(SD_DEM_V_Wind) == "system.index"] <- "DateTime"
colnames(SD_DEM_V_Wind)[colnames(SD_DEM_V_Wind) == "mean"] <- "ERA5_V_Wind_m/s"



SD_DEM_Precip <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Rainfall/ERA5_export_SD_DEM_Precipitation.csv")
# Renaming columns in the SD_DEM_Temp data frame
colnames(SD_DEM_Precip)[colnames(SD_DEM_Precip) == "system.index"] <- "DateTime"
colnames(SD_DEM_Precip)[colnames(SD_DEM_Precip) == "mean"] <- "ERA5_Precip_m"


# List of datasets to combine
datasets <- list(SD_DEM_Solarad, SD_DEM_LWR, SD_DEM_Temp, SD_DEM_Dewpoint, SD_DEM_Pressure, SD_DEM_Precip, SD_DEM_U_Wind, SD_DEM_V_Wind)

# Combine all datasets in the list by "DateTime"
SD_DEM_Combined <- Reduce(function(x, y) merge(x, y, by = "DateTime", all = TRUE), datasets)



# Select columns you want to keep
SD_DEM_ERA5_MET <- SD_DEM_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
SD_DEM_ERA5_MET$DateTime <- as.POSIXct(substr(SD_DEM_ERA5_MET$DateTime, 1, 15), 
                                       format = "%Y%m%dT%H", tz = "UTC")





#Convert variables to appropriate units 

#Convert ERA5_SSR_J/M2 from J/m² to W/m²
SD_DEM_ERA5_MET$`ERA5_SSR_W/M2` <- SD_DEM_ERA5_MET$`ERA5_SSR_J/M2` / 3600


#Convert ERA5_LWR_J/M2 from J/m² to W/m²
SD_DEM_ERA5_MET$`ERA5_LWR_W/M2` <- SD_DEM_ERA5_MET$`ERA5_LWR_J/M2` / 3600


# Convert ERA5_Dewpoint_temp_K from Kelvin to Celsius
SD_DEM_ERA5_MET$ERA5_Dewpoint_temp_C <- SD_DEM_ERA5_MET$ERA5_Dewpoint_temp_K - 273.15


# Convert ERA5_Temp_K from Kelvin to Celsius
SD_DEM_ERA5_MET$ERA5_Temp_C <- SD_DEM_ERA5_MET$ERA5_Temp_K - 273.15


# Convert from m to mm
SD_DEM_ERA5_MET$ERA5_Precip_mm <- SD_DEM_ERA5_MET$ERA5_Precip_m * 1000


# convert from mm to kg m-2 s-1
SD_DEM_ERA5_MET$`ERA5_Precip_kg/m2/s` <- SD_DEM_ERA5_MET$ERA5_Precip_mm / 3600


# Calculate wind speed using the U & V components
SD_DEM_ERA5_MET$`ERA5_Wind_Speed_m/s` <- sqrt(SD_DEM_ERA5_MET$`ERA5_U_Wind_m/s`^2 + SD_DEM_ERA5_MET$`ERA5_V_Wind_m/s`^2)



# Convert Pressure from Pa to hPa
SD_DEM_ERA5_MET <- SD_DEM_ERA5_MET %>%
  mutate(ERA5_Pressure_hPa = ERA5_Pressure_Pa / 100)

# Calculate Actual Vapor Pressure (e) in hPa using Dew Point Temperature in Celsius
SD_DEM_ERA5_MET <- SD_DEM_ERA5_MET %>%
  mutate(e_hPa = 6.112 * exp((17.67 * ERA5_Dewpoint_temp_C) / (ERA5_Dewpoint_temp_C + 243.5)))

# Calculate Specific Humidity (q) in kg/kg
SD_DEM_ERA5_MET <- SD_DEM_ERA5_MET %>%
  mutate(`ERA5_Specific_humidity_kg/kg` = (0.622 * e_hPa) / (ERA5_Pressure_hPa - (0.378 * e_hPa)))




# Interpolate data to get half hourly data'

# Step 1: Generate half-hourly timestamps
time_half_hourly <- seq(from = min(SD_DEM_ERA5_MET$DateTime), 
                        to = max(SD_DEM_ERA5_MET$DateTime), by = "30 mins")

# Step 2: Interpolate each variable to the half-hourly time step

# For Temperature 
temperature_half_hourly <- approx(x = SD_DEM_ERA5_MET$DateTime, 
                                  y = SD_DEM_ERA5_MET$ERA5_Temp_C, 
                                  xout = time_half_hourly)$y

# For Temperature (K)
temperature_K_half_hourly <- approx(x = SD_DEM_ERA5_MET$DateTime, 
                                    y = SD_DEM_ERA5_MET$ERA5_Temp_K, 
                                    xout = time_half_hourly)$y

# For Dewpoint Temperature (C)
dewpoint_half_hourly <- approx(x = SD_DEM_ERA5_MET$DateTime, 
                               y = SD_DEM_ERA5_MET$ERA5_Dewpoint_temp_C, 
                               xout = time_half_hourly)$y

# For Solar Surface Radiation (ERAS_SSR_W/M2)
ssr_half_hourly <- approx(x = SD_DEM_ERA5_MET$DateTime, 
                          y = SD_DEM_ERA5_MET$`ERA5_SSR_W/M2`, 
                          xout = time_half_hourly)$y

# For thermal radiation (ERA5_LWR_W/M2)
lwr_half_hourly <- approx(x = SD_DEM_ERA5_MET$DateTime,
                          y = SD_DEM_ERA5_MET$`ERA5_LWR_W/M2`,
                          xout = time_half_hourly)$y

# For wind speed
wndspd_half_hourly <- approx(x = SD_DEM_ERA5_MET$DateTime,
                             y = SD_DEM_ERA5_MET$`ERA5_Wind_Speed_m/s`,
                             xout = time_half_hourly)$y

# For pressure (Pa)
pressure_Pa_half_hourly <- approx(x = SD_DEM_ERA5_MET$DateTime,
                                  y = SD_DEM_ERA5_MET$`ERA5_Pressure_Pa`,
                                  xout = time_half_hourly)$y

# For pressure (hpa)
pressure_hPa_half_hourly <- approx(x = SD_DEM_ERA5_MET$DateTime,
                                   y = SD_DEM_ERA5_MET$`ERA5_Pressure_hPa`,
                                   xout = time_half_hourly)$y

# For specific humidity
specificHumidity_half_hourly <- approx(x = SD_DEM_ERA5_MET$DateTime,
                                       y = SD_DEM_ERA5_MET$`ERA5_Specific_humidity_kg/kg`,
                                       xout = time_half_hourly)$y


# For precipitation
precipitation_mm_half_hourly <- approx(x = SD_DEM_ERA5_MET$DateTime,
                                       y = SD_DEM_ERA5_MET$ERA5_Precip_mm,
                                       xout = time_half_hourly)$y

# For precipitation kg/m2/s
precipitation_kg_half_hourly <- approx(x = SD_DEM_ERA5_MET$DateTime,
                                       y = SD_DEM_ERA5_MET$`ERA5_Precip_kg/m2/s`,
                                       xout = time_half_hourly)$y


# Step 3: Combine the interpolated data into a new dataframe
SD_DEM_ERA5_MET_HH <- data.frame(
  DateTime = time_half_hourly,
  ERA5_Temp_C = temperature_half_hourly,
  ERA5_Temp_K = temperature_K_half_hourly,
  ERA5_Dewpoint_temp_C = dewpoint_half_hourly,
  `ERA5_SSR_W/M2` = ssr_half_hourly,
  `ERA5_LWR_W/M2` = lwr_half_hourly,
  `ERA5_Specific_humidity_kg/kg` = specificHumidity_half_hourly,
  `ERA5_Pressure_Pa` = pressure_Pa_half_hourly,
  `ERA5_Pressure_hPa` = pressure_hPa_half_hourly,
  `ERA5_Wind_Speed_m/s` = wndspd_half_hourly,
  ERA5_Precip_mm = precipitation_mm_half_hourly,
  `ERA5_Precip_kg/m2/s`= precipitation_kg_half_hourly
)





# Step 4: plot hourly vs interpolated for QC

ggplot() +
  geom_line(data = SD_DEM_ERA5_MET, aes(x = DateTime, y = ERA5_Temp_C), color = "red", size = 1.2) +
  geom_line(data = SD_DEM_ERA5_MET_HH, aes(x = DateTime, y = ERA5_Temp_C), color = "blue", linetype = "dashed", size = 1) +
  labs(title = "Hourly vs Half-Hourly Interpolated Temperature",
       x = "Date", y = "Temperature (C)") +
  theme_minimal()



# Subset the data for a specific time range that exists in the dataset (e.g., one day)
subset_data_hourly <- subset(SD_DEM_ERA5_MET, DateTime >= as.POSIXct("2005-11-11 00:00:00") & DateTime <= as.POSIXct("2005-11-12 00:00:00"))
subset_data_half_hourly <- subset(SD_DEM_ERA5_MET_HH, DateTime >= as.POSIXct("2005-11-11 00:00:00") & DateTime <= as.POSIXct("2005-11-12 00:00:00"))

# Plot the subsetted data
ggplot() +
  geom_point(data = subset_data_hourly, aes(x = DateTime, y = ERA5_Temp_C), color = "red", size = 1.5) +
  geom_point(data = subset_data_half_hourly, aes(x = DateTime, y = ERA5_Temp_C), color = "blue", size = 1) +
  labs(title = "Hourly vs Half-Hourly Interpolated Temperature (Zoomed In)",
       x = "Time", y = "Temperature (°C)") +
  theme_minimal()






# Calculating VPD using land air temperature and dew point temperature 
# Constants
Pmsl <- 1013.25  # mean sea level pressure in hPa
Z_m <- 537.2    # altitude in meters (as provided)

# Calculate VPD for ERA5-Land
calculate_vpd <- function(temp_C, dewpoint_temp_C, Z_m) {
  # Calculate Pmst (equation 6)
  Pmst <- Pmsl * ((temp_C + 273.16) / (temp_C + 273.16 + 0.0065 * Z_m))^5.625
  
  # Corrected formula for fw (equation 5)
  fw <- 1 + 7 * 10^(-4) + 3.46 * 10^(-6) * Pmst
  
  # Calculate SVP (equation 3)
  SVP <- 6.112 * fw * exp((17.67 * temp_C) / (temp_C + 243.5))
  
  # Calculate AVP (equation 4)
  AVP <- 6.112 * fw * exp((17.67 * dewpoint_temp_C) / (dewpoint_temp_C + 243.5))
  
  # Calculate VPD (equation 2)
  VPD <- SVP - AVP
  
  return(VPD)
}

# Apply VPD calculation to the dataset
SD_DEM_ERA5_MET_HH <- SD_DEM_ERA5_MET_HH %>%
  mutate(VPD_hPa = calculate_vpd(ERA5_Temp_C, ERA5_Dewpoint_temp_C, Z_m))








# for SN_DHR  ----

SN_DHR_Solarad <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/SolarRad/ERA5_export_SN_DHR_SSR.csv")
# Renaming columns in the SN_DHR_Solar_rad data frame
colnames(SN_DHR_Solarad)[colnames(SN_DHR_Solarad) == "system.index"] <- "DateTime"
colnames(SN_DHR_Solarad)[colnames(SN_DHR_Solarad) == "mean"] <- "ERA5_SSR_J/M2"



SN_DHR_Dewpoint <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Dewpoint/ERA5_export_dewpoint_SN_DHR.csv")
#Renaming columns in the SN_DHR_dew_point data frame
colnames(SN_DHR_Dewpoint)[colnames(SN_DHR_Dewpoint) == "system.index"] <- "DateTime"
colnames(SN_DHR_Dewpoint)[colnames(SN_DHR_Dewpoint) == "mean"] <- "ERA5_Dewpoint_temp_K"



SN_DHR_Temp <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Temp/ERA5_export_temp_SN_DHR.csv")
# Renaming columns in the SN_DHR_Temp data frame
colnames(SN_DHR_Temp)[colnames(SN_DHR_Temp) == "system.index"] <- "DateTime"
colnames(SN_DHR_Temp)[colnames(SN_DHR_Temp) == "mean"] <- "ERA5_Temp_K"


SN_DHR_LWR <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/ThermalRad/ERA5_export_SN_DHR_LWR.csv")
# Renaming columns in the SN_DHR_Temp data frame
colnames(SN_DHR_LWR)[colnames(SN_DHR_LWR) == "system.index"] <- "DateTime"
colnames(SN_DHR_LWR)[colnames(SN_DHR_LWR) == "mean"] <- "ERA5_LWR_J/M2"



SN_DHR_Pressure <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Pressure/ERA5_export_SN_DHR_Pressure.csv")
# Renaming columns in the SN_DHR_Temp data frame
colnames(SN_DHR_Pressure)[colnames(SN_DHR_Pressure) == "system.index"] <- "DateTime"
colnames(SN_DHR_Pressure)[colnames(SN_DHR_Pressure) == "mean"] <- "ERA5_Pressure_Pa"



SN_DHR_U_Wind <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/U_Wind/ERA5_export_SN_DHR_U_Wind.csv")
# Renaming columns in the SN_DHR_Temp data frame
colnames(SN_DHR_U_Wind)[colnames(SN_DHR_U_Wind) == "system.index"] <- "DateTime"
colnames(SN_DHR_U_Wind)[colnames(SN_DHR_U_Wind) == "mean"] <- "ERA5_U_Wind_m/s"



SN_DHR_V_Wind <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/V_Wind/ERA5_export_SN_DHR_V_Wind.csv")
# Renaming columns in the SN_DHR_Temp data frame
colnames(SN_DHR_V_Wind)[colnames(SN_DHR_V_Wind) == "system.index"] <- "DateTime"
colnames(SN_DHR_V_Wind)[colnames(SN_DHR_V_Wind) == "mean"] <- "ERA5_V_Wind_m/s"



SN_DHR_Precip <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Rainfall/ERA5_export_SN_DHR_Precipitation.csv")
# Renaming columns in the SN_DHR_Temp data frame
colnames(SN_DHR_Precip)[colnames(SN_DHR_Precip) == "system.index"] <- "DateTime"
colnames(SN_DHR_Precip)[colnames(SN_DHR_Precip) == "mean"] <- "ERA5_Precip_m"





# List of datasets to combine
datasets <- list(SN_DHR_Solarad, SN_DHR_LWR, SN_DHR_Temp, SN_DHR_Dewpoint, SN_DHR_Pressure, SN_DHR_Precip, SN_DHR_U_Wind, SN_DHR_V_Wind)

# Combine all datasets in the list by "DateTime"
SN_DHR_Combined <- Reduce(function(x, y) merge(x, y, by = "DateTime", all = TRUE), datasets)



# Select columns you want to keep
SN_DHR_ERA5_MET <- SN_DHR_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
SN_DHR_ERA5_MET$DateTime <- as.POSIXct(substr(SN_DHR_ERA5_MET$DateTime, 1, 15), 
                                       format = "%Y%m%dT%H", tz = "UTC")




#Convert variables to appropriate units 

#Convert ERA5_SSR_J/M2 from J/m² to W/m²
SN_DHR_ERA5_MET$`ERA5_SSR_W/M2` <- SN_DHR_ERA5_MET$`ERA5_SSR_J/M2` / 3600


#Convert ERA5_LWR_J/M2 from J/m² to W/m²
SN_DHR_ERA5_MET$`ERA5_LWR_W/M2` <- SN_DHR_ERA5_MET$`ERA5_LWR_J/M2` / 3600


# Convert ERA5_Dewpoint_temp_K from Kelvin to Celsius
SN_DHR_ERA5_MET$ERA5_Dewpoint_temp_C <- SN_DHR_ERA5_MET$ERA5_Dewpoint_temp_K - 273.15


# Convert ERA5_Temp_K from Kelvin to Celsius
SN_DHR_ERA5_MET$ERA5_Temp_C <- SN_DHR_ERA5_MET$ERA5_Temp_K - 273.15


# convert from m to mm
SN_DHR_ERA5_MET$ERA5_Precip_mm <- SN_DHR_ERA5_MET$ERA5_Precip_m * 1000


# convert from mm to kg m-2 s-1
SN_DHR_ERA5_MET$`ERA5_Precip_kg/m2/s` <- SN_DHR_ERA5_MET$ERA5_Precip_mm / 3600


# Calculate wind speed using the U & V components
SN_DHR_ERA5_MET$`ERA5_Wind_Speed_m/s` <- sqrt(SN_DHR_ERA5_MET$`ERA5_U_Wind_m/s`^2 + SN_DHR_ERA5_MET$`ERA5_V_Wind_m/s`^2)



# Convert Pressure from Pa to hPa
SN_DHR_ERA5_MET <- SN_DHR_ERA5_MET %>%
  mutate(ERA5_Pressure_hPa = ERA5_Pressure_Pa / 100)

# Calculate Actual Vapor Pressure (e) in hPa using Dew Point Temperature in Celsius
SN_DHR_ERA5_MET <- SN_DHR_ERA5_MET %>%
  mutate(e_hPa = 6.112 * exp((17.67 * ERA5_Dewpoint_temp_C) / (ERA5_Dewpoint_temp_C + 243.5)))

# Calculate Specific Humidity (q) in kg/kg
SN_DHR_ERA5_MET <- SN_DHR_ERA5_MET %>%
  mutate(`ERA5_Specific_humidity_kg/kg` = (0.622 * e_hPa) / (ERA5_Pressure_hPa - (0.378 * e_hPa)))





# Interpolate data to get half hourly data'

# Step 1: Generate half-hourly timestamps
time_half_hourly <- seq(from = min(SN_DHR_ERA5_MET$DateTime), 
                        to = max(SN_DHR_ERA5_MET$DateTime), by = "30 mins")

# Step 2: Interpolate each variable to the half-hourly time step

# For Temperature (C)
temperature_half_hourly <- approx(x = SN_DHR_ERA5_MET$DateTime, 
                                  y = SN_DHR_ERA5_MET$ERA5_Temp_C, 
                                  xout = time_half_hourly)$y

# For Temperature (K)
temperature_K_half_hourly <- approx(x = SN_DHR_ERA5_MET$DateTime, 
                                    y = SN_DHR_ERA5_MET$ERA5_Temp_K, 
                                    xout = time_half_hourly)$y


# For Dewpoint Temperature 
dewpoint_half_hourly <- approx(x = SN_DHR_ERA5_MET$DateTime, 
                               y = SN_DHR_ERA5_MET$ERA5_Dewpoint_temp_C, 
                               xout = time_half_hourly)$y

# For Solar Surface Radiation (ERAS_SSR_W/M2)
ssr_half_hourly <- approx(x = SN_DHR_ERA5_MET$DateTime, 
                          y = SN_DHR_ERA5_MET$`ERA5_SSR_W/M2`, 
                          xout = time_half_hourly)$y

# For thermal radiation (ERA5_LWR_W/M2)
lwr_half_hourly <- approx(x = SN_DHR_ERA5_MET$DateTime,
                          y = SN_DHR_ERA5_MET$`ERA5_LWR_W/M2`,
                          xout = time_half_hourly)$y

# For wind speed
wndspd_half_hourly <- approx(x = SN_DHR_ERA5_MET$DateTime,
                             y = SN_DHR_ERA5_MET$`ERA5_Wind_Speed_m/s`,
                             xout = time_half_hourly)$y

# For pressure (Pa)
pressure_Pa_half_hourly <- approx(x = SN_DHR_ERA5_MET$DateTime,
                                  y = SN_DHR_ERA5_MET$`ERA5_Pressure_Pa`,
                                  xout = time_half_hourly)$y

# For pressure (hpa)
pressure_hPa_half_hourly <- approx(x = SN_DHR_ERA5_MET$DateTime,
                                   y = SN_DHR_ERA5_MET$`ERA5_Pressure_hPa`,
                                   xout = time_half_hourly)$y

# For specific humidity
specificHumidity_half_hourly <- approx(x = SN_DHR_ERA5_MET$DateTime,
                                       y = SN_DHR_ERA5_MET$`ERA5_Specific_humidity_kg/kg`,
                                       xout = time_half_hourly)$y


# For precipitation
precipitation_mm_half_hourly <- approx(x = SN_DHR_ERA5_MET$DateTime,
                                       y = SN_DHR_ERA5_MET$ERA5_Precip_mm,
                                       xout = time_half_hourly)$y


# For precipitation kg/m2/s
precipitation_kg_half_hourly <- approx(x = SN_DHR_ERA5_MET$DateTime,
                                       y = SN_DHR_ERA5_MET$`ERA5_Precip_kg/m2/s`,
                                       xout = time_half_hourly)$y



# Step 3: Combine the interpolated data into a new dataframe
SN_DHR_ERA5_MET_HH <- data.frame(
  DateTime = time_half_hourly,
  ERA5_Temp_C = temperature_half_hourly,
  ERA5_Temp_K = temperature_K_half_hourly,
  ERA5_Dewpoint_temp_C = dewpoint_half_hourly,
  `ERA5_SSR_W/M2` = ssr_half_hourly,
  `ERA5_LWR_W/M2` = lwr_half_hourly,
  `ERA5_Specific_humidity_kg/kg` = specificHumidity_half_hourly,
  `ERA5_Pressure_Pa` = pressure_Pa_half_hourly,
  `ERA5_Pressure_hPa` = pressure_hPa_half_hourly,
  `ERA5_Wind_Speed_m/s` = wndspd_half_hourly,
  ERA5_Precip_mm = precipitation_mm_half_hourly,
  `ERA5_Precip_kg/m2/s`= precipitation_kg_half_hourly
)





# Step 4: plot hourly vs interpolated for QC

ggplot() +
  geom_line(data = SN_DHR_ERA5_MET, aes(x = DateTime, y = ERA5_Temp_C), color = "red", size = 1.2) +
  geom_line(data = SN_DHR_ERA5_MET_HH, aes(x = DateTime, y = ERA5_Temp_C), color = "blue", linetype = "dashed", size = 1) +
  labs(title = "Hourly vs Half-Hourly Interpolated Temperature",
       x = "Date", y = "Temperature (C)") +
  theme_minimal()



# Subset the data for a specific time range that exists in the dataset (e.g., one day)
subset_data_hourly <- subset(SN_DHR_ERA5_MET, DateTime >= as.POSIXct("2010-11-11 00:00:00") & DateTime <= as.POSIXct("2010-11-12 00:00:00"))
subset_data_half_hourly <- subset(SN_DHR_ERA5_MET_HH, DateTime >= as.POSIXct("2010-11-11 00:00:00") & DateTime <= as.POSIXct("2010-11-12 00:00:00"))

# Plot the subsetted data
ggplot() +
  geom_point(data = subset_data_hourly, aes(x = DateTime, y = ERA5_Temp_C), color = "red", size = 1.5) +
  geom_point(data = subset_data_half_hourly, aes(x = DateTime, y = ERA5_Temp_C), color = "blue", size = 1) +
  labs(title = "Hourly vs Half-Hourly Interpolated Temperature (Zoomed In)",
       x = "Time", y = "Temperature (°C)") +
  theme_minimal()





# Calculating VPD using land air temperature and dew point temperature 
# Constants
Pmsl <- 1013.25  # mean sea level pressure in hPa
Z_m <- 42.12    # altitude in meters (as provided)

# Calculate VPD for ERA5-Land
calculate_vpd <- function(temp_C, dewpoint_temp_C, Z_m) {
  # Calculate Pmst (equation 6)
  Pmst <- Pmsl * ((temp_C + 273.16) / (temp_C + 273.16 + 0.0065 * Z_m))^5.625
  
  # Corrected formula for fw (equation 5)
  fw <- 1 + 7 * 10^(-4) + 3.46 * 10^(-6) * Pmst
  
  # Calculate SVP (equation 3)
  SVP <- 6.112 * fw * exp((17.67 * temp_C) / (temp_C + 243.5))
  
  # Calculate AVP (equation 4)
  AVP <- 6.112 * fw * exp((17.67 * dewpoint_temp_C) / (dewpoint_temp_C + 243.5))
  
  # Calculate VPD (equation 2)
  VPD <- SVP - AVP
  
  return(VPD)
}

# Apply VPD calculation to the dataset
SN_DHR_ERA5_MET_HH <- SN_DHR_ERA5_MET_HH %>%
  mutate(VPD_hPa = calculate_vpd(ERA5_Temp_C, ERA5_Dewpoint_temp_C, Z_m))








# for SN_NKR  ----

SN_NKR_Solarad <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/SolarRad/ERA5_export_SN_NKR_SSR.csv")
# Renaming columns in the SN_NKR_Solar_rad data frame
colnames(SN_NKR_Solarad)[colnames(SN_NKR_Solarad) == "system.index"] <- "DateTime"
colnames(SN_NKR_Solarad)[colnames(SN_NKR_Solarad) == "mean"] <- "ERA5_SSR_J/M2"



SN_NKR_Dewpoint <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Dewpoint/ERA5_export_dewpoint_SN_NKR.csv")
#Renaming columns in the SN_NKR_dew_point data frame
colnames(SN_NKR_Dewpoint)[colnames(SN_NKR_Dewpoint) == "system.index"] <- "DateTime"
colnames(SN_NKR_Dewpoint)[colnames(SN_NKR_Dewpoint) == "mean"] <- "ERA5_Dewpoint_temp_K"



SN_NKR_Temp <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Temp/ERA5_export_temp_SN_NKR.csv")
# Renaming columns in the SN_NKR_Temp data frame
colnames(SN_NKR_Temp)[colnames(SN_NKR_Temp) == "system.index"] <- "DateTime"
colnames(SN_NKR_Temp)[colnames(SN_NKR_Temp) == "mean"] <- "ERA5_Temp_K"


SN_NKR_LWR <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/ThermalRad/ERA5_export_SN_NKR_LWR.csv")
# Renaming columns in the SN_NKR_Temp data frame
colnames(SN_NKR_LWR)[colnames(SN_NKR_LWR) == "system.index"] <- "DateTime"
colnames(SN_NKR_LWR)[colnames(SN_NKR_LWR) == "mean"] <- "ERA5_LWR_J/M2"



SN_NKR_Pressure <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Pressure/ERA5_export_SN_NKR_Pressure.csv")
# Renaming columns in the SN_NKR_Temp data frame
colnames(SN_NKR_Pressure)[colnames(SN_NKR_Pressure) == "system.index"] <- "DateTime"
colnames(SN_NKR_Pressure)[colnames(SN_NKR_Pressure) == "mean"] <- "ERA5_Pressure_Pa"



SN_NKR_U_Wind <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/U_Wind/ERA5_export_SN_NKR_U_Wind.csv")
# Renaming columns in the SN_NKR_Temp data frame
colnames(SN_NKR_U_Wind)[colnames(SN_NKR_U_Wind) == "system.index"] <- "DateTime"
colnames(SN_NKR_U_Wind)[colnames(SN_NKR_U_Wind) == "mean"] <- "ERA5_U_Wind_m/s"



SN_NKR_V_Wind <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/V_Wind/ERA5_export_SN_NKR_V_Wind.csv")
# Renaming columns in the SN_NKR_Temp data frame
colnames(SN_NKR_V_Wind)[colnames(SN_NKR_V_Wind) == "system.index"] <- "DateTime"
colnames(SN_NKR_V_Wind)[colnames(SN_NKR_V_Wind) == "mean"] <- "ERA5_V_Wind_m/s"



SN_NKR_Precip <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Rainfall/ERA5_export_SN_NKR_Precipitation.csv")
# Renaming columns in the SN_NKR_Temp data frame
colnames(SN_NKR_Precip)[colnames(SN_NKR_Precip) == "system.index"] <- "DateTime"
colnames(SN_NKR_Precip)[colnames(SN_NKR_Precip) == "mean"] <- "ERA5_Precip_m"



# List of datasets to combine
datasets <- list(SN_NKR_Solarad, SN_NKR_LWR, SN_NKR_Temp, SN_NKR_Dewpoint, SN_NKR_Pressure, SN_NKR_Precip, SN_NKR_U_Wind, SN_NKR_V_Wind)

# Combine all datasets in the list by "DateTime"
SN_NKR_Combined <- Reduce(function(x, y) merge(x, y, by = "DateTime", all = TRUE), datasets)




# Select columns you want to keep
SN_NKR_ERA5_MET <- SN_NKR_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
SN_NKR_ERA5_MET$DateTime <- as.POSIXct(substr(SN_NKR_ERA5_MET$DateTime, 1, 15), 
                                       format = "%Y%m%dT%H", tz = "UTC")




#Convert variables to appropriate units 

#Convert ERA5_SSR_J/M2 from J/m² to W/m²
SN_NKR_ERA5_MET$`ERA5_SSR_W/M2` <- SN_NKR_ERA5_MET$`ERA5_SSR_J/M2` / 3600


#Convert ERA5_LWR_J/M2 from J/m² to W/m²
SN_NKR_ERA5_MET$`ERA5_LWR_W/M2` <- SN_NKR_ERA5_MET$`ERA5_LWR_J/M2` / 3600


# Convert ERA5_Dewpoint_temp_K from Kelvin to Celsius
SN_NKR_ERA5_MET$ERA5_Dewpoint_temp_C <- SN_NKR_ERA5_MET$ERA5_Dewpoint_temp_K - 273.15


# Convert ERA5_Temp_K from Kelvin to Celsius
SN_NKR_ERA5_MET$ERA5_Temp_C <- SN_NKR_ERA5_MET$ERA5_Temp_K - 273.15


# Convert from m to mm
SN_NKR_ERA5_MET$ERA5_Precip_mm <- SN_NKR_ERA5_MET$ERA5_Precip_m * 1000


# convert from mm to kg m-2 s-1
SN_NKR_ERA5_MET$`ERA5_Precip_kg/m2/s` <- SN_NKR_ERA5_MET$ERA5_Precip_mm / 3600


# Calculate wind speed using the U & V components
SN_NKR_ERA5_MET$`ERA5_Wind_Speed_m/s` <- sqrt(SN_NKR_ERA5_MET$`ERA5_U_Wind_m/s`^2 + SN_NKR_ERA5_MET$`ERA5_V_Wind_m/s`^2)



# Convert Pressure from Pa to hPa
SN_NKR_ERA5_MET <- SN_NKR_ERA5_MET %>%
  mutate(ERA5_Pressure_hPa = ERA5_Pressure_Pa / 100)

# Calculate Actual Vapor Pressure (e) in hPa using Dew Point Temperature in Celsius
SN_NKR_ERA5_MET <- SN_NKR_ERA5_MET %>%
  mutate(e_hPa = 6.112 * exp((17.67 * ERA5_Dewpoint_temp_C) / (ERA5_Dewpoint_temp_C + 243.5)))

# Calculate Specific Humidity (q) in kg/kg
SN_NKR_ERA5_MET <- SN_NKR_ERA5_MET %>%
  mutate(`ERA5_Specific_humidity_kg/kg` = (0.622 * e_hPa) / (ERA5_Pressure_hPa - (0.378 * e_hPa)))





# Interpolate data to get half hourly data'

# Step 1: Generate half-hourly timestamps
time_half_hourly <- seq(from = min(SN_NKR_ERA5_MET$DateTime), 
                        to = max(SN_NKR_ERA5_MET$DateTime), by = "30 mins")

# Step 2: Interpolate each variable to the half-hourly time step

# For Temperature (C)
temperature_half_hourly <- approx(x = SN_NKR_ERA5_MET$DateTime, 
                                  y = SN_NKR_ERA5_MET$ERA5_Temp_C, 
                                  xout = time_half_hourly)$y

# For Temperature (K)
temperature_K_half_hourly <- approx(x = SN_NKR_ERA5_MET$DateTime, 
                                    y = SN_NKR_ERA5_MET$ERA5_Temp_K, 
                                    xout = time_half_hourly)$y

# For Dewpoint Temperature 
dewpoint_half_hourly <- approx(x = SN_NKR_ERA5_MET$DateTime, 
                               y = SN_NKR_ERA5_MET$ERA5_Dewpoint_temp_C, 
                               xout = time_half_hourly)$y

# For Solar Surface Radiation (ERAS_SSR_W/M2)
ssr_half_hourly <- approx(x = SN_NKR_ERA5_MET$DateTime, 
                          y = SN_NKR_ERA5_MET$`ERA5_SSR_W/M2`, 
                          xout = time_half_hourly)$y

# For thermal radiation (ERA5_LWR_W/M2)
lwr_half_hourly <- approx(x = SN_NKR_ERA5_MET$DateTime,
                          y = SN_NKR_ERA5_MET$`ERA5_LWR_W/M2`,
                          xout = time_half_hourly)$y

# For wind speed
wndspd_half_hourly <- approx(x = SN_NKR_ERA5_MET$DateTime,
                             y = SN_NKR_ERA5_MET$`ERA5_Wind_Speed_m/s`,
                             xout = time_half_hourly)$y

# For pressure (Pa)
pressure_Pa_half_hourly <- approx(x = SN_NKR_ERA5_MET$DateTime,
                                  y = SN_NKR_ERA5_MET$`ERA5_Pressure_Pa`,
                                  xout = time_half_hourly)$y

# For pressure (hpa)
pressure_hPa_half_hourly <- approx(x = SN_NKR_ERA5_MET$DateTime,
                                   y = SN_NKR_ERA5_MET$`ERA5_Pressure_hPa`,
                                   xout = time_half_hourly)$y

# For specific humidity
specificHumidity_half_hourly <- approx(x = SN_NKR_ERA5_MET$DateTime,
                                       y = SN_NKR_ERA5_MET$`ERA5_Specific_humidity_kg/kg`,
                                       xout = time_half_hourly)$y

# For precipitation
precipitation_mm_half_hourly <- approx(x = SN_NKR_ERA5_MET$DateTime,
                                       y = SN_NKR_ERA5_MET$ERA5_Precip_mm,
                                       xout = time_half_hourly)$y

# For precipitation kg/m2/s
precipitation_kg_half_hourly <- approx(x = SN_NKR_ERA5_MET$DateTime,
                                       y = SN_NKR_ERA5_MET$`ERA5_Precip_kg/m2/s`,
                                       xout = time_half_hourly)$y


# Step 3: Combine the interpolated data into a new dataframe
SN_NKR_ERA5_MET_HH <- data.frame(
  DateTime = time_half_hourly,
  ERA5_Temp_C = temperature_half_hourly,
  ERA5_Temp_K = temperature_K_half_hourly,
  ERA5_Dewpoint_temp_C = dewpoint_half_hourly,
  `ERA5_SSR_W/M2` = ssr_half_hourly,
  `ERA5_LWR_W/M2` = lwr_half_hourly,
  `ERA5_Specific_humidity_kg/kg` = specificHumidity_half_hourly,
  `ERA5_Pressure_Pa` = pressure_Pa_half_hourly,
  `ERA5_Pressure_hPa` = pressure_hPa_half_hourly,
  `ERA5_Wind_Speed_m/s` = wndspd_half_hourly,
  ERA5_Precip_mm = precipitation_mm_half_hourly,
  `ERA5_Precip_kg/m2/s`= precipitation_kg_half_hourly
)




# Step 4: plot hourly vs interpolated for QC

ggplot() +
  geom_line(data = SN_NKR_ERA5_MET, aes(x = DateTime, y = ERA5_Temp_C), color = "red", size = 1.2) +
  geom_line(data = SN_NKR_ERA5_MET_HH, aes(x = DateTime, y = ERA5_Temp_C), color = "blue", linetype = "dashed", size = 1) +
  labs(title = "Hourly vs Half-Hourly Interpolated Temperature",
       x = "Date", y = "Temperature (C)") +
  theme_minimal()



# Subset the data for a specific time range that exists in the dataset (e.g., one day)
subset_data_hourly <- subset(SN_NKR_ERA5_MET, DateTime >= as.POSIXct("2019-11-11 00:00:00") & DateTime <= as.POSIXct("2019-11-12 00:00:00"))
subset_data_half_hourly <- subset(SN_NKR_ERA5_MET_HH, DateTime >= as.POSIXct("2019-11-11 00:00:00") & DateTime <= as.POSIXct("2019-11-12 00:00:00"))

# Plot the subsetted data
ggplot() +
  geom_point(data = subset_data_hourly, aes(x = DateTime, y = ERA5_Temp_C), color = "red", size = 1.5) +
  geom_point(data = subset_data_half_hourly, aes(x = DateTime, y = ERA5_Temp_C), color = "blue", size = 1) +
  labs(title = "Hourly vs Half-Hourly Interpolated Temperature (Zoomed In)",
       x = "Time", y = "Temperature (°C)") +
  theme_minimal()






# Calculating VPD using land air temperature and dew point temperature 
# Constants
Pmsl <- 1013.25  # mean sea level pressure in hPa
Z_m <- 8.71    # altitude in meters (as provided)

# Calculate VPD for ERA5-Land
calculate_vpd <- function(temp_C, dewpoint_temp_C, Z_m) {
  # Calculate Pmst (equation 6)
  Pmst <- Pmsl * ((temp_C + 273.16) / (temp_C + 273.16 + 0.0065 * Z_m))^5.625
  
  # Corrected formula for fw (equation 5)
  fw <- 1 + 7 * 10^(-4) + 3.46 * 10^(-6) * Pmst
  
  # Calculate SVP (equation 3)
  SVP <- 6.112 * fw * exp((17.67 * temp_C) / (temp_C + 243.5))
  
  # Calculate AVP (equation 4)
  AVP <- 6.112 * fw * exp((17.67 * dewpoint_temp_C) / (dewpoint_temp_C + 243.5))
  
  # Calculate VPD (equation 2)
  VPD <- SVP - AVP
  
  return(VPD)
}

# Apply VPD calculation to the dataset
SN_NKR_ERA5_MET_HH <- SN_NKR_ERA5_MET_HH %>%
  mutate(VPD_hPa = calculate_vpd(ERA5_Temp_C, ERA5_Dewpoint_temp_C, Z_m))








# for SN_RAG  ----

SN_RAG_Solarad <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/SolarRad/ERA5_export_SN_RAG_SSR.csv")
# Renaming columns in the SN_RAG_Solar_rad data frame
colnames(SN_RAG_Solarad)[colnames(SN_RAG_Solarad) == "system.index"] <- "DateTime"
colnames(SN_RAG_Solarad)[colnames(SN_RAG_Solarad) == "mean"] <- "ERA5_SSR_J/M2"



SN_RAG_Dewpoint <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Dewpoint/ERA5_export_dewpoint_SN_RAG.csv")
#Renaming columns in the SN_RAG_dew_point data frame
colnames(SN_RAG_Dewpoint)[colnames(SN_RAG_Dewpoint) == "system.index"] <- "DateTime"
colnames(SN_RAG_Dewpoint)[colnames(SN_RAG_Dewpoint) == "mean"] <- "ERA5_Dewpoint_temp_K"



SN_RAG_Temp <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Temp/ERA5_export_temp_SN_RAG.csv")
# Renaming columns in the SN_RAG_Temp data frame
colnames(SN_RAG_Temp)[colnames(SN_RAG_Temp) == "system.index"] <- "DateTime"
colnames(SN_RAG_Temp)[colnames(SN_RAG_Temp) == "mean"] <- "ERA5_Temp_K"


SN_RAG_LWR <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/ThermalRad/ERA5_export_SN_RAG_LWR.csv")
# Renaming columns in the SN_RAG_Temp data frame
colnames(SN_RAG_LWR)[colnames(SN_RAG_LWR) == "system.index"] <- "DateTime"
colnames(SN_RAG_LWR)[colnames(SN_RAG_LWR) == "mean"] <- "ERA5_LWR_J/M2"



SN_RAG_Pressure <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Pressure/ERA5_export_SN_RAG_Pressure.csv")
# Renaming columns in the SN_RAG_Temp data frame
colnames(SN_RAG_Pressure)[colnames(SN_RAG_Pressure) == "system.index"] <- "DateTime"
colnames(SN_RAG_Pressure)[colnames(SN_RAG_Pressure) == "mean"] <- "ERA5_Pressure_Pa"



SN_RAG_U_Wind <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/U_Wind/ERA5_export_SN_RAG_U_Wind.csv")
# Renaming columns in the SN_RAG_Temp data frame
colnames(SN_RAG_U_Wind)[colnames(SN_RAG_U_Wind) == "system.index"] <- "DateTime"
colnames(SN_RAG_U_Wind)[colnames(SN_RAG_U_Wind) == "mean"] <- "ERA5_U_Wind_m/s"



SN_RAG_V_Wind <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/V_Wind/ERA5_export_SN_RAG_V_Wind.csv")
# Renaming columns in the SN_RAG_Temp data frame
colnames(SN_RAG_V_Wind)[colnames(SN_RAG_V_Wind) == "system.index"] <- "DateTime"
colnames(SN_RAG_V_Wind)[colnames(SN_RAG_V_Wind) == "mean"] <- "ERA5_V_Wind_m/s"



SN_RAG_Precip <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Rainfall/ERA5_export_SN_RAG_Precipitation.csv")
# Renaming columns in the SN_RAG_Temp data frame
colnames(SN_RAG_Precip)[colnames(SN_RAG_Precip) == "system.index"] <- "DateTime"
colnames(SN_RAG_Precip)[colnames(SN_RAG_Precip) == "mean"] <- "ERA5_Precip_m"




# List of datasets to combine
datasets <- list(SN_RAG_Solarad, SN_RAG_LWR, SN_RAG_Temp, SN_RAG_Dewpoint, SN_RAG_Pressure, SN_RAG_Precip, SN_RAG_U_Wind, SN_RAG_V_Wind)

# Combine all datasets in the list by "DateTime"
SN_RAG_Combined <- Reduce(function(x, y) merge(x, y, by = "DateTime", all = TRUE), datasets)


# Select columns you want to keep
SN_RAG_ERA5_MET <- SN_RAG_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
SN_RAG_ERA5_MET$DateTime <- as.POSIXct(substr(SN_RAG_ERA5_MET$DateTime, 1, 15), 
                                       format = "%Y%m%dT%H", tz = "UTC")





#Convert variables to appropriate units 

#Convert ERA5_SSR_J/M2 from J/m² to W/m²
SN_RAG_ERA5_MET$`ERA5_SSR_W/M2` <- SN_RAG_ERA5_MET$`ERA5_SSR_J/M2` / 3600


#Convert ERA5_LWR_J/M2 from J/m² to W/m²
SN_RAG_ERA5_MET$`ERA5_LWR_W/M2` <- SN_RAG_ERA5_MET$`ERA5_LWR_J/M2` / 3600


# Convert ERA5_Dewpoint_temp_K from Kelvin to Celsius
SN_RAG_ERA5_MET$ERA5_Dewpoint_temp_C <- SN_RAG_ERA5_MET$ERA5_Dewpoint_temp_K - 273.15


# Convert ERA5_Temp_K from Kelvin to Celsius
SN_RAG_ERA5_MET$ERA5_Temp_C <- SN_RAG_ERA5_MET$ERA5_Temp_K - 273.15


# Convert from m to mm
SN_RAG_ERA5_MET$ERA5_Precip_mm <- SN_RAG_ERA5_MET$ERA5_Precip_m * 1000


# convert from mm to kg m-2 s-1
SN_RAG_ERA5_MET$`ERA5_Precip_kg/m2/s` <- SN_RAG_ERA5_MET$ERA5_Precip_mm / 3600


# Calculate wind speed using the U & V components
SN_RAG_ERA5_MET$`ERA5_Wind_Speed_m/s` <- sqrt(SN_RAG_ERA5_MET$`ERA5_U_Wind_m/s`^2 + SN_RAG_ERA5_MET$`ERA5_V_Wind_m/s`^2)



# Convert Pressure from Pa to hPa
SN_RAG_ERA5_MET <- SN_RAG_ERA5_MET %>%
  mutate(ERA5_Pressure_hPa = ERA5_Pressure_Pa / 100)

# Calculate Actual Vapor Pressure (e) in hPa using Dew Point Temperature in Celsius
SN_RAG_ERA5_MET <- SN_RAG_ERA5_MET %>%
  mutate(e_hPa = 6.112 * exp((17.67 * ERA5_Dewpoint_temp_C) / (ERA5_Dewpoint_temp_C + 243.5)))

# Calculate Specific Humidity (q) in kg/kg
SN_RAG_ERA5_MET <- SN_RAG_ERA5_MET %>%
  mutate(`ERA5_Specific_humidity_kg/kg` = (0.622 * e_hPa) / (ERA5_Pressure_hPa - (0.378 * e_hPa)))





# Interpolate data to get half hourly data'

# Step 1: Generate half-hourly timestamps
time_half_hourly <- seq(from = min(SN_RAG_ERA5_MET$DateTime), 
                        to = max(SN_RAG_ERA5_MET$DateTime), by = "30 mins")

# Step 2: Interpolate each variable to the half-hourly time step

# For Temperature (C)
temperature_half_hourly <- approx(x = SN_RAG_ERA5_MET$DateTime, 
                                  y = SN_RAG_ERA5_MET$ERA5_Temp_C, 
                                  xout = time_half_hourly)$y

# For Temperature (K)
temperature_K_half_hourly <- approx(x = SN_RAG_ERA5_MET$DateTime, 
                                    y = SN_RAG_ERA5_MET$ERA5_Temp_K, 
                                    xout = time_half_hourly)$y

# For Dewpoint Temperature 
dewpoint_half_hourly <- approx(x = SN_RAG_ERA5_MET$DateTime, 
                               y = SN_RAG_ERA5_MET$ERA5_Dewpoint_temp_C, 
                               xout = time_half_hourly)$y

# For Solar Surface Radiation (ERAS_SSR_W/M2)
ssr_half_hourly <- approx(x = SN_RAG_ERA5_MET$DateTime, 
                          y = SN_RAG_ERA5_MET$`ERA5_SSR_W/M2`, 
                          xout = time_half_hourly)$y

# For thermal radiation (ERA5_LWR_W/M2)
lwr_half_hourly <- approx(x = SN_RAG_ERA5_MET$DateTime,
                          y = SN_RAG_ERA5_MET$`ERA5_LWR_W/M2`,
                          xout = time_half_hourly)$y

# For wind speed
wndspd_half_hourly <- approx(x = SN_RAG_ERA5_MET$DateTime,
                             y = SN_RAG_ERA5_MET$`ERA5_Wind_Speed_m/s`,
                             xout = time_half_hourly)$y

# For pressure (Pa)
pressure_Pa_half_hourly <- approx(x = SN_RAG_ERA5_MET$DateTime,
                                  y = SN_RAG_ERA5_MET$`ERA5_Pressure_Pa`,
                                  xout = time_half_hourly)$y

# For pressure (hpa)
pressure_hPa_half_hourly <- approx(x = SN_RAG_ERA5_MET$DateTime,
                                   y = SN_RAG_ERA5_MET$`ERA5_Pressure_hPa`,
                                   xout = time_half_hourly)$y

# For specific humidity
specificHumidity_half_hourly <- approx(x = SN_RAG_ERA5_MET$DateTime,
                                       y = SN_RAG_ERA5_MET$`ERA5_Specific_humidity_kg/kg`,
                                       xout = time_half_hourly)$y

# For precipitation
precipitation_mm_half_hourly <- approx(x = SN_RAG_ERA5_MET$DateTime,
                                       y = SN_RAG_ERA5_MET$ERA5_Precip_mm,
                                       xout = time_half_hourly)$y


# For precipitation kg/m2/s
precipitation_kg_half_hourly <- approx(x = SN_RAG_ERA5_MET$DateTime,
                                       y = SN_RAG_ERA5_MET$`ERA5_Precip_kg/m2/s`,
                                       xout = time_half_hourly)$y


# Step 3: Combine the interpolated data into a new dataframe
SN_RAG_ERA5_MET_HH <- data.frame(
  DateTime = time_half_hourly,
  ERA5_Temp_C = temperature_half_hourly,
  ERA5_Temp_K = temperature_K_half_hourly,
  ERA5_Dewpoint_temp_C = dewpoint_half_hourly,
  `ERA5_SSR_W/M2` = ssr_half_hourly,
  `ERA5_LWR_W/M2` = lwr_half_hourly,
  `ERA5_Specific_humidity_kg/kg` = specificHumidity_half_hourly,
  `ERA5_Pressure_Pa` = pressure_Pa_half_hourly,
  `ERA5_Pressure_hPa` = pressure_hPa_half_hourly,
  `ERA5_Wind_Speed_m/s` = wndspd_half_hourly,
  ERA5_Precip_mm = precipitation_mm_half_hourly,
  `ERA5_Precip_kg/m2/s`= precipitation_kg_half_hourly
)




# Step 4: plot hourly vs interpolated for QC

ggplot() +
  geom_line(data = SN_RAG_ERA5_MET, aes(x = DateTime, y = ERA5_Temp_C), color = "red", size = 1.2) +
  geom_line(data = SN_RAG_ERA5_MET_HH, aes(x = DateTime, y = ERA5_Temp_C), color = "blue", linetype = "dashed", size = 1) +
  labs(title = "Hourly vs Half-Hourly Interpolated Temperature",
       x = "Date", y = "Temperature (C)") +
  theme_minimal()



# Subset the data for a specific time range that exists in the dataset (e.g., one day)
subset_data_hourly <- subset(SN_RAG_ERA5_MET, DateTime >= as.POSIXct("2019-11-11 00:00:00") & DateTime <= as.POSIXct("2019-11-12 00:00:00"))
subset_data_half_hourly <- subset(SN_RAG_ERA5_MET_HH, DateTime >= as.POSIXct("2019-11-11 00:00:00") & DateTime <= as.POSIXct("2019-11-12 00:00:00"))

# Plot the subsetted data
ggplot() +
  geom_point(data = subset_data_hourly, aes(x = DateTime, y = ERA5_Temp_C), color = "red", size = 1.5) +
  geom_point(data = subset_data_half_hourly, aes(x = DateTime, y = ERA5_Temp_C), color = "blue", size = 1) +
  labs(title = "Hourly vs Half-Hourly Interpolated Temperature (Zoomed In)",
       x = "Time", y = "Temperature (°C)") +
  theme_minimal()





# Calculating VPD using land air temperature and dew point temperature 
# Constants
Pmsl <- 1013.25  # mean sea level pressure in hPa
Z_m <- 8.16    # altitude in meters (as provided)

# Calculate VPD for ERA5-Land
calculate_vpd <- function(temp_C, dewpoint_temp_C, Z_m) {
  # Calculate Pmst (equation 6)
  Pmst <- Pmsl * ((temp_C + 273.16) / (temp_C + 273.16 + 0.0065 * Z_m))^5.625
  
  # Corrected formula for fw (equation 5)
  fw <- 1 + 7 * 10^(-4) + 3.46 * 10^(-6) * Pmst
  
  # Calculate SVP (equation 3)
  SVP <- 6.112 * fw * exp((17.67 * temp_C) / (temp_C + 243.5))
  
  # Calculate AVP (equation 4)
  AVP <- 6.112 * fw * exp((17.67 * dewpoint_temp_C) / (dewpoint_temp_C + 243.5))
  
  # Calculate VPD (equation 2)
  VPD <- SVP - AVP
  
  return(VPD)
}

# Apply VPD calculation to the dataset
SN_RAG_ERA5_MET_HH <- SN_RAG_ERA5_MET_HH %>%
  mutate(VPD_hPa = calculate_vpd(ERA5_Temp_C, ERA5_Dewpoint_temp_C, Z_m))








# for UG_JIN  ----

UG_JIN_Solarad <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/SolarRad/ERA5_export_UG_JIN_SSR.csv")
# Renaming columns in the UG_JIN_Solar_rad data frame
colnames(UG_JIN_Solarad)[colnames(UG_JIN_Solarad) == "system.index"] <- "DateTime"
colnames(UG_JIN_Solarad)[colnames(UG_JIN_Solarad) == "mean"] <- "ERA5_SSR_J/M2"



UG_JIN_Dewpoint <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Dewpoint/ERA5_export_dewpoint_UG_JIN.csv")
#Renaming columns in the UG_JIN_dew_point data frame
colnames(UG_JIN_Dewpoint)[colnames(UG_JIN_Dewpoint) == "system.index"] <- "DateTime"
colnames(UG_JIN_Dewpoint)[colnames(UG_JIN_Dewpoint) == "mean"] <- "ERA5_Dewpoint_temp_K"



UG_JIN_Temp <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Temp/ERA5_export_temp_UG_JIN.csv")
# Renaming columns in the UG_JIN_Temp data frame
colnames(UG_JIN_Temp)[colnames(UG_JIN_Temp) == "system.index"] <- "DateTime"
colnames(UG_JIN_Temp)[colnames(UG_JIN_Temp) == "mean"] <- "ERA5_Temp_K"


UG_JIN_LWR <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/ThermalRad/ERA5_export_UG_JIN_LWR.csv")
# Renaming columns in the UG_JIN_Temp data frame
colnames(UG_JIN_LWR)[colnames(UG_JIN_LWR) == "system.index"] <- "DateTime"
colnames(UG_JIN_LWR)[colnames(UG_JIN_LWR) == "mean"] <- "ERA5_LWR_J/M2"



UG_JIN_Pressure <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Pressure/ERA5_export_UG_JIN_Pressure.csv")
# Renaming columns in the UG_JIN_Temp data frame
colnames(UG_JIN_Pressure)[colnames(UG_JIN_Pressure) == "system.index"] <- "DateTime"
colnames(UG_JIN_Pressure)[colnames(UG_JIN_Pressure) == "mean"] <- "ERA5_Pressure_Pa"



UG_JIN_U_Wind <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/U_Wind/ERA5_export_UG_JIN_U_Wind.csv")
# Renaming columns in the UG_JIN_Temp data frame
colnames(UG_JIN_U_Wind)[colnames(UG_JIN_U_Wind) == "system.index"] <- "DateTime"
colnames(UG_JIN_U_Wind)[colnames(UG_JIN_U_Wind) == "mean"] <- "ERA5_U_Wind_m/s"



UG_JIN_V_Wind <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/V_Wind/ERA5_export_UG_JIN_V_Wind.csv")
# Renaming columns in the UG_JIN_Temp data frame
colnames(UG_JIN_V_Wind)[colnames(UG_JIN_V_Wind) == "system.index"] <- "DateTime"
colnames(UG_JIN_V_Wind)[colnames(UG_JIN_V_Wind) == "mean"] <- "ERA5_V_Wind_m/s"



UG_JIN_Precip <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Rainfall/ERA5_export_UG_JIN_Precipitation.csv")
# Renaming columns in the UG_JIN_Temp data frame
colnames(UG_JIN_Precip)[colnames(UG_JIN_Precip) == "system.index"] <- "DateTime"
colnames(UG_JIN_Precip)[colnames(UG_JIN_Precip) == "mean"] <- "ERA5_Precip_m"



# List of datasets to combine
datasets <- list(UG_JIN_Solarad, UG_JIN_LWR, UG_JIN_Temp, UG_JIN_Dewpoint, UG_JIN_Pressure, UG_JIN_Precip, UG_JIN_U_Wind, UG_JIN_V_Wind)

# Combine all datasets in the list by "DateTime"
UG_JIN_Combined <- Reduce(function(x, y) merge(x, y, by = "DateTime", all = TRUE), datasets)



# Select columns you want to keep
UG_JIN_ERA5_MET <- UG_JIN_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
UG_JIN_ERA5_MET$DateTime <- as.POSIXct(substr(UG_JIN_ERA5_MET$DateTime, 1, 15), 
                                       format = "%Y%m%dT%H", tz = "UTC")




#Convert variables to appropriate units 

#Convert ERA5_SSR_J/M2 from J/m² to W/m²
UG_JIN_ERA5_MET$`ERA5_SSR_W/M2` <- UG_JIN_ERA5_MET$`ERA5_SSR_J/M2` / 3600


#Convert ERA5_LWR_J/M2 from J/m² to W/m²
UG_JIN_ERA5_MET$`ERA5_LWR_W/M2` <- UG_JIN_ERA5_MET$`ERA5_LWR_J/M2` / 3600


# Convert ERA5_Dewpoint_temp_K from Kelvin to Celsius
UG_JIN_ERA5_MET$ERA5_Dewpoint_temp_C <- UG_JIN_ERA5_MET$ERA5_Dewpoint_temp_K - 273.15


# Convert ERA5_Temp_K from Kelvin to Celsius
UG_JIN_ERA5_MET$ERA5_Temp_C <- UG_JIN_ERA5_MET$ERA5_Temp_K - 273.15


# Convert from m to mm
UG_JIN_ERA5_MET$ERA5_Precip_mm <- UG_JIN_ERA5_MET$ERA5_Precip_m * 1000


# convert from mm to kg m-2 s-1
UG_JIN_ERA5_MET$`ERA5_Precip_kg/m2/s` <- UG_JIN_ERA5_MET$ERA5_Precip_mm / 3600


# Calculate wind speed using the U & V components
UG_JIN_ERA5_MET$`ERA5_Wind_Speed_m/s` <- sqrt(UG_JIN_ERA5_MET$`ERA5_U_Wind_m/s`^2 + UG_JIN_ERA5_MET$`ERA5_V_Wind_m/s`^2)



# Convert Pressure from Pa to hPa
UG_JIN_ERA5_MET <- UG_JIN_ERA5_MET %>%
  mutate(ERA5_Pressure_hPa = ERA5_Pressure_Pa / 100)

# Calculate Actual Vapor Pressure (e) in hPa using Dew Point Temperature in Celsius
UG_JIN_ERA5_MET <- UG_JIN_ERA5_MET %>%
  mutate(e_hPa = 6.112 * exp((17.67 * ERA5_Dewpoint_temp_C) / (ERA5_Dewpoint_temp_C + 243.5)))

# Calculate Specific Humidity (q) in kg/kg
UG_JIN_ERA5_MET <- UG_JIN_ERA5_MET %>%
  mutate(`ERA5_Specific_humidity_kg/kg` = (0.622 * e_hPa) / (ERA5_Pressure_hPa - (0.378 * e_hPa)))




# Interpolate data to get half hourly data'

# Step 1: Generate half-hourly timestamps
time_half_hourly <- seq(from = min(UG_JIN_ERA5_MET$DateTime), 
                        to = max(UG_JIN_ERA5_MET$DateTime), by = "30 mins")

# Step 2: Interpolate each variable to the half-hourly time step

# For Temperature (C)
temperature_half_hourly <- approx(x = UG_JIN_ERA5_MET$DateTime, 
                                  y = UG_JIN_ERA5_MET$ERA5_Temp_C, 
                                  xout = time_half_hourly)$y


# For Temperature (K)
temperature_K_half_hourly <- approx(x = UG_JIN_ERA5_MET$DateTime, 
                                    y = UG_JIN_ERA5_MET$ERA5_Temp_K, 
                                    xout = time_half_hourly)$y


# For Dewpoint Temperature 
dewpoint_half_hourly <- approx(x = UG_JIN_ERA5_MET$DateTime, 
                               y = UG_JIN_ERA5_MET$ERA5_Dewpoint_temp_C, 
                               xout = time_half_hourly)$y

# For Solar Surface Radiation (ERAS_SSR_W/M2)
ssr_half_hourly <- approx(x = UG_JIN_ERA5_MET$DateTime, 
                          y = UG_JIN_ERA5_MET$`ERA5_SSR_W/M2`, 
                          xout = time_half_hourly)$y

# For thermal radiation (ERA5_LWR_W/M2)
lwr_half_hourly <- approx(x = UG_JIN_ERA5_MET$DateTime,
                          y = UG_JIN_ERA5_MET$`ERA5_LWR_W/M2`,
                          xout = time_half_hourly)$y

# For wind speed
wndspd_half_hourly <- approx(x = UG_JIN_ERA5_MET$DateTime,
                             y = UG_JIN_ERA5_MET$`ERA5_Wind_Speed_m/s`,
                             xout = time_half_hourly)$y

# For pressure (Pa)
pressure_Pa_half_hourly <- approx(x = UG_JIN_ERA5_MET$DateTime,
                                  y = UG_JIN_ERA5_MET$`ERA5_Pressure_Pa`,
                                  xout = time_half_hourly)$y

# For pressure (hpa)
pressure_hPa_half_hourly <- approx(x = UG_JIN_ERA5_MET$DateTime,
                                   y = UG_JIN_ERA5_MET$`ERA5_Pressure_hPa`,
                                   xout = time_half_hourly)$y

# For specific humidity
specificHumidity_half_hourly <- approx(x = UG_JIN_ERA5_MET$DateTime,
                                       y = UG_JIN_ERA5_MET$`ERA5_Specific_humidity_kg/kg`,
                                       xout = time_half_hourly)$y


# For precipitation
precipitation_mm_half_hourly <- approx(x = UG_JIN_ERA5_MET$DateTime,
                                       y = UG_JIN_ERA5_MET$ERA5_Precip_mm,
                                       xout = time_half_hourly)$y

# For precipitation kg/m2/s
precipitation_kg_half_hourly <- approx(x = UG_JIN_ERA5_MET$DateTime,
                                       y = UG_JIN_ERA5_MET$`ERA5_Precip_kg/m2/s`,
                                       xout = time_half_hourly)$y


# Step 3: Combine the interpolated data into a new dataframe
UG_JIN_ERA5_MET_HH <- data.frame(
  DateTime = time_half_hourly,
  ERA5_Temp_C = temperature_half_hourly,
  ERA5_Temp_K = temperature_K_half_hourly,
  ERA5_Dewpoint_temp_C = dewpoint_half_hourly,
  `ERA5_SSR_W/M2` = ssr_half_hourly,
  `ERA5_LWR_W/M2` = lwr_half_hourly,
  `ERA5_Specific_humidity_kg/kg` = specificHumidity_half_hourly,
  `ERA5_Pressure_Pa` = pressure_Pa_half_hourly,
  `ERA5_Pressure_hPa` = pressure_hPa_half_hourly,
  `ERA5_Wind_Speed_m/s` = wndspd_half_hourly,
  ERA5_Precip_mm = precipitation_mm_half_hourly,
  `ERA5_Precip_kg/m2/s`= precipitation_kg_half_hourly
)





# Step 4: plot hourly vs interpolated for QC

ggplot() +
  geom_line(data = UG_JIN_ERA5_MET, aes(x = DateTime, y = ERA5_Temp_C), color = "red", size = 1.2) +
  geom_line(data = UG_JIN_ERA5_MET_HH, aes(x = DateTime, y = ERA5_Temp_C), color = "blue", linetype = "dashed", size = 1) +
  labs(title = "Hourly vs Half-Hourly Interpolated Temperature",
       x = "Date", y = "Temperature (C)") +
  theme_minimal()



# Subset the data for a specific time range that exists in the dataset (e.g., one day)
subset_data_hourly <- subset(UG_JIN_ERA5_MET, DateTime >= as.POSIXct("2003-11-11 00:00:00") & DateTime <= as.POSIXct("2003-11-12 00:00:00"))
subset_data_half_hourly <- subset(UG_JIN_ERA5_MET_HH, DateTime >= as.POSIXct("2003-11-11 00:00:00") & DateTime <= as.POSIXct("2003-11-12 00:00:00"))

# Plot the subsetted data
ggplot() +
  geom_point(data = subset_data_hourly, aes(x = DateTime, y = ERA5_Temp_C), color = "red", size = 1.5) +
  geom_point(data = subset_data_half_hourly, aes(x = DateTime, y = ERA5_Temp_C), color = "blue", size = 1) +
  labs(title = "Hourly vs Half-Hourly Interpolated Temperature (Zoomed In)",
       x = "Time", y = "Temperature (°C)") +
  theme_minimal()






# Calculating VPD using land air temperature and dew point temperature 
# Constants
Pmsl <- 1013.25  # mean sea level pressure in hPa
Z_m <- 1170.81    # altitude in meters (as provided)

# Calculate VPD for ERA5-Land
calculate_vpd <- function(temp_C, dewpoint_temp_C, Z_m) {
  # Calculate Pmst (equation 6)
  Pmst <- Pmsl * ((temp_C + 273.16) / (temp_C + 273.16 + 0.0065 * Z_m))^5.625
  
  # Corrected formula for fw (equation 5)
  fw <- 1 + 7 * 10^(-4) + 3.46 * 10^(-6) * Pmst
  
  # Calculate SVP (equation 3)
  SVP <- 6.112 * fw * exp((17.67 * temp_C) / (temp_C + 243.5))
  
  # Calculate AVP (equation 4)
  AVP <- 6.112 * fw * exp((17.67 * dewpoint_temp_C) / (dewpoint_temp_C + 243.5))
  
  # Calculate VPD (equation 2)
  VPD <- SVP - AVP
  
  return(VPD)
}

# Apply VPD calculation to the dataset
UG_JIN_ERA5_MET_HH <- UG_JIN_ERA5_MET_HH %>%
  mutate(VPD_hPa = calculate_vpd(ERA5_Temp_C, ERA5_Dewpoint_temp_C, Z_m))







# for ZA_CATH ----

ZA_CATH_Solarad <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/SolarRad/ERA5_export_ZA_CATH_SSR.csv")
# Renaming columns in the ZA_CATH_Solar_rad data frame
colnames(ZA_CATH_Solarad)[colnames(ZA_CATH_Solarad) == "system.index"] <- "DateTime"
colnames(ZA_CATH_Solarad)[colnames(ZA_CATH_Solarad) == "mean"] <- "ERA5_SSR_J/M2"



ZA_CATH_Dewpoint <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Dewpoint/ERA5_export_dewpoint_ZA_CATH.csv")
#Renaming columns in the ZA_CATH_dew_point data frame
colnames(ZA_CATH_Dewpoint)[colnames(ZA_CATH_Dewpoint) == "system.index"] <- "DateTime"
colnames(ZA_CATH_Dewpoint)[colnames(ZA_CATH_Dewpoint) == "mean"] <- "ERA5_Dewpoint_temp_K"



ZA_CATH_Temp <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Temp/ERA5_export_temp_ZA_CATH.csv")
# Renaming columns in the ZA_CATH_Temp data frame
colnames(ZA_CATH_Temp)[colnames(ZA_CATH_Temp) == "system.index"] <- "DateTime"
colnames(ZA_CATH_Temp)[colnames(ZA_CATH_Temp) == "mean"] <- "ERA5_Temp_K"


ZA_CATH_LWR <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/ThermalRad/ERA5_export_ZA_CATH_LWR.csv")
# Renaming columns in the ZA_CATH_Temp data frame
colnames(ZA_CATH_LWR)[colnames(ZA_CATH_LWR) == "system.index"] <- "DateTime"
colnames(ZA_CATH_LWR)[colnames(ZA_CATH_LWR) == "mean"] <- "ERA5_LWR_J/M2"



ZA_CATH_Pressure <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Pressure/ERA5_export_ZA_CATH_Pressure.csv")
# Renaming columns in the ZA_CATH_Temp data frame
colnames(ZA_CATH_Pressure)[colnames(ZA_CATH_Pressure) == "system.index"] <- "DateTime"
colnames(ZA_CATH_Pressure)[colnames(ZA_CATH_Pressure) == "mean"] <- "ERA5_Pressure_Pa"



ZA_CATH_U_Wind <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/U_Wind/ERA5_export_ZA_CATH_U_Wind.csv")
# Renaming columns in the ZA_CATH_Temp data frame
colnames(ZA_CATH_U_Wind)[colnames(ZA_CATH_U_Wind) == "system.index"] <- "DateTime"
colnames(ZA_CATH_U_Wind)[colnames(ZA_CATH_U_Wind) == "mean"] <- "ERA5_U_Wind_m/s"



ZA_CATH_V_Wind <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/V_Wind/ERA5_export_ZA_CATH_V_Wind.csv")
# Renaming columns in the ZA_CATH_Temp data frame
colnames(ZA_CATH_V_Wind)[colnames(ZA_CATH_V_Wind) == "system.index"] <- "DateTime"
colnames(ZA_CATH_V_Wind)[colnames(ZA_CATH_V_Wind) == "mean"] <- "ERA5_V_Wind_m/s"



ZA_CATH_Precip <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Rainfall/ERA5_export_ZA_CATH_Precipitation.csv")
# Renaming columns in the ZA_CATH_Temp data frame
colnames(ZA_CATH_Precip)[colnames(ZA_CATH_Precip) == "system.index"] <- "DateTime"
colnames(ZA_CATH_Precip)[colnames(ZA_CATH_Precip) == "mean"] <- "ERA5_Precip_m"



# List of datasets to combine
datasets <- list(ZA_CATH_Solarad, ZA_CATH_LWR, ZA_CATH_Temp, ZA_CATH_Dewpoint, ZA_CATH_Pressure, ZA_CATH_Precip, ZA_CATH_U_Wind, ZA_CATH_V_Wind)

# Combine all datasets in the list by "DateTime"
ZA_CATH_Combined <- Reduce(function(x, y) merge(x, y, by = "DateTime", all = TRUE), datasets)



# Select columns you want to keep
ZA_CATH_ERA5_MET <- ZA_CATH_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
ZA_CATH_ERA5_MET$DateTime <- as.POSIXct(substr(ZA_CATH_ERA5_MET$DateTime, 1, 15), 
                                       format = "%Y%m%dT%H", tz = "UTC")




#Convert variables to appropriate units 

#Convert ERA5_SSR_J/M2 from J/m² to W/m²
ZA_CATH_ERA5_MET$`ERA5_SSR_W/M2` <- ZA_CATH_ERA5_MET$`ERA5_SSR_J/M2` / 3600


#Convert ERA5_LWR_J/M2 from J/m² to W/m²
ZA_CATH_ERA5_MET$`ERA5_LWR_W/M2` <- ZA_CATH_ERA5_MET$`ERA5_LWR_J/M2` / 3600


# Convert ERA5_Dewpoint_temp_K from Kelvin to Celsius
ZA_CATH_ERA5_MET$ERA5_Dewpoint_temp_C <- ZA_CATH_ERA5_MET$ERA5_Dewpoint_temp_K - 273.15


# Convert ERA5_Temp_K from Kelvin to Celsius
ZA_CATH_ERA5_MET$ERA5_Temp_C <- ZA_CATH_ERA5_MET$ERA5_Temp_K - 273.15


# Convert from m to mm
ZA_CATH_ERA5_MET$ERA5_Precip_mm <- ZA_CATH_ERA5_MET$ERA5_Precip_m * 1000

# convert from mm to kg m-2 s-1
ZA_CATH_ERA5_MET$`ERA5_Precip_kg/m2/s` <- ZA_CATH_ERA5_MET$ERA5_Precip_mm / 3600


# Calculate wind speed using the U & V components
ZA_CATH_ERA5_MET$`ERA5_Wind_Speed_m/s` <- sqrt(ZA_CATH_ERA5_MET$`ERA5_U_Wind_m/s`^2 + ZA_CATH_ERA5_MET$`ERA5_V_Wind_m/s`^2)


# Convert Pressure from Pa to hPa
ZA_CATH_ERA5_MET <- ZA_CATH_ERA5_MET %>%
  mutate(ERA5_Pressure_hPa = ERA5_Pressure_Pa / 100)

# Calculate Actual Vapor Pressure (e) in hPa using Dew Point Temperature in Celsius
ZA_CATH_ERA5_MET <- ZA_CATH_ERA5_MET %>%
  mutate(e_hPa = 6.112 * exp((17.67 * ERA5_Dewpoint_temp_C) / (ERA5_Dewpoint_temp_C + 243.5)))

# Calculate Specific Humidity (q) in kg/kg
ZA_CATH_ERA5_MET <- ZA_CATH_ERA5_MET %>%
  mutate(`ERA5_Specific_humidity_kg/kg` = (0.622 * e_hPa) / (ERA5_Pressure_hPa - (0.378 * e_hPa)))






# Interpolate data to get half hourly data'

# Step 1: Generate half-hourly timestamps
time_half_hourly <- seq(from = min(ZA_CATH_ERA5_MET$DateTime), 
                        to = max(ZA_CATH_ERA5_MET$DateTime), by = "30 mins")

# Step 2: Interpolate each variable to the half-hourly time step

# For Temperature (C)
temperature_half_hourly <- approx(x = ZA_CATH_ERA5_MET$DateTime, 
                                  y = ZA_CATH_ERA5_MET$ERA5_Temp_C, 
                                  xout = time_half_hourly)$y

# For Temperature (K)
temperature_K_half_hourly <- approx(x = ZA_CATH_ERA5_MET$DateTime, 
                                    y = ZA_CATH_ERA5_MET$ERA5_Temp_K, 
                                    xout = time_half_hourly)$y

# For Dewpoint Temperature 
dewpoint_half_hourly <- approx(x = ZA_CATH_ERA5_MET$DateTime, 
                               y = ZA_CATH_ERA5_MET$ERA5_Dewpoint_temp_C, 
                               xout = time_half_hourly)$y

# For Solar Surface Radiation (ERAS_SSR_W/M2)
ssr_half_hourly <- approx(x = ZA_CATH_ERA5_MET$DateTime, 
                          y = ZA_CATH_ERA5_MET$`ERA5_SSR_W/M2`, 
                          xout = time_half_hourly)$y

# For thermal radiation (ERA5_LWR_W/M2)
lwr_half_hourly <- approx(x = ZA_CATH_ERA5_MET$DateTime,
                          y = ZA_CATH_ERA5_MET$`ERA5_LWR_W/M2`,
                          xout = time_half_hourly)$y

# For wind speed
wndspd_half_hourly <- approx(x = ZA_CATH_ERA5_MET$DateTime,
                             y = ZA_CATH_ERA5_MET$`ERA5_Wind_Speed_m/s`,
                             xout = time_half_hourly)$y

# For pressure (Pa)
pressure_Pa_half_hourly <- approx(x = ZA_CATH_ERA5_MET$DateTime,
                                  y = ZA_CATH_ERA5_MET$`ERA5_Pressure_Pa`,
                                  xout = time_half_hourly)$y

# For pressure (hpa)
pressure_hPa_half_hourly <- approx(x = ZA_CATH_ERA5_MET$DateTime,
                                   y = ZA_CATH_ERA5_MET$`ERA5_Pressure_hPa`,
                                   xout = time_half_hourly)$y

# For specific humidity
specificHumidity_half_hourly <- approx(x = ZA_CATH_ERA5_MET$DateTime,
                                       y = ZA_CATH_ERA5_MET$`ERA5_Specific_humidity_kg/kg`,
                                       xout = time_half_hourly)$y


# For precipitation
precipitation_mm_half_hourly <- approx(x = ZA_CATH_ERA5_MET$DateTime,
                                       y = ZA_CATH_ERA5_MET$ERA5_Precip_mm,
                                       xout = time_half_hourly)$y

# For precipitation kg/m2/s
precipitation_kg_half_hourly <- approx(x = ZA_CATH_ERA5_MET$DateTime,
                                       y = ZA_CATH_ERA5_MET$`ERA5_Precip_kg/m2/s`,
                                       xout = time_half_hourly)$y


# Step 3: Combine the interpolated data into a new dataframe
ZA_CATH_ERA5_MET_HH <- data.frame(
  DateTime = time_half_hourly,
  ERA5_Temp_C = temperature_half_hourly,
  ERA5_Temp_K = temperature_K_half_hourly,
  ERA5_Dewpoint_temp_C = dewpoint_half_hourly,
  `ERA5_SSR_W/M2` = ssr_half_hourly,
  `ERA5_LWR_W/M2` = lwr_half_hourly,
  `ERA5_Specific_humidity_kg/kg` = specificHumidity_half_hourly,
  `ERA5_Pressure_Pa` = pressure_Pa_half_hourly,
  `ERA5_Pressure_hPa` = pressure_hPa_half_hourly,
  `ERA5_Wind_Speed_m/s` = wndspd_half_hourly,
  ERA5_Precip_mm = precipitation_mm_half_hourly,
  `ERA5_Precip_kg/m2/s`= precipitation_kg_half_hourly
)






# Step 4: plot hourly vs interpolated for QC

ggplot() +
  geom_line(data = ZA_CATH_ERA5_MET, aes(x = DateTime, y = ERA5_Temp_C), color = "red", size = 1.2) +
  geom_line(data = ZA_CATH_ERA5_MET_HH, aes(x = DateTime, y = ERA5_Temp_C), color = "blue", linetype = "dashed", size = 1) +
  labs(title = "Hourly vs Half-Hourly Interpolated Temperature",
       x = "Date", y = "Temperature (C)") +
  theme_minimal()



# Subset the data for a specific time range that exists in the dataset (e.g., one day)
subset_data_hourly <- subset(ZA_CATH_ERA5_MET, DateTime >= as.POSIXct("2014-11-11 00:00:00") & DateTime <= as.POSIXct("2014-11-12 00:00:00"))
subset_data_half_hourly <- subset(ZA_CATH_ERA5_MET_HH, DateTime >= as.POSIXct("2014-11-11 00:00:00") & DateTime <= as.POSIXct("2014-11-12 00:00:00"))

# Plot the subsetted data
ggplot() +
  geom_point(data = subset_data_hourly, aes(x = DateTime, y = ERA5_Temp_C), color = "red", size = 1.5) +
  geom_point(data = subset_data_half_hourly, aes(x = DateTime, y = ERA5_Temp_C), color = "blue", size = 1) +
  labs(title = "Hourly vs Half-Hourly Interpolated Temperature (Zoomed In)",
       x = "Time", y = "Temperature (°C)") +
  theme_minimal()





# Calculating VPD using land air temperature and dew point temperature 
# Constants
Pmsl <- 1013.25  # mean sea level pressure in hPa
Z_m <- 1858.4    # altitude in meters (as provided)

# Calculate VPD for ERA5-Land
calculate_vpd <- function(temp_C, dewpoint_temp_C, Z_m) {
  # Calculate Pmst (equation 6)
  Pmst <- Pmsl * ((temp_C + 273.16) / (temp_C + 273.16 + 0.0065 * Z_m))^5.625
  
  # Corrected formula for fw (equation 5)
  fw <- 1 + 7 * 10^(-4) + 3.46 * 10^(-6) * Pmst
  
  # Calculate SVP (equation 3)
  SVP <- 6.112 * fw * exp((17.67 * temp_C) / (temp_C + 243.5))
  
  # Calculate AVP (equation 4)
  AVP <- 6.112 * fw * exp((17.67 * dewpoint_temp_C) / (dewpoint_temp_C + 243.5))
  
  # Calculate VPD (equation 2)
  VPD <- SVP - AVP
  
  return(VPD)
}

# Apply VPD calculation to the dataset
ZA_CATH_ERA5_MET_HH <- ZA_CATH_ERA5_MET_HH %>%
  mutate(VPD_hPa = calculate_vpd(ERA5_Temp_C, ERA5_Dewpoint_temp_C, Z_m))








# for ZA_KRU  ----

ZA_KRU_Solarad <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/SolarRad/ERA5_export_ZA_KRU_SSR.csv")
# Renaming columns in the ZA_KRU_Solar_rad data frame
colnames(ZA_KRU_Solarad)[colnames(ZA_KRU_Solarad) == "system.index"] <- "DateTime"
colnames(ZA_KRU_Solarad)[colnames(ZA_KRU_Solarad) == "mean"] <- "ERA5_SSR_J/M2"



ZA_KRU_Dewpoint <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Dewpoint/ERA5_export_dewpoint_ZA_KRU.csv")
#Renaming columns in the ZA_KRU_dew_point data frame
colnames(ZA_KRU_Dewpoint)[colnames(ZA_KRU_Dewpoint) == "system.index"] <- "DateTime"
colnames(ZA_KRU_Dewpoint)[colnames(ZA_KRU_Dewpoint) == "mean"] <- "ERA5_Dewpoint_temp_K"



ZA_KRU_Temp <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Temp/ERA5_export_temp_ZA_KRU.csv")
# Renaming columns in the ZA_KRU_Temp data frame
colnames(ZA_KRU_Temp)[colnames(ZA_KRU_Temp) == "system.index"] <- "DateTime"
colnames(ZA_KRU_Temp)[colnames(ZA_KRU_Temp) == "mean"] <- "ERA5_Temp_K"


ZA_KRU_LWR <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/ThermalRad/ERA5_export_ZA_KRU_LWR.csv")
# Renaming columns in the ZA_KRU_Temp data frame
colnames(ZA_KRU_LWR)[colnames(ZA_KRU_LWR) == "system.index"] <- "DateTime"
colnames(ZA_KRU_LWR)[colnames(ZA_KRU_LWR) == "mean"] <- "ERA5_LWR_J/M2"



ZA_KRU_Pressure <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Pressure/ERA5_export_ZA_KRU_Pressure.csv")
# Renaming columns in the ZA_KRU_Temp data frame
colnames(ZA_KRU_Pressure)[colnames(ZA_KRU_Pressure) == "system.index"] <- "DateTime"
colnames(ZA_KRU_Pressure)[colnames(ZA_KRU_Pressure) == "mean"] <- "ERA5_Pressure_Pa"



ZA_KRU_U_Wind <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/U_Wind/ERA5_export_ZA_KRU_U_Wind.csv")
# Renaming columns in the ZA_KRU_Temp data frame
colnames(ZA_KRU_U_Wind)[colnames(ZA_KRU_U_Wind) == "system.index"] <- "DateTime"
colnames(ZA_KRU_U_Wind)[colnames(ZA_KRU_U_Wind) == "mean"] <- "ERA5_U_Wind_m/s"



ZA_KRU_V_Wind <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/V_Wind/ERA5_export_ZA_KRU_V_Wind.csv")
# Renaming columns in the ZA_KRU_Temp data frame
colnames(ZA_KRU_V_Wind)[colnames(ZA_KRU_V_Wind) == "system.index"] <- "DateTime"
colnames(ZA_KRU_V_Wind)[colnames(ZA_KRU_V_Wind) == "mean"] <- "ERA5_V_Wind_m/s"



ZA_KRU_Precip <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Rainfall/ERA5_export_ZA_KRU_Precipitation.csv")
# Renaming columns in the ZA_KRU_Temp data frame
colnames(ZA_KRU_Precip)[colnames(ZA_KRU_Precip) == "system.index"] <- "DateTime"
colnames(ZA_KRU_Precip)[colnames(ZA_KRU_Precip) == "mean"] <- "ERA5_Precip_m"



# List of datasets to combine
datasets <- list(ZA_KRU_Solarad, ZA_KRU_LWR, ZA_KRU_Temp, ZA_KRU_Dewpoint, ZA_KRU_Pressure, ZA_KRU_Precip, ZA_KRU_U_Wind, ZA_KRU_V_Wind)

# Combine all datasets in the list by "DateTime"
ZA_KRU_Combined <- Reduce(function(x, y) merge(x, y, by = "DateTime", all = TRUE), datasets)



# Select columns you want to keep
ZA_KRU_ERA5_MET <- ZA_KRU_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
ZA_KRU_ERA5_MET$DateTime <- as.POSIXct(substr(ZA_KRU_ERA5_MET$DateTime, 1, 15), 
                                        format = "%Y%m%dT%H", tz = "UTC")



#Convert variables to appropriate units 

#Convert ERA5_SSR_J/M2 from J/m² to W/m²
ZA_KRU_ERA5_MET$`ERA5_SSR_W/M2` <- ZA_KRU_ERA5_MET$`ERA5_SSR_J/M2` / 3600


#Convert ERA5_LWR_J/M2 from J/m² to W/m²
ZA_KRU_ERA5_MET$`ERA5_LWR_W/M2` <- ZA_KRU_ERA5_MET$`ERA5_LWR_J/M2` / 3600


# Convert ERA5_Dewpoint_temp_K from Kelvin to Celsius
ZA_KRU_ERA5_MET$ERA5_Dewpoint_temp_C <- ZA_KRU_ERA5_MET$ERA5_Dewpoint_temp_K - 273.15


# Convert ERA5_Temp_K from Kelvin to Celsius
ZA_KRU_ERA5_MET$ERA5_Temp_C <- ZA_KRU_ERA5_MET$ERA5_Temp_K - 273.15


# convert from m to mm
ZA_KRU_ERA5_MET$ERA5_Precip_mm <- ZA_KRU_ERA5_MET$ERA5_Precip_m * 1000


# convert from mm to kg m-2 s-1
ZA_KRU_ERA5_MET$`ERA5_Precip_kg/m2/s` <- ZA_KRU_ERA5_MET$ERA5_Precip_mm / 3600


# Calculate wind speed using the U & V components
ZA_KRU_ERA5_MET$`ERA5_Wind_Speed_m/s` <- sqrt(ZA_KRU_ERA5_MET$`ERA5_U_Wind_m/s`^2 + ZA_KRU_ERA5_MET$`ERA5_V_Wind_m/s`^2)



# Convert Pressure from Pa to hPa
ZA_KRU_ERA5_MET <- ZA_KRU_ERA5_MET %>%
  mutate(ERA5_Pressure_hPa = ERA5_Pressure_Pa / 100)

# Calculate Actual Vapor Pressure (e) in hPa using Dew Point Temperature in Celsius
ZA_KRU_ERA5_MET <- ZA_KRU_ERA5_MET %>%
  mutate(e_hPa = 6.112 * exp((17.67 * ERA5_Dewpoint_temp_C) / (ERA5_Dewpoint_temp_C + 243.5)))

# Calculate Specific Humidity (q) in kg/kg
ZA_KRU_ERA5_MET <- ZA_KRU_ERA5_MET %>%
  mutate(`ERA5_Specific_humidity_kg/kg` = (0.622 * e_hPa) / (ERA5_Pressure_hPa - (0.378 * e_hPa)))





# Interpolate data to get half hourly data'

# Step 1: Generate half-hourly timestamps
time_half_hourly <- seq(from = min(ZA_KRU_ERA5_MET$DateTime), 
                        to = max(ZA_KRU_ERA5_MET$DateTime), by = "30 mins")

# Step 2: Interpolate each variable to the half-hourly time step

# For Temperature (C)
temperature_half_hourly <- approx(x = ZA_KRU_ERA5_MET$DateTime, 
                                  y = ZA_KRU_ERA5_MET$ERA5_Temp_C, 
                                  xout = time_half_hourly)$y

# For Temperature (K)
temperature_K_half_hourly <- approx(x = ZA_KRU_ERA5_MET$DateTime, 
                                    y = ZA_KRU_ERA5_MET$ERA5_Temp_K, 
                                    xout = time_half_hourly)$y

# For Dewpoint Temperature 
dewpoint_half_hourly <- approx(x = ZA_KRU_ERA5_MET$DateTime, 
                               y = ZA_KRU_ERA5_MET$ERA5_Dewpoint_temp_C, 
                               xout = time_half_hourly)$y

# For Solar Surface Radiation (ERAS_SSR_W/M2)
ssr_half_hourly <- approx(x = ZA_KRU_ERA5_MET$DateTime, 
                          y = ZA_KRU_ERA5_MET$`ERA5_SSR_W/M2`, 
                          xout = time_half_hourly)$y

# For thermal radiation (ERA5_LWR_W/M2)
lwr_half_hourly <- approx(x = ZA_KRU_ERA5_MET$DateTime,
                          y = ZA_KRU_ERA5_MET$`ERA5_LWR_W/M2`,
                          xout = time_half_hourly)$y

# For wind speed
wndspd_half_hourly <- approx(x = ZA_KRU_ERA5_MET$DateTime,
                             y = ZA_KRU_ERA5_MET$`ERA5_Wind_Speed_m/s`,
                             xout = time_half_hourly)$y

# For pressure (Pa)
pressure_Pa_half_hourly <- approx(x = ZA_KRU_ERA5_MET$DateTime,
                                  y = ZA_KRU_ERA5_MET$`ERA5_Pressure_Pa`,
                                  xout = time_half_hourly)$y

# For pressure (hpa)
pressure_hPa_half_hourly <- approx(x = ZA_KRU_ERA5_MET$DateTime,
                                   y = ZA_KRU_ERA5_MET$`ERA5_Pressure_hPa`,
                                   xout = time_half_hourly)$y

# For specific humidity
specificHumidity_half_hourly <- approx(x = ZA_KRU_ERA5_MET$DateTime,
                                       y = ZA_KRU_ERA5_MET$`ERA5_Specific_humidity_kg/kg`,
                                       xout = time_half_hourly)$y

# For precipitation
precipitation_mm_half_hourly <- approx(x = ZA_KRU_ERA5_MET$DateTime,
                                       y = ZA_KRU_ERA5_MET$ERA5_Precip_mm,
                                       xout = time_half_hourly)$y

# For precipitation kg/m2/s
precipitation_kg_half_hourly <- approx(x = ZA_KRU_ERA5_MET$DateTime,
                                       y = ZA_KRU_ERA5_MET$`ERA5_Precip_kg/m2/s`,
                                       xout = time_half_hourly)$y


# Step 3: Combine the interpolated data into a new dataframe
ZA_KRU_ERA5_MET_HH <- data.frame(
  DateTime = time_half_hourly,
  ERA5_Temp_C = temperature_half_hourly,
  ERA5_Temp_K = temperature_K_half_hourly,
  ERA5_Dewpoint_temp_C = dewpoint_half_hourly,
  `ERA5_SSR_W/M2` = ssr_half_hourly,
  `ERA5_LWR_W/M2` = lwr_half_hourly,
  `ERA5_Specific_humidity_kg/kg` = specificHumidity_half_hourly,
  `ERA5_Pressure_Pa` = pressure_Pa_half_hourly,
  `ERA5_Pressure_hPa` = pressure_hPa_half_hourly,
  `ERA5_Wind_Speed_m/s` = wndspd_half_hourly,
  ERA5_Precip_mm = precipitation_mm_half_hourly,
  `ERA5_Precip_kg/m2/s`= precipitation_kg_half_hourly
)





# Step 4: plot hourly vs interpolated for QC

ggplot() +
  geom_line(data = ZA_KRU_ERA5_MET, aes(x = DateTime, y = ERA5_Temp_C), color = "red", size = 1.2) +
  geom_line(data = ZA_KRU_ERA5_MET_HH, aes(x = DateTime, y = ERA5_Temp_C), color = "blue", linetype = "dashed", size = 1) +
  labs(title = "Hourly vs Half-Hourly Interpolated Temperature",
       x = "Date", y = "Temperature (C)") +
  theme_minimal()



# Subset the data for a specific time range that exists in the dataset (e.g., one day)
subset_data_hourly <- subset(ZA_KRU_ERA5_MET, DateTime >= as.POSIXct("2005-11-11 00:00:00") & DateTime <= as.POSIXct("2005-11-12 00:00:00"))
subset_data_half_hourly <- subset(ZA_KRU_ERA5_MET_HH, DateTime >= as.POSIXct("2005-11-11 00:00:00") & DateTime <= as.POSIXct("2005-11-12 00:00:00"))

# Plot the subsetted data
ggplot() +
  geom_point(data = subset_data_hourly, aes(x = DateTime, y = ERA5_Temp_C), color = "red", size = 1.5) +
  geom_point(data = subset_data_half_hourly, aes(x = DateTime, y = ERA5_Temp_C), color = "blue", size = 1) +
  labs(title = "Hourly vs Half-Hourly Interpolated Temperature (Zoomed In)",
       x = "Time", y = "Temperature (°C)") +
  theme_minimal()





# Calculating VPD using land air temperature and dew point temperature 
# Constants
Pmsl <- 1013.25  # mean sea level pressure in hPa
Z_m <- 357.57   # altitude in meters (as provided)

# Calculate VPD for ERA5-Land
calculate_vpd <- function(temp_C, dewpoint_temp_C, Z_m) {
  # Calculate Pmst (equation 6)
  Pmst <- Pmsl * ((temp_C + 273.16) / (temp_C + 273.16 + 0.0065 * Z_m))^5.625
  
  # Corrected formula for fw (equation 5)
  fw <- 1 + 7 * 10^(-4) + 3.46 * 10^(-6) * Pmst
  
  # Calculate SVP (equation 3)
  SVP <- 6.112 * fw * exp((17.67 * temp_C) / (temp_C + 243.5))
  
  # Calculate AVP (equation 4)
  AVP <- 6.112 * fw * exp((17.67 * dewpoint_temp_C) / (dewpoint_temp_C + 243.5))
  
  # Calculate VPD (equation 2)
  VPD <- SVP - AVP
  
  return(VPD)
}

# Apply VPD calculation to the dataset
ZA_KRU_ERA5_MET_HH <- ZA_KRU_ERA5_MET_HH %>%
  mutate(VPD_hPa = calculate_vpd(ERA5_Temp_C, ERA5_Dewpoint_temp_C, Z_m))










# for ZA_WGN  ----

ZA_WGN_Solarad <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/SolarRad/ERA5_export_ZA_WGN_SSR.csv")
# Renaming columns in the ZA_WGN_Solar_rad data frame
colnames(ZA_WGN_Solarad)[colnames(ZA_WGN_Solarad) == "system.index"] <- "DateTime"
colnames(ZA_WGN_Solarad)[colnames(ZA_WGN_Solarad) == "mean"] <- "ERA5_SSR_J/M2"



ZA_WGN_Dewpoint <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Dewpoint/ERA5_export_dewpoint_ZA_WGN.csv")
#Renaming columns in the ZA_WGN_dew_point data frame
colnames(ZA_WGN_Dewpoint)[colnames(ZA_WGN_Dewpoint) == "system.index"] <- "DateTime"
colnames(ZA_WGN_Dewpoint)[colnames(ZA_WGN_Dewpoint) == "mean"] <- "ERA5_Dewpoint_temp_K"



ZA_WGN_Temp <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Temp/ERA5_export_temp_ZA_WGN.csv")
# Renaming columns in the ZA_WGN_Temp data frame
colnames(ZA_WGN_Temp)[colnames(ZA_WGN_Temp) == "system.index"] <- "DateTime"
colnames(ZA_WGN_Temp)[colnames(ZA_WGN_Temp) == "mean"] <- "ERA5_Temp_K"


ZA_WGN_LWR <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/ThermalRad/ERA5_export_ZA_WGN_LWR.csv")
# Renaming columns in the ZA_WGN_Temp data frame
colnames(ZA_WGN_LWR)[colnames(ZA_WGN_LWR) == "system.index"] <- "DateTime"
colnames(ZA_WGN_LWR)[colnames(ZA_WGN_LWR) == "mean"] <- "ERA5_LWR_J/M2"



ZA_WGN_Pressure <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Pressure/ERA5_export_ZA_WGN_Pressure.csv")
# Renaming columns in the ZA_WGN_Temp data frame
colnames(ZA_WGN_Pressure)[colnames(ZA_WGN_Pressure) == "system.index"] <- "DateTime"
colnames(ZA_WGN_Pressure)[colnames(ZA_WGN_Pressure) == "mean"] <- "ERA5_Pressure_Pa"



ZA_WGN_U_Wind <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/U_Wind/ERA5_export_ZA_WGN_U_Wind.csv")
# Renaming columns in the ZA_WGN_Temp data frame
colnames(ZA_WGN_U_Wind)[colnames(ZA_WGN_U_Wind) == "system.index"] <- "DateTime"
colnames(ZA_WGN_U_Wind)[colnames(ZA_WGN_U_Wind) == "mean"] <- "ERA5_U_Wind_m/s"



ZA_WGN_V_Wind <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/V_Wind/ERA5_export_ZA_WGN_V_Wind.csv")
# Renaming columns in the ZA_WGN_Temp data frame
colnames(ZA_WGN_V_Wind)[colnames(ZA_WGN_V_Wind) == "system.index"] <- "DateTime"
colnames(ZA_WGN_V_Wind)[colnames(ZA_WGN_V_Wind) == "mean"] <- "ERA5_V_Wind_m/s"



ZA_WGN_Precip <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Rainfall/ERA5_export_ZA_WGN_Precipitation.csv")
# Renaming columns in the ZA_WGN_Temp data frame
colnames(ZA_WGN_Precip)[colnames(ZA_WGN_Precip) == "system.index"] <- "DateTime"
colnames(ZA_WGN_Precip)[colnames(ZA_WGN_Precip) == "mean"] <- "ERA5_Precip_m"





# List of datasets to combine
datasets <- list(ZA_WGN_Solarad, ZA_WGN_LWR, ZA_WGN_Temp, ZA_WGN_Dewpoint, ZA_WGN_Pressure, ZA_WGN_Precip, ZA_WGN_U_Wind, ZA_WGN_V_Wind)

# Combine all datasets in the list by "DateTime"
ZA_WGN_Combined <- Reduce(function(x, y) merge(x, y, by = "DateTime", all = TRUE), datasets)





# Select columns you want to keep
ZA_WGN_ERA5_MET <- ZA_WGN_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
ZA_WGN_ERA5_MET$DateTime <- as.POSIXct(substr(ZA_WGN_ERA5_MET$DateTime, 1, 15), 
                                       format = "%Y%m%dT%H", tz = "UTC")





#Convert variables to appropriate units 

#Convert ERA5_SSR_J/M2 from J/m² to W/m²
ZA_WGN_ERA5_MET$`ERA5_SSR_W/M2` <- ZA_WGN_ERA5_MET$`ERA5_SSR_J/M2` / 3600


#Convert ERA5_LWR_J/M2 from J/m² to W/m²
ZA_WGN_ERA5_MET$`ERA5_LWR_W/M2` <- ZA_WGN_ERA5_MET$`ERA5_LWR_J/M2` / 3600


# Convert ERA5_Dewpoint_temp_K from Kelvin to Celsius
ZA_WGN_ERA5_MET$ERA5_Dewpoint_temp_C <- ZA_WGN_ERA5_MET$ERA5_Dewpoint_temp_K - 273.15


# Convert ERA5_Temp_K from Kelvin to Celsius
ZA_WGN_ERA5_MET$ERA5_Temp_C <- ZA_WGN_ERA5_MET$ERA5_Temp_K - 273.15


# Convert from m to mm
ZA_WGN_ERA5_MET$ERA5_Precip_mm <- ZA_WGN_ERA5_MET$ERA5_Precip_m * 1000


# convert from mm to kg m-2 s-1
ZA_WGN_ERA5_MET$`ERA5_Precip_kg/m2/s` <- ZA_WGN_ERA5_MET$ERA5_Precip_mm / 3600


# Calculate wind speed using the U & V components
ZA_WGN_ERA5_MET$`ERA5_Wind_Speed_m/s` <- sqrt(ZA_WGN_ERA5_MET$`ERA5_U_Wind_m/s`^2 + ZA_WGN_ERA5_MET$`ERA5_V_Wind_m/s`^2)



# Convert Pressure from Pa to hPa
ZA_WGN_ERA5_MET <- ZA_WGN_ERA5_MET %>%
  mutate(ERA5_Pressure_hPa = ERA5_Pressure_Pa / 100)

# Calculate Actual Vapor Pressure (e) in hPa using Dew Point Temperature in Celsius
ZA_WGN_ERA5_MET <- ZA_WGN_ERA5_MET %>%
  mutate(e_hPa = 6.112 * exp((17.67 * ERA5_Dewpoint_temp_C) / (ERA5_Dewpoint_temp_C + 243.5)))

# Calculate Specific Humidity (q) in kg/kg
ZA_WGN_ERA5_MET <- ZA_WGN_ERA5_MET %>%
  mutate(`ERA5_Specific_humidity_kg/kg` = (0.622 * e_hPa) / (ERA5_Pressure_hPa - (0.378 * e_hPa)))





# Interpolate data to get half hourly data'

# Step 1: Generate half-hourly timestamps
time_half_hourly <- seq(from = min(ZA_WGN_ERA5_MET$DateTime), 
                        to = max(ZA_WGN_ERA5_MET$DateTime), by = "30 mins")

# Step 2: Interpolate each variable to the half-hourly time step

# For Temperature (C)
temperature_half_hourly <- approx(x = ZA_WGN_ERA5_MET$DateTime, 
                                  y = ZA_WGN_ERA5_MET$ERA5_Temp_C, 
                                  xout = time_half_hourly)$y


# For Temperature (K)
temperature_K_half_hourly <- approx(x = ZA_WGN_ERA5_MET$DateTime, 
                                    y = ZA_WGN_ERA5_MET$ERA5_Temp_K, 
                                    xout = time_half_hourly)$y

# For Dewpoint Temperature 
dewpoint_half_hourly <- approx(x = ZA_WGN_ERA5_MET$DateTime, 
                               y = ZA_WGN_ERA5_MET$ERA5_Dewpoint_temp_C, 
                               xout = time_half_hourly)$y

# For Solar Surface Radiation (ERAS_SSR_W/M2)
ssr_half_hourly <- approx(x = ZA_WGN_ERA5_MET$DateTime, 
                          y = ZA_WGN_ERA5_MET$`ERA5_SSR_W/M2`, 
                          xout = time_half_hourly)$y

# For thermal radiation (ERA5_LWR_W/M2)
lwr_half_hourly <- approx(x = ZA_WGN_ERA5_MET$DateTime,
                          y = ZA_WGN_ERA5_MET$`ERA5_LWR_W/M2`,
                          xout = time_half_hourly)$y

# For wind speed
wndspd_half_hourly <- approx(x = ZA_WGN_ERA5_MET$DateTime,
                             y = ZA_WGN_ERA5_MET$`ERA5_Wind_Speed_m/s`,
                             xout = time_half_hourly)$y

# For pressure (Pa)
pressure_Pa_half_hourly <- approx(x = ZA_WGN_ERA5_MET$DateTime,
                                  y = ZA_WGN_ERA5_MET$`ERA5_Pressure_Pa`,
                                  xout = time_half_hourly)$y

# For pressure (hpa)
pressure_hPa_half_hourly <- approx(x = ZA_WGN_ERA5_MET$DateTime,
                                   y = ZA_WGN_ERA5_MET$`ERA5_Pressure_hPa`,
                                   xout = time_half_hourly)$y

# For specific humidity
specificHumidity_half_hourly <- approx(x = ZA_WGN_ERA5_MET$DateTime,
                                       y = ZA_WGN_ERA5_MET$`ERA5_Specific_humidity_kg/kg`,
                                       xout = time_half_hourly)$y

# For precipitation
precipitation_mm_half_hourly <- approx(x = ZA_WGN_ERA5_MET$DateTime,
                                       y = ZA_WGN_ERA5_MET$ERA5_Precip_mm,
                                       xout = time_half_hourly)$y


# For precipitation kg/m2/s
precipitation_kg_half_hourly <- approx(x = ZA_WGN_ERA5_MET$DateTime,
                                       y = ZA_WGN_ERA5_MET$`ERA5_Precip_kg/m2/s`,
                                       xout = time_half_hourly)$y


# Step 3: Combine the interpolated data into a new dataframe
ZA_WGN_ERA5_MET_HH <- data.frame(
  DateTime = time_half_hourly,
  ERA5_Temp_C = temperature_half_hourly,
  ERA5_Temp_K = temperature_K_half_hourly,
  ERA5_Dewpoint_temp_C = dewpoint_half_hourly,
  `ERA5_SSR_W/M2` = ssr_half_hourly,
  `ERA5_LWR_W/M2` = lwr_half_hourly,
  `ERA5_Specific_humidity_kg/kg` = specificHumidity_half_hourly,
  `ERA5_Pressure_Pa` = pressure_Pa_half_hourly,
  `ERA5_Pressure_hPa` = pressure_hPa_half_hourly,
  `ERA5_Wind_Speed_m/s` = wndspd_half_hourly,
  ERA5_Precip_mm = precipitation_mm_half_hourly,
  `ERA5_Precip_kg/m2/s`= precipitation_kg_half_hourly
)



# Step 4: plot hourly vs interpolated for QC

ggplot() +
  geom_line(data = ZA_WGN_ERA5_MET, aes(x = DateTime, y = ERA5_Temp_C), color = "red", size = 1.2) +
  geom_line(data = ZA_WGN_ERA5_MET_HH, aes(x = DateTime, y = ERA5_Temp_C), color = "blue", linetype = "dashed", size = 1) +
  labs(title = "Hourly vs Half-Hourly Interpolated Temperature",
       x = "Date", y = "Temperature (C)") +
  theme_minimal()



# Subset the data for a specific time range that exists in the dataset (e.g., one day)
subset_data_hourly <- subset(ZA_WGN_ERA5_MET, DateTime >= as.POSIXct("2010-11-11 00:00:00") & DateTime <= as.POSIXct("2010-11-12 00:00:00"))
subset_data_half_hourly <- subset(ZA_WGN_ERA5_MET_HH, DateTime >= as.POSIXct("2010-11-11 00:00:00") & DateTime <= as.POSIXct("2010-11-12 00:00:00"))

# Plot the subsetted data
ggplot() +
  geom_point(data = subset_data_hourly, aes(x = DateTime, y = ERA5_Temp_C), color = "red", size = 1.5) +
  geom_point(data = subset_data_half_hourly, aes(x = DateTime, y = ERA5_Temp_C), color = "blue", size = 1) +
  labs(title = "Hourly vs Half-Hourly Interpolated Temperature (Zoomed In)",
       x = "Time", y = "Temperature (°C)") +
  theme_minimal()







# Calculating VPD using land air temperature and dew point temperature 
# Constants
Pmsl <- 1013.25  # mean sea level pressure in hPa
Z_m <- 1473.11    # altitude in meters (as provided)

# Calculate VPD for ERA5-Land
calculate_vpd <- function(temp_C, dewpoint_temp_C, Z_m) {
  # Calculate Pmst (equation 6)
  Pmst <- Pmsl * ((temp_C + 273.16) / (temp_C + 273.16 + 0.0065 * Z_m))^5.625
  
  # Corrected formula for fw (equation 5)
  fw <- 1 + 7 * 10^(-4) + 3.46 * 10^(-6) * Pmst
  
  # Calculate SVP (equation 3)
  SVP <- 6.112 * fw * exp((17.67 * temp_C) / (temp_C + 243.5))
  
  # Calculate AVP (equation 4)
  AVP <- 6.112 * fw * exp((17.67 * dewpoint_temp_C) / (dewpoint_temp_C + 243.5))
  
  # Calculate VPD (equation 2)
  VPD <- SVP - AVP
  
  return(VPD)
}

# Apply VPD calculation to the dataset
ZA_WGN_ERA5_MET_HH <- ZA_WGN_ERA5_MET_HH %>%
  mutate(VPD_hPa = calculate_vpd(ERA5_Temp_C, ERA5_Dewpoint_temp_C, Z_m))







# for ZM_MON  ----

ZM_MON_Solarad <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/SolarRad/ERA5_export_ZM_MON_SSR.csv")
# Renaming columns in the ZM_MON_Solar_rad data frame
colnames(ZM_MON_Solarad)[colnames(ZM_MON_Solarad) == "system.index"] <- "DateTime"
colnames(ZM_MON_Solarad)[colnames(ZM_MON_Solarad) == "mean"] <- "ERA5_SSR_J/M2"



ZM_MON_Dewpoint <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Dewpoint/ERA5_export_dewpoint_ZM_MON.csv")
#Renaming columns in the ZM_MON_dew_point data frame
colnames(ZM_MON_Dewpoint)[colnames(ZM_MON_Dewpoint) == "system.index"] <- "DateTime"
colnames(ZM_MON_Dewpoint)[colnames(ZM_MON_Dewpoint) == "mean"] <- "ERA5_Dewpoint_temp_K"



ZM_MON_Temp <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Temp/ERA5_export_temp_ZM_MON.csv")
# Renaming columns in the ZM_MON_Temp data frame
colnames(ZM_MON_Temp)[colnames(ZM_MON_Temp) == "system.index"] <- "DateTime"
colnames(ZM_MON_Temp)[colnames(ZM_MON_Temp) == "mean"] <- "ERA5_Temp_K"


ZM_MON_LWR <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/ThermalRad/ERA5_export_ZM_MON_LWR.csv")
# Renaming columns in the ZM_MON_Temp data frame
colnames(ZM_MON_LWR)[colnames(ZM_MON_LWR) == "system.index"] <- "DateTime"
colnames(ZM_MON_LWR)[colnames(ZM_MON_LWR) == "mean"] <- "ERA5_LWR_J/M2"



ZM_MON_Pressure <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Pressure/ERA5_export_ZM_MON_Pressure.csv")
# Renaming columns in the ZM_MON_Temp data frame
colnames(ZM_MON_Pressure)[colnames(ZM_MON_Pressure) == "system.index"] <- "DateTime"
colnames(ZM_MON_Pressure)[colnames(ZM_MON_Pressure) == "mean"] <- "ERA5_Pressure_Pa"



ZM_MON_U_Wind <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/U_Wind/ERA5_export_ZM_MON_U_Wind.csv")
# Renaming columns in the ZM_MON_Temp data frame
colnames(ZM_MON_U_Wind)[colnames(ZM_MON_U_Wind) == "system.index"] <- "DateTime"
colnames(ZM_MON_U_Wind)[colnames(ZM_MON_U_Wind) == "mean"] <- "ERA5_U_Wind_m/s"



ZM_MON_V_Wind <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/V_Wind/ERA5_export_ZM_MON_V_Wind.csv")
# Renaming columns in the ZM_MON_Temp data frame
colnames(ZM_MON_V_Wind)[colnames(ZM_MON_V_Wind) == "system.index"] <- "DateTime"
colnames(ZM_MON_V_Wind)[colnames(ZM_MON_V_Wind) == "mean"] <- "ERA5_V_Wind_m/s"



ZM_MON_Precip <- read.csv("C:/Users/efa206/OneDrive - University of Exeter/Desktop/ERA5_Data/Rainfall/ERA5_export_ZM_MON_Precipitation.csv")
# Renaming columns in the ZM_MON_Temp data frame
colnames(ZM_MON_Precip)[colnames(ZM_MON_Precip) == "system.index"] <- "DateTime"
colnames(ZM_MON_Precip)[colnames(ZM_MON_Precip) == "mean"] <- "ERA5_Precip_m"


# List of datasets to combine
datasets <- list(ZM_MON_Solarad, ZM_MON_LWR, ZM_MON_Temp, ZM_MON_Dewpoint, ZM_MON_Pressure, ZM_MON_Precip, ZM_MON_U_Wind, ZM_MON_V_Wind)

# Combine all datasets in the list by "DateTime"
ZM_MON_Combined <- Reduce(function(x, y) merge(x, y, by = "DateTime", all = TRUE), datasets)



# Select columns you want to keep
ZM_MON_ERA5_MET <- ZM_MON_Combined %>% 
  select(-Site_Code.x, -datetime.x, -.geo.x, -Site_Code.y, -datetime.y, -.geo.y)


# Convert DateTime to a proper POSIXct object in UTC
ZM_MON_ERA5_MET$DateTime <- as.POSIXct(substr(ZM_MON_ERA5_MET$DateTime, 1, 15), 
                                       format = "%Y%m%dT%H", tz = "UTC")





#Convert variables to appropriate units 

#Convert ERA5_SSR_J/M2 from J/m² to W/m²
ZM_MON_ERA5_MET$`ERA5_SSR_W/M2` <- ZM_MON_ERA5_MET$`ERA5_SSR_J/M2` / 3600


#Convert ERA5_LWR_J/M2 from J/m² to W/m²
ZM_MON_ERA5_MET$`ERA5_LWR_W/M2` <- ZM_MON_ERA5_MET$`ERA5_LWR_J/M2` / 3600


# Convert ERA5_Dewpoint_temp_K from Kelvin to Celsius
ZM_MON_ERA5_MET$ERA5_Dewpoint_temp_C <- ZM_MON_ERA5_MET$ERA5_Dewpoint_temp_K - 273.15


# Convert ERA5_Temp_K from Kelvin to Celsius
ZM_MON_ERA5_MET$ERA5_Temp_C <- ZM_MON_ERA5_MET$ERA5_Temp_K - 273.15


# Convert from m to mm
ZM_MON_ERA5_MET$ERA5_Precip_mm <- ZM_MON_ERA5_MET$ERA5_Precip_m * 1000


# convert from mm to kg m-2 s-1
ZM_MON_ERA5_MET$`ERA5_Precip_kg/m2/s` <- ZM_MON_ERA5_MET$ERA5_Precip_mm / 3600


# Calculate wind speed using the U & V components
ZM_MON_ERA5_MET$`ERA5_Wind_Speed_m/s` <- sqrt(ZM_MON_ERA5_MET$`ERA5_U_Wind_m/s`^2 + ZM_MON_ERA5_MET$`ERA5_V_Wind_m/s`^2)



# Convert Pressure from Pa to hPa
ZM_MON_ERA5_MET <- ZM_MON_ERA5_MET %>%
  mutate(ERA5_Pressure_hPa = ERA5_Pressure_Pa / 100)

# Calculate Actual Vapor Pressure (e) in hPa using Dew Point Temperature in Celsius
ZM_MON_ERA5_MET <- ZM_MON_ERA5_MET %>%
  mutate(e_hPa = 6.112 * exp((17.67 * ERA5_Dewpoint_temp_C) / (ERA5_Dewpoint_temp_C + 243.5)))

# Calculate Specific Humidity (q) in kg/kg
ZM_MON_ERA5_MET <- ZM_MON_ERA5_MET %>%
  mutate(`ERA5_Specific_humidity_kg/kg` = (0.622 * e_hPa) / (ERA5_Pressure_hPa - (0.378 * e_hPa)))




# Interpolate data to get half hourly data'

# Step 1: Generate half-hourly timestamps
time_half_hourly <- seq(from = min(ZM_MON_ERA5_MET$DateTime), 
                        to = max(ZM_MON_ERA5_MET$DateTime), by = "30 mins")

# Step 2: Interpolate each variable to the half-hourly time step

# For Temperature (C)
temperature_half_hourly <- approx(x = ZM_MON_ERA5_MET$DateTime, 
                                  y = ZM_MON_ERA5_MET$ERA5_Temp_C, 
                                  xout = time_half_hourly)$y

# For Temperature (K)
temperature_K_half_hourly <- approx(x = ZM_MON_ERA5_MET$DateTime, 
                                    y = ZM_MON_ERA5_MET$ERA5_Temp_K, 
                                    xout = time_half_hourly)$y

# For Dewpoint Temperature 
dewpoint_half_hourly <- approx(x = ZM_MON_ERA5_MET$DateTime, 
                               y = ZM_MON_ERA5_MET$ERA5_Dewpoint_temp_C, 
                               xout = time_half_hourly)$y

# For Solar Surface Radiation (ERAS_SSR_W/M2)
ssr_half_hourly <- approx(x = ZM_MON_ERA5_MET$DateTime, 
                          y = ZM_MON_ERA5_MET$`ERA5_SSR_W/M2`, 
                          xout = time_half_hourly)$y

# For thermal radiation (ERA5_LWR_W/M2)
lwr_half_hourly <- approx(x = ZM_MON_ERA5_MET$DateTime,
                          y = ZM_MON_ERA5_MET$`ERA5_LWR_W/M2`,
                          xout = time_half_hourly)$y

# For wind speed
wndspd_half_hourly <- approx(x = ZM_MON_ERA5_MET$DateTime,
                             y = ZM_MON_ERA5_MET$`ERA5_Wind_Speed_m/s`,
                             xout = time_half_hourly)$y

# For pressure (Pa)
pressure_Pa_half_hourly <- approx(x = ZM_MON_ERA5_MET$DateTime,
                                  y = ZM_MON_ERA5_MET$`ERA5_Pressure_Pa`,
                                  xout = time_half_hourly)$y

# For pressure (hpa)
pressure_hPa_half_hourly <- approx(x = ZM_MON_ERA5_MET$DateTime,
                                   y = ZM_MON_ERA5_MET$`ERA5_Pressure_hPa`,
                                   xout = time_half_hourly)$y

# For specific humidity
specificHumidity_half_hourly <- approx(x = ZM_MON_ERA5_MET$DateTime,
                                       y = ZM_MON_ERA5_MET$`ERA5_Specific_humidity_kg/kg`,
                                       xout = time_half_hourly)$y


# For precipitation
precipitation_mm_half_hourly <- approx(x = ZM_MON_ERA5_MET$DateTime,
                                       y = ZM_MON_ERA5_MET$ERA5_Precip_mm,
                                       xout = time_half_hourly)$y


# For precipitation kg/m2/s
precipitation_kg_half_hourly <- approx(x = ZM_MON_ERA5_MET$DateTime,
                                       y = ZM_MON_ERA5_MET$`ERA5_Precip_kg/m2/s`,
                                       xout = time_half_hourly)$y


# Step 3: Combine the interpolated data into a new dataframe
ZM_MON_ERA5_MET_HH <- data.frame(
  DateTime = time_half_hourly,
  ERA5_Temp_C = temperature_half_hourly,
  ERA5_Temp_K = temperature_K_half_hourly,
  ERA5_Dewpoint_temp_C = dewpoint_half_hourly,
  `ERA5_SSR_W/M2` = ssr_half_hourly,
  `ERA5_LWR_W/M2` = lwr_half_hourly,
  `ERA5_Specific_humidity_kg/kg` = specificHumidity_half_hourly,
  `ERA5_Pressure_Pa` = pressure_Pa_half_hourly,
  `ERA5_Pressure_hPa` = pressure_hPa_half_hourly,
  `ERA5_Wind_Speed_m/s` = wndspd_half_hourly,
  ERA5_Precip_mm = precipitation_mm_half_hourly,
  `ERA5_Precip_kg/m2/s`= precipitation_kg_half_hourly
)






# Step 4: plot hourly vs interpolated for QC

ggplot() +
  geom_line(data = ZM_MON_ERA5_MET, aes(x = DateTime, y = ERA5_Temp_C), color = "red", size = 1.2) +
  geom_line(data = ZM_MON_ERA5_MET_HH, aes(x = DateTime, y = ERA5_Temp_C), color = "blue", linetype = "dashed", size = 1) +
  labs(title = "Hourly vs Half-Hourly Interpolated Temperature",
       x = "Date", y = "Temperature (K)") +
  theme_minimal()



# Subset the data for a specific time range that exists in the dataset (e.g., one day)
subset_data_hourly <- subset(ZM_MON_ERA5_MET, DateTime >= as.POSIXct("2005-11-11 00:00:00") & DateTime <= as.POSIXct("2005-11-12 00:00:00"))
subset_data_half_hourly <- subset(ZM_MON_ERA5_MET_HH, DateTime >= as.POSIXct("2005-11-11 00:00:00") & DateTime <= as.POSIXct("2005-11-12 00:00:00"))

# Plot the subsetted data
ggplot() +
  geom_point(data = subset_data_hourly, aes(x = DateTime, y = ERA5_Temp_C), color = "red", size = 1.5) +
  geom_point(data = subset_data_half_hourly, aes(x = DateTime, y = ERA5_Temp_C), color = "blue", size = 1) +
  labs(title = "Hourly vs Half-Hourly Interpolated Temperature (Zoomed In)",
       x = "Time", y = "Temperature (°C)") +
  theme_minimal()






# Calculating VPD using land air temperature and dew point temperature 
# Constants
Pmsl <- 1013.25  # mean sea level pressure in hPa
Z_m <- 1086.27    # altitude in meters (as provided)

# Calculate VPD for ERA5-Land
calculate_vpd <- function(temp_C, dewpoint_temp_C, Z_m) {
  # Calculate Pmst (equation 6)
  Pmst <- Pmsl * ((temp_C + 273.16) / (temp_C + 273.16 + 0.0065 * Z_m))^5.625
  
  # Corrected formula for fw (equation 5)
  fw <- 1 + 7 * 10^(-4) + 3.46 * 10^(-6) * Pmst
  
  # Calculate SVP (equation 3)
  SVP <- 6.112 * fw * exp((17.67 * temp_C) / (temp_C + 243.5))
  
  # Calculate AVP (equation 4)
  AVP <- 6.112 * fw * exp((17.67 * dewpoint_temp_C) / (dewpoint_temp_C + 243.5))
  
  # Calculate VPD (equation 2)
  VPD <- SVP - AVP
  
  return(VPD)
}

# Apply VPD calculation to the dataset
ZM_MON_ERA5_MET_HH <- ZM_MON_ERA5_MET_HH %>%
  mutate(VPD_hPa = calculate_vpd(ERA5_Temp_C, ERA5_Dewpoint_temp_C, Z_m))

