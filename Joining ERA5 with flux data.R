
# for BW_GUM ----
# Perform a full join on the date and time columns
BW_GUM_DATA <- full_join(BW_GUM_Flux_new, BW_GUM_ERA5_MET_HH_temp, by = c("date" = "DateTime"))

# Rename the column
BW_GUM_DATA <- BW_GUM_DATA %>%
  rename(DateTime = date)

# Export the data frame to the specified directory
write.csv(BW_GUM_DATA, "C:/Users/efa206/OneDrive - University of Exeter/Desktop/Data_for JULES/BW_GUM_DATA.csv", row.names = FALSE)


# for BW_NXR ----
# Perform a full join on the date and time columns
BW_NXR_DATA <- full_join(BW_NXR_Flux_new, BW_NXR_ERA5_MET_HH_temp, by = c("date" = "DateTime"))

# Rename the column
BW_NXR_DATA <- BW_NXR_DATA %>%
  rename(DateTime = date)

# Export the data frame to the specified directory
write.csv(BW_NXR_DATA, "C:/Users/efa206/OneDrive - University of Exeter/Desktop/Data_for JULES/BW_NXR_DATA.csv", row.names = FALSE)





# for CG_TCH ----
# Perform a full join on the date and time columns
CG_TCH_DATA <- full_join(CG_TCH_Flux_new, CG_TCH_ERA5_MET_HH_temp, by = c("TIMESTAMP_START" = "DateTime"))

# Rename the column
CG_TCH_DATA <- CG_TCH_DATA %>%
  rename(DateTime = TIMESTAMP_START)

# Export the data frame to the specified directory
write.csv(CG_TCH_DATA, "C:/Users/efa206/OneDrive - University of Exeter/Desktop/Data_for JULES/CG_TCH_DATA.csv", row.names = FALSE)





# for GH_ANK ----
# Perform a full join on the date and time columns
GH_ANK_DATA <- full_join(GH_ANK_Flux_new, GH_ANK_ERA5_MET_HH_temp, by = c("TIMESTAMP_START" = "DateTime"))

# Rename the column
GH_ANK_DATA <- GH_ANK_DATA %>%
  rename(DateTime = TIMESTAMP_START)

# Export the data frame to the specified directory
write.csv(GH_ANK_DATA, "C:/Users/efa206/OneDrive - University of Exeter/Desktop/Data_for JULES/GH_ANK_DATA.csv", row.names = FALSE)





# for ML_AGG ----
# Perform a full join on the date and time columns
ML_AGG_DATA <- full_join(ML_AgG_Flux_new, ML_AGG_ERA5_MET_HH_temp, by = c("date_end_UTC" = "DateTime"))

# Rename the column
ML_AGG_DATA <- ML_AGG_DATA %>%
  rename(DateTime = date_end_UTC)

# Export the data frame to the specified directory
write.csv(ML_AGG_DATA, "C:/Users/efa206/OneDrive - University of Exeter/Desktop/Data_for JULES/ML_AGG_DATA.csv", row.names = FALSE)

# I had to change the interpolation to match standard time points i.e 30mins-00mins instead to the arbitrary 15mins-45mins, and this is the line of code to save it locally
write.csv(ML_AGG_ERA5_MET_HH_temp, "C:/Users/efa206/OneDrive - University of Exeter/Desktop/New_Met_Data/ML_AGG_DATA_2.csv", row.names = FALSE)


# for NE_WAF ----
# Perform a full join on the date and time columns
NE_WAF_DATA <- full_join(NE_WaF_Flux_new, NE_WAF_ERA5_MET_HH_temp, by = c("date_end_UTC" = "DateTime"))

# Rename the column
NE_WAF_DATA <- NE_WAF_DATA %>%
  rename(DateTime = date_end_UTC)

# Export the data frame to the specified directory
write.csv(NE_WAF_DATA, "C:/Users/efa206/OneDrive - University of Exeter/Desktop/Data_for JULES/NE_WAF_DATA.csv", row.names = FALSE)


# for NE_WAM ----
# Perform a full join on the date and time columns
NE_WAM_DATA <- full_join(NE_WaM_Flux_new, NE_WAM_ERA5_MET_HH_temp, by = c("date_end_UTC" = "DateTime"))

# Rename the column
NE_WAM_DATA <- NE_WAM_DATA %>%
  rename(DateTime = date_end_UTC)

# Export the data frame to the specified directory
write.csv(NE_WAM_DATA, "C:/Users/efa206/OneDrive - University of Exeter/Desktop/Data_for JULES/NE_WAM_DATA.csv", row.names = FALSE)





# for SD_DEM ----
# Perform a full join on the date and time columns
SD_DEM_DATA <- full_join(SD_DEM_Flux_new, SD_DEM_ERA5_MET_HH_temp, by = c("TIMESTAMP_START" = "DateTime"))

# Rename the column
SD_DEM_DATA <- SD_DEM_DATA %>%
  rename(DateTime = TIMESTAMP_START)

# Export the data frame to the specified directory
write.csv(SD_DEM_DATA, "C:/Users/efa206/OneDrive - University of Exeter/Desktop/Data_for JULES/SD_DEM_DATA.csv", row.names = FALSE)





# for SN_DHR ----
# Perform a full join on the date and time columns
SN_DHR_DATA <- full_join(SN_Dhr_Flux_new, SN_DHR_ERA5_MET_HH_temp, by = c("DateTime2" = "DateTime"))

# Rename the column
SN_DHR_DATA <- SN_DHR_DATA %>%
  rename(DateTime = DateTime2)

# Export the data frame to the specified directory
write.csv(SN_DHR_DATA, "C:/Users/efa206/OneDrive - University of Exeter/Desktop/Data_for JULES/SN_DHR_DATA.csv", row.names = FALSE)





# for SN_NKR ----
# Perform a full join on the date and time columns
SN_NKR_DATA <- full_join(SN_Nkr_Flux_new, SN_NKR_ERA5_MET_HH_temp, by = c("date_end_UTC" = "DateTime"))

# Rename the column
SN_NKR_DATA <- SN_NKR_DATA %>%
  rename(DateTime = date_end_UTC)

# Export the data frame to the specified directory
write.csv(SN_NKR_DATA, "C:/Users/efa206/OneDrive - University of Exeter/Desktop/Data_for JULES/SN_NKR_DATA.csv", row.names = FALSE)





# for SN_RAG ----
# Perform a full join on the date and time columns
SN_RAG_DATA <- full_join(SN_RAG_Flux_new, SN_RAG_ERA5_MET_HH_temp, by = c("date_end_UTC" = "DateTime"))

# Rename the column
SN_RAG_DATA <- SN_RAG_DATA %>%
  rename(DateTime = date_end_UTC)

# Export the data frame to the specified directory
write.csv(SN_RAG_DATA, "C:/Users/efa206/OneDrive - University of Exeter/Desktop/Data_for JULES/SN_RAG_DATA.csv", row.names = FALSE)


# I had to change the interpolation to match standard time points i.e 30mins-00mins instead to the arbitrary 15mins-45mins, and this is the line of code to save it locally
write.csv(SN_RAG_ERA5_MET_HH_temp, "C:/Users/efa206/OneDrive - University of Exeter/Desktop/New_Met_Data/SN_RAG_DATA_2.csv", row.names = FALSE)









# for UG_JIN ----
# Perform a full join on the date and time columns
UG_JIN_DATA <- full_join(UG_JIN_Flux_new, UG_JIN_ERA5_MET_HH_temp, by = c("DateTime" = "DateTime"))

# Rename the column
UG_JIN_DATA <- UG_JIN_DATA %>%
  rename(DateTime = DateTime)

# Export the data frame to the specified directory
write.csv(UG_JIN_DATA, "C:/Users/efa206/OneDrive - University of Exeter/Desktop/Data_for JULES/UG_JIN_DATA.csv", row.names = FALSE)









# for ZA_CATH_2016 ----
# Perform a full join on the date and time columns
ZA_CATH_DATA_2016 <- full_join(ZA_cath_Flux_new, ZA_CATH_ERA5_MET_HH_temp, by = c("TIMESTAMP" = "DateTime"))

# Rename the column
ZA_CATH_DATA_2016 <- ZA_CATH_DATA_2016 %>%
  rename(DateTime = TIMESTAMP)

# Export the data frame to the specified directory
write.csv(ZA_CATH_DATA_2016, "C:/Users/efa206/OneDrive - University of Exeter/Desktop/Data_for JULES/ZA_CATH_DATA_2016.csv", row.names = FALSE)

# for ZA_CATH_2014 ----
# Perform a full join on the date and time columns
ZA_CATH_DATA_2014 <- full_join(ZA_Cath_Flux_2014, ZA_CATH_ERA5_MET_HH_temp_14, by = c("TIMESTAMP" = "DateTime"))

# Rename the column
ZA_CATH_DATA_2014 <- ZA_CATH_DATA_2014 %>%
  rename(DateTime = TIMESTAMP)

# Export the data frame to the specified directory
write.csv(ZA_CATH_DATA_2014, "C:/Users/efa206/OneDrive - University of Exeter/Desktop/Data_for JULES/ZA_CATH_DATA_2014.csv", row.names = FALSE)









# for ZA_KRU ----
# Perform a full join on the date and time columns
ZA_KRU_DATA <- full_join(ZA_Kru_Flux_new, ZA_KRU_ERA5_MET_HH_temp, by = c("TIMESTAMP_START" = "DateTime"))

# Rename the column
ZA_KRU_DATA <- ZA_KRU_DATA %>%
  rename(DateTime = TIMESTAMP_START)

# Export the data frame to the specified directory
write.csv(ZA_KRU_DATA, "C:/Users/efa206/OneDrive - University of Exeter/Desktop/Data_for JULES/ZA_KRU_DATA.csv", row.names = FALSE)





# for ZA_WGN ----
# Perform a full join on the date and time columns
ZA_WGN_DATA <- full_join(ZA_Wgn_Flux_new, ZA_WGN_ERA5_MET_HH_temp, by = c("period_end" = "DateTime"))


# Rename the column
ZA_WGN_DATA <- ZA_WGN_DATA %>%
  rename(DateTime = period_end)

# Sort the data frame in ascending order by the DateTime column
ZA_WGN_DATA <- ZA_WGN_DATA %>%
  arrange(DateTime)


# Export the data frame to the specified directory
write.csv(ZA_WGN_DATA, "C:/Users/efa206/OneDrive - University of Exeter/Desktop/Data_for JULES/ZA_WGN_DATA.csv", row.names = FALSE)





# for ZM_MON ----
# Perform a full join on the date and time columns
ZM_MON_DATA <- full_join(ZM_Mon_Flux_new, ZM_MON_ERA5_MET_HH_temp, by = c("TIMESTAMP_START" = "DateTime"))

# Rename the column
ZM_MON_DATA <- ZM_MON_DATA %>%
  rename(DateTime = TIMESTAMP_START)

# Export the data frame to the specified directory
write.csv(ZM_MON_DATA, "C:/Users/efa206/OneDrive - University of Exeter/Desktop/Data_for JULES/ZM_MON_DATA.csv", row.names = FALSE)




