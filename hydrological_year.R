# ============================
# Hydrological-year starts per site from ERA5-Land hourly precipitation
# Inputs:
#   - Per-site hourly CSV/NetCDF with a time column + 'total_precipitation_hourly' (metres)
#   - site_meta.csv with: site, lat, lon, tz   (tz optional)
# Output:
#   - hydro_year_starts.csv with start DOY/month-day and onset DOY/month-day
# Notes:
#   - Assign each hourly value to the day it ENDED (shift -1h before as.Date)
#   - Uses 31-day smoothing on daily climatology
# Baseline: 1991–2020
# ============================

# libraries ----
library(dplyr)
library(readr)
library(lubridate)
library(stringr)
library(zoo)
library(lutz)
library(ncdf4)
library(purrr)
library(tidyr)
library(lutz)

# paths ----
data_dir <- "C:/Users/efa206/OneDrive - University of Exeter/Desktop/hydrological_year_data"
meta_path <- file.path(data_dir, "site_meta.csv")


# load site metadata ----
if (!file.exists(meta_path)) {
  stop("Missing 'site_meta.csv' in the data folder.\n",
       "Create a CSV with columns: site,lat,lon,tz (tz optional). Example:\n",
       "site,lat,lon,tz\nBW_GUM,-21.2,25.0,Africa/Gaborone\nSN_DHR,14.6,-16.2,Africa/Dakar\n")
}
site_meta <- read_csv(meta_path, show_col_types = FALSE) %>%
  mutate(tz = if ("tz" %in% names(.)) tz else NA_character_)

# Fill missing tz via lat/lon lookup; fallback to lon-based offset
if (any(is.na(site_meta$tz))) {
  site_meta <- site_meta %>%
    mutate(tz = ifelse(is.na(tz),
                       lutz::tz_lookup_coords(lat, lon, method = "accurate"),
                       tz))
}
site_meta <- site_meta %>% mutate(offset_h = round(lon/15))


# functions ----
to_local <- function(time_utc, tz, offset_h = 0L) {
  # Prefer real tz, else fixed offset (kept as UTC label)
  if (!is.na(tz) && nzchar(tz)) with_tz(time_utc, tz)
  else (time_utc + hours(offset_h)) %>% force_tz("UTC")
}



# CSV loader: find a time column + precip column in metres; parse '19910101_00' etc.
read_hourly_csv <- function(file) {
  x <- read_csv(file, guess_max = 100000, show_col_types = FALSE)
  time_col <- intersect(names(x), c("time","timestamp","datetime","date_time","date","valid_time"))
  p_cols   <- intersect(names(x), c("total_precipitation_hourly","tp","tp_m","precip_m",
                                    "tp_hourly_m","total_precipitation","precipitation","precip","mean"))
  if (length(time_col)==0 || length(p_cols)==0) {
    stop("CSV ", basename(file), " must have a time column and a precipitation column (metres).")
  }
  tt <- as.character(x[[time_col[1]]])
  # normalize to YYYYMMDD_HH
  tt <- gsub("-", "", tt); tt <- gsub(" ", "_", tt)
  if (!grepl("_", tt[1])) tt <- paste0(substr(tt,1,8), "_", substr(tt,9,10))
  t_utc <- lubridate::ymd_h(tt, tz = "UTC")
  if (all(is.na(t_utc))) stop("Can't parse timestamps in ", basename(file))
  tibble(time_utc = t_utc, tp_m = as.numeric(x[[p_cols[1]]]))
}



# NetCDF loader: common ERA5 variable names
read_hourly_nc <- function(file) {
  nc <- nc_open(file); on.exit(nc_close(nc))
  vname <- intersect(c("total_precipitation_hourly","tp"), names(nc$var))
  if (length(vname)==0) stop("NetCDF ", basename(file), " must contain 'total_precipitation_hourly' or 'tp'")
  tp <- as.numeric(ncvar_get(nc, vname[1]))
  time <- as.numeric(ncvar_get(nc, "time"))
  tunits <- ncatt_get(nc, "time", "units")$value
  origin_str <- sub("^hours since\\s+", "", tunits)
  origin <- as.POSIXct(origin_str, tz = "UTC")
  if (is.na(origin)) stop("Unrecognised time units: ", tunits)
  t_utc <- origin + hours(time)
  tibble(time_utc = t_utc, tp_m = tp)
}



read_hourly_any <- function(file) {
  ext <- tolower(tools::file_ext(file))
  if (ext %in% c("csv","txt")) return(read_hourly_csv(file))
  if (ext %in% c("nc","nc4","cdf")) return(read_hourly_nc(file))
  stop("Unsupported file type: ", basename(file))
}




# Hourly (metres) -> local daily totals (mm); assign to day the hour ENDED
hourly_to_daily_mm <- function(df_hourly, tz, offset_h = 0L) {
  df_hourly %>%
    mutate(tp_m = pmax(tp_m, 0)) %>%  # clip tiny negatives
    mutate(time_local = to_local(time_utc, tz, offset_h)) %>%
    mutate(date = as.Date(time_local - lubridate::hours(1)),  # key: -1h before as.Date
           mm   = tp_m * 1000) %>%                            # m -> mm
    group_by(date) %>%
    summarise(precip_mm = sum(mm, na.rm = TRUE),
              n_hours   = dplyr::n(),
              .groups   = "drop") %>%
    mutate(qc_ok = n_hours >= 20)
}



# From daily totals -> hydrological-year start DOY (1..365)
hydro_start_from_daily <- function(daily_mm,
                                   baseline = c(1991, 2020),
                                   smooth_k = 31,
                                   buffer_days = 30) {
  d <- daily_mm %>%
    filter(year(date) >= baseline[1], year(date) <= baseline[2]) %>%
    filter(!(month(date)==2 & mday(date)==29)) %>%       # drop Feb 29
    mutate(doy = yday(date))
  
  # daily climatology Qi for DOY 1..365
  clim <- d %>%
    group_by(doy) %>%
    summarise(Qi = mean(precip_mm, na.rm = TRUE), .groups = "drop") %>%
    right_join(tibble(doy = 1:365), by = "doy") %>%
    arrange(doy) %>%
    mutate(Qi = tidyr::replace_na(Qi, 0))
  
  
  
  # smooth Qi (centered rolling mean)
  Qi <- clim$Qi
  if (smooth_k > 1) {
    pad <- floor(smooth_k/2)
    Qi_pad <- c(tail(Qi, pad), Qi, head(Qi, pad))
    Qi_s   <- zoo::rollmean(Qi_pad, k = smooth_k, align = "center", fill = NA)
    Qi_s   <- Qi_s[(pad+1):(pad+365)]
  } else Qi_s <- Qi
  
  Qbar <- mean(Qi_s, na.rm = TRUE)
  C    <- cumsum(Qi_s - Qbar)
  
  # onset = day after minimum C(d); hydro-year start = onset - buffer
  min_idx   <- which.min(C)                 # 1..365
  onset_doy <- ifelse(min_idx == 365, 1, min_idx + 1)
  start_doy <- ((onset_doy - buffer_days - 1) %% 365) + 1
  
  list(start_doy = start_doy, onset_doy = onset_doy)
}

doy_to_md <- function(doy) format(as.Date(doy - 1, origin = "2001-01-01"), "%b-%d")



# find files for each site (file names contain the site code) ----
all_files <- list.files(data_dir, full.names = TRUE)
data_files <- all_files[grepl("\\.(csv|txt|nc|nc4|cdf)$", tolower(all_files)) &
                          !grepl("site_meta\\.csv$", all_files)]

site_file_map <- purrr::map_chr(site_meta$site, function(s) {
  hits <- data_files[str_detect(basename(data_files), fixed(s))]
  if (length(hits) == 0) NA_character_ else hits[1]
})
if (any(is.na(site_file_map))) {
  stop("No data file found for site(s): ",
       paste(site_meta$site[is.na(site_file_map)], collapse = ", "),
       "\nMake sure each file name contains its site code (e.g., 'BW_GUM_*.csv').")
}



# main loop: per-site processing ----
results <- purrr::pmap_dfr(
  list(site = site_meta$site, file = site_file_map, tz = site_meta$tz, offh = site_meta$offset_h),
  function(site, file, tz, offh) {
    message("Processing ", site, " from ", basename(file))
    h <- read_hourly_any(file)
    d <- hourly_to_daily_mm(h, tz = tz, offset_h = offh)
    hs <- hydro_start_from_daily(d, baseline = c(1991, 2020), smooth_k = 31, buffer_days = 30)
    tibble(
      site      = site,
      start_doy = hs$start_doy,
      onset_doy = hs$onset_doy,
      start_md  = doy_to_md(hs$start_doy),
      onset_md  = doy_to_md(hs$onset_doy),
      tz_used   = ifelse(is.na(tz) || !nzchar(tz), paste0("offset_", offh, "h"), tz),
      file      = basename(file)
    )
  }
)



# save & print ----
out_path <- file.path(data_dir, "hydro_year_starts.csv")
write_csv(results, out_path)
print(results, n = nrow(results))
cat("\nSaved: ", out_path, "\n")
