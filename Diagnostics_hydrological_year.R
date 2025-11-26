# =========================
# DIAGNOSTIC: rainy-season onset by cumulative anomaly (min vs max), ALL SITES
# Input folder: "C:/Users/efa206/OneDrive - University of Exeter/Desktop/hydrological_year_data"
# Requires: site_meta.csv with columns: site,lat,lon,tz  (tz optional)
# Files: per-site hourly precip (metres) with a time column + 'total_precipitation_hourly'
# Outputs:
#   - diagnostics/<site>_onset_diagnostic.png
#   - diagnostics/diagnostic_stats.csv (summary table for all sites)
# Notes:
#   - Assign each hourly value to the day it ENDED (shift -1h before as.Date)
#   - Uses 31-day smoothing on daily climatology
# Baseline: 1991–2020
# =========================

# library ----
library(dplyr)
library(readr)
library(lubridate)
library(stringr)
library(zoo)
library(lutz)
library(ncdf4)
library(purrr)
library(tidyr)
library(ggplot2)
library(scales)
library(patchwork)

# paths & dirs ----
data_dir <- "C:/Users/efa206/OneDrive - University of Exeter/Desktop/hydrological_year_data"
meta_path <- file.path(data_dir, "site_meta.csv")
out_dir  <- file.path(data_dir, "diagnostics")
dir.create(out_dir, showWarnings = FALSE)

# load metadata ----
site_meta <- read_csv(meta_path, show_col_types = FALSE) %>%
  mutate(tz = if ("tz" %in% names(.)) tz else NA_character_)
if (any(is.na(site_meta$tz))) {
  site_meta <- site_meta %>%
    mutate(tz = ifelse(is.na(tz),
                       lutz::tz_lookup_coords(lat, lon, method = "accurate"),
                       tz))
}
site_meta <- site_meta %>% mutate(offset_h = round(lon/15))

# helpers ----
to_local <- function(time_utc, tz, offset_h = 0L) {
  if (!is.na(tz) && nzchar(tz)) with_tz(time_utc, tz) else (time_utc + hours(offset_h)) %>% force_tz("UTC")
}

# Robust CSV reader (handles "19910101_00" etc.)
read_hourly_csv <- function(file) {
  x <- read_csv(file, guess_max = 100000, show_col_types = FALSE)
  time_col <- intersect(names(x), c("time","timestamp","datetime","date_time","date","valid_time"))
  p_cols   <- intersect(names(x), c("total_precipitation_hourly","tp","tp_m","precip_m",
                                    "tp_hourly_m","total_precipitation","precipitation","precip","mean"))
  if (length(time_col)==0 || length(p_cols)==0) stop("CSV needs a time col + precip col (metres): ", basename(file))
  tt <- as.character(x[[time_col[1]]])
  # normalise to YYYYMMDD_HH
  tt <- gsub("-", "", tt); tt <- gsub(" ", "_", tt)
  if (!grepl("_", tt[1])) tt <- paste0(substr(tt,1,8), "_", substr(tt,9,10))
  t_utc <- lubridate::ymd_h(tt, tz = "UTC")
  if (all(is.na(t_utc))) stop("Can't parse timestamps in ", basename(file))
  tibble(time_utc = t_utc, tp_m = as.numeric(x[[p_cols[1]]]))
}

read_hourly_nc <- function(file) {
  nc <- ncdf4::nc_open(file); on.exit(ncdf4::nc_close(nc))
  vname <- intersect(c("total_precipitation_hourly","tp"), names(nc$var))
  if (length(vname)==0) stop("NC must have 'total_precipitation_hourly' or 'tp': ", basename(file))
  tp <- as.numeric(ncdf4::ncvar_get(nc, vname[1]))
  time <- as.numeric(ncdf4::ncvar_get(nc, "time"))
  tunits <- ncdf4::ncatt_get(nc, "time", "units")$value
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
  stop("Unsupported file: ", basename(file))
}

# Hourly (metres) -> local daily totals (mm); assign to the day the hour ENDED
hourly_to_daily_mm <- function(df_hourly, tz, offset_h = 0L) {
  df_hourly %>%
    mutate(tp_m = pmax(tp_m, 0)) %>%
    mutate(time_local = to_local(time_utc, tz, offset_h)) %>%
    mutate(date = as.Date(time_local - lubridate::hours(1)),  # <-- key: shift back 1h
           mm   = tp_m * 1000) %>%                            # m -> mm
    group_by(date) %>%
    summarise(precip_mm = sum(mm, na.rm = TRUE),
              n_hours   = dplyr::n(), .groups = "drop")
}

doy_to_md <- function(doy) as.Date(doy - 1, origin = "2001-01-01") %>% format("%b-%d")

# Build Qi and C(d), show both min(C) and max(C) interpretations
diagnose_onset <- function(daily_mm,
                           baseline = c(1991, 2020),
                           smooth_k = 31,         # stronger smoothing
                           buffer_days = 30) {
  d <- daily_mm %>%
    filter(year(date) >= baseline[1], year(date) <= baseline[2]) %>%
    filter(!(month(date)==2 & mday(date)==29)) %>%
    mutate(doy = yday(date))
  
  clim <- d %>%
    group_by(doy) %>%
    summarise(Qi = mean(precip_mm, na.rm = TRUE), .groups = "drop") %>%
    right_join(tibble(doy = 1:365), by = "doy") %>%
    arrange(doy) %>%
    mutate(Qi = replace_na(Qi, 0))
  
  # smooth Qi
  Qi <- clim$Qi
  if (smooth_k > 1) {
    pad <- floor(smooth_k/2)
    Qi_pad <- c(tail(Qi, pad), Qi, head(Qi, pad))
    Qi_s   <- zoo::rollmean(Qi_pad, k = smooth_k, align = "center", fill = NA)
    Qi_s   <- Qi_s[(pad+1):(pad+365)]
  } else Qi_s <- Qi
  
  Qbar <- mean(Qi_s, na.rm = TRUE)
  C    <- cumsum(Qi_s - Qbar)
  
  # extrema
  i_min <- which.min(C)                 # 1..365
  i_max <- which.max(C)
  
  onset_min <- ifelse(i_min == 365, 1, i_min + 1)
  onset_max <- ifelse(i_max == 365, 1, i_max + 1)
  
  start_min <- ((onset_min - buffer_days - 1) %% 365) + 1
  start_max <- ((onset_max - buffer_days - 1) %% 365) + 1
  
  list(
    clim_df = tibble(doy = 1:365, Qi = Qi_s),
    c_df    = tibble(doy = 1:365, C = C),
    stats   = tibble(
      method = c("min(C) + 1", "max(C) + 1"),
      onset_doy = c(onset_min, onset_max),
      start_doy = c(start_min, start_max),
      onset_md  = c(doy_to_md(onset_min), doy_to_md(onset_max)),
      start_md  = c(doy_to_md(start_min), doy_to_md(start_max))
    )
  )
}

plot_diagnostic <- function(site, diag_list, out_png) {
  clim_df <- diag_list$clim_df
  c_df    <- diag_list$c_df
  s       <- diag_list$stats
  
  col_min <- "#1f77b4"
  col_max <- "#d62728"
  
  p1 <- ggplot(clim_df, aes(doy, Qi)) +
    geom_line() +
    geom_vline(xintercept = s$onset_doy[s$method=="min(C) + 1"], linetype = "solid", color = col_min) +
    geom_vline(xintercept = s$start_doy[s$method=="min(C) + 1"], linetype = "dashed", color = col_min) +
    geom_vline(xintercept = s$onset_doy[s$method=="max(C) + 1"], linetype = "solid", color = col_max) +
    geom_vline(xintercept = s$start_doy[s$method=="max(C) + 1"], linetype = "dashed", color = col_max) +
    annotate("text", x = s$onset_doy[1], y = max(clim_df$Qi, na.rm=TRUE)*0.95,
             label = "onset[min(C)]", hjust = -0.05, size = 3, color = col_min) +
    annotate("text", x = s$onset_doy[2], y = max(clim_df$Qi, na.rm=TRUE)*0.85,
             label = "onset[max(C)]", hjust = -0.05, size = 3, color = col_max) +
    labs(title = paste0(site, " — Daily precip climatology (Qi, mm/day)"),
         x = "Day of year", y = "Qi (mm/day)") +
    scale_x_continuous(breaks = c(1, 60, 120, 180, 240, 300, 365),
                       labels = function(z) format(as.Date(z-1, "2001-01-01"), "%b-%d")) +
    theme_minimal(base_size = 11)
  
  p2 <- ggplot(c_df, aes(doy, C)) +
    geom_line() +
    geom_point(data = c_df %>% slice(which.min(C)), color = col_min, size = 2) +
    geom_point(data = c_df %>% slice(which.max(C)), color = col_max, size = 2) +
    geom_vline(xintercept = s$onset_doy[s$method=="min(C) + 1"], linetype = "solid", color = col_min) +
    geom_vline(xintercept = s$onset_doy[s$method=="max(C) + 1"], linetype = "solid", color = col_max) +
    labs(title = "Cumulative anomaly C(d) = cumsum(Qi - mean(Qi))",
         x = "Day of year", y = "C(d) (mm)") +
    scale_x_continuous(breaks = c(1, 60, 120, 180, 240, 300, 365),
                       labels = function(z) format(as.Date(z-1, "2001-01-01"), "%b-%d")) +
    theme_minimal(base_size = 11)
  
  g <- p1 / p2 + plot_layout(heights = c(2, 2)) +
    plot_annotation(
      subtitle = paste0(
        "Solid = onset; Dashed = hydro-year start (onset − 30 days)\n",
        "min(C): start ", s$start_md[1], " | onset ", s$onset_md[1], "    ",
        "max(C): start ", s$start_md[2], " | onset ", s$onset_md[2]
      )
    )
  
  ggsave(out_png, g, width = 10, height = 7, dpi = 150)
}

# find files (name contains site code) ----
all_files <- list.files(data_dir, full.names = TRUE)
data_files <- all_files[grepl("\\.(csv|txt|nc|nc4|cdf)$", tolower(all_files)) &
                          !grepl("site_meta\\.csv$", all_files)]

site_file <- function(site) {
  hits <- data_files[str_detect(basename(data_files), fixed(site))]
  if (length(hits)==0) NA_character_ else hits[1]
}

# run diagnostics for ALL sites with data ----
stats_list <- list()
for (site in site_meta$site) {
  file <- site_file(site)
  if (is.na(file)) {
    message("Skipping ", site, " — no matching file found.")
    next
  }
  message("Diagnosing ", site, " ...")
  meta <- site_meta %>% filter(.data$site == site)
  hourly <- read_hourly_any(file)
  daily  <- hourly_to_daily_mm(hourly, tz = meta$tz[1], offset_h = meta$offset_h[1])
  
  diag <- diagnose_onset(daily, baseline = c(1991,2020), smooth_k = 31, buffer_days = 30)
  
  # print a tiny table to console
  print(diag$stats %>% mutate(site = site, .before = 1))
  
  out_png <- file.path(out_dir, paste0(site, "_onset_diagnostic.png"))
  plot_diagnostic(site, diag, out_png)
  message("Saved: ", out_png)
  
  stats_list[[site]] <- diag$stats %>% mutate(site = site, .before = 1)
}

# save combined stats for all sites ----
if (length(stats_list)) {
  diag_stats <- bind_rows(stats_list)
  write_csv(diag_stats, file.path(out_dir, "diagnostic_stats.csv"))
  message("Saved: ", file.path(out_dir, "diagnostic_stats.csv"))
}
cat("\nDone. Open the PNGs in: ", out_dir, "\n")
