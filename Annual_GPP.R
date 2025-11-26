# Pipeline ----
# - Annual GPP with calendar-year coverage (>= 80%) and matched days 
# - Uses your ncd_to_df() / time_bounds
# - Obs: µmol CO2 m^-2 s^-1 -> kg C m^-2 s^-1 (×1.2e-8), half-hourly NA -> 0 (when NA = no GPP)
# - Daily totals = daily_mean_flux * 86400
# - Inner-join Obs/JULES by date (same-day mask), then coverage per calendar year (>=80%)
# - Annual sums from the joined days only
# - Plots: all sites; then sites with >=4 kept years
# - Metrics: r, RMSE, MAE, bias, CVs, CV ratio, NSE, KGE, OLS slope

# load libraries ----
library(tidyverse)
library(lubridate)
library(ncdf4)
library(purrr)
library(stringr)
library(plotrix)
library(RColorBrewer)
library(openair)
library(ggplot2)

# function to load ncd files ----
ncd_start <- function(ncd){
  if (inherits(ncd, "character")){
    ncd <- nc_open(ncd); on.exit(nc_close(ncd))
  }
  time_units <- ncatt_get(ncd, "time_bounds")$units
  gsub("[^0-9:-]", " ", time_units) |> trimws() |> ymd_hms()
}

ncd_to_df <- function(ncd, label=NULL) {
  ncd <- nc_open(ncd); on.exit(nc_close(ncd))
  start_date <- ncd_start(ncd)
  
  multi_dim_to_df <- function(x, .name){
    x <- as_tibble(t(x), .name_repair="unique")
    rename_with(x, ~paste(.name, str_replace(colnames(x), "...", ""), sep="_"))
  }
  vars_to_df <- function(var){
    vals <- ncvar_get(ncd, var)
    if (length(dim(vals)) > 1) suppressMessages(multi_dim_to_df(vals, var)) else tibble(!!var := (vals))
  }
  var_names <- setdiff(names(ncd$var), c("latitude", "longitude"))
  ncd_df <- map(var_names, vars_to_df) |> reduce(bind_cols) |>
    mutate(across(c("time_bounds_1", "time_bounds_2"), ~ start_date + .x))
  if (!is.null(label)) ncd_df <- mutate(ncd_df, label = label)
  attr(ncd_df, "lat")  <- ncvar_get(ncd,"latitude")
  attr(ncd_df, "long") <- ncvar_get(ncd,"longitude")
  ncd_df
}


# Configuration to load directory & data ----
csv_folder <- "C:/Users/efa206/OneDrive - University of Exeter/Desktop/combined_data_with_ET"
ncd_folder <- "C:/Users/efa206/OneDrive - University of Exeter/Desktop/JULES_Output/File_1000"
COVERAGE_THRESH <- 0.80   # 80% to accommodate SN_NKR
                          # 90% which would be ideal would eliminate SN_NKR which is missing only 1 month and half 

site_map <- tribble(
  ~site_label,       ~csv_file,                   ~ncd_file,
  "BW_GUM",          "BW_GUM_with_ET.csv",        "part6_BW_GUM-JULES_vn7.4.D.nc",
  "BW_NXR",          "BW_NXR_with_ET.csv",        "part6_BW_NXR-JULES_vn7.4.D.nc",
  "CG_TCH",          "CG_TCH_with_ET.csv",        "part6_CG_TCH-JULES_vn7.4.D.nc",
  "GH_ANK",          "GH_ANK_with_ET.csv",        "part6_GH_ANK-JULES_vn7.4.D.nc",
  "ML_AGG",          "ML_AGG_with_ET.csv",        "part6_ML_AGG-JULES_vn7.4.D.nc",
  "NE_WAF",          "NE_WAF_with_ET.csv",        "part6_NE_WAF-JULES_vn7.4.D.nc",
  "NE_WAM",          "NE_WAM_with_ET.csv",        "part6_NE_WAM-JULES_vn7.4.D.nc",
  "SD_DEM",          "SD_DEM_with_ET.csv",        "part6_SD_DEM-JULES_vn7.4.D.nc",
  "SN_DHR",          "SN_DHR_with_ET.csv",        "part6_SN_DHR-JULES_vn7.4.D.nc",
  "SN_NKR",          "SN_NKR_with_ET.csv",        "part6_SN_NKR-JULES_vn7.4.D.nc",
  "SN_RAG",          "SN_RAG_with_ET.csv",        "part6_SN_RAG-JULES_vn7.4.D.nc",
  "UG_JIN",          "UG_JIN_with_ET.csv",        "part6_UG_JIN-JULES_vn7.4.D.nc",
  "ZA_CATH2016",     "ZA_CATH2016_with_ET.csv",   "part6_ZA_CATH_16-JULES_vn7.4.D.nc",
  "ZA_KRU",          "ZA_KRU_with_ET.csv",        "part6_ZA_KRU-JULES_vn7.4.D.nc",
  "ZA_WGN",          "ZA_WGN_with_ET.csv",        "part6_ZA_WGN-JULES_vn7.4.D.nc",
  "ZM_MON",          "ZM_MON_with_ET.csv",        "part6_ZM_MON-JULES_vn7.4.D.nc"
)

# daily totals per site (Obs & JULES), then inner-join by date ----
daily_joined <- function(csv_path, nc_path) {
  # JULES daily totals from time_bounds (dt ~ 86400)
  jules_daily <- ncd_to_df(nc_path) %>%
    transmute(
      date   = as_date(time_bounds_2),
      dt_sec = as.numeric(difftime(time_bounds_2, time_bounds_1, units = "secs")),
      gpp_gb = gpp_gb,
      GPP_mod_day = gpp_gb * dt_sec           # kg C m^-2 day^-1
    ) %>%
    group_by(date) %>%
    summarise(GPP_mod_day = sum(GPP_mod_day, na.rm = TRUE), .groups = "drop")  # (safe even if multiple records/day)
  
  # Obs: convert µmol/s -> kgC/s, set sub-daily NA->0 (when NA == no GPP), daily mean * 86400
  obs_daily <- readr::read_csv(csv_path, show_col_types = FALSE) %>%
    transmute(
      TIMESTAMP = ymd_hms(TIMESTAMP),
      date      = as_date(TIMESTAMP),
      GPP_kgCs  = replace_na(GPP, 0) * 1.2e-8
    ) %>%
    group_by(date) %>%
    summarise(GPP_obs_day = 86400 * mean(GPP_kgCs, na.rm = TRUE), .groups = "drop")
  
  # Inner join = same-day mask
  inner_join(obs_daily, jules_daily, by = "date")
}

# apply coverage rule (>= 80%) and aggregate to annual ----
annual_for_site <- function(site_label, csv_path, nc_path) {
  dj <- daily_joined(csv_path, nc_path) %>%
    mutate(year = year(date))
  
  # expected days per calendar year (leap aware)
  keep_years <- dj %>%
    group_by(year) %>%
    summarise(
      joint_days = n(),
      exp_days   = if_else(leap_year(year), 366L, 365L),
      coverage   = joint_days / exp_days,
      .groups = "drop"
    ) %>%
    filter(coverage >= COVERAGE_THRESH)
  
  # annual sums on the kept years (joined-day mask already applied)
  dj %>%
    semi_join(keep_years, by = "year") %>%
    group_by(year) %>%
    summarise(
      GPP_obs_y = sum(GPP_obs_day, na.rm = TRUE),
      GPP_mod_y = sum(GPP_mod_day, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(SiteID = site_label, coverage = keep_years$coverage[match(year, keep_years$year)]) %>%
    relocate(SiteID, year, coverage)
}

# run all sites ----
annual_all <- purrr::map_dfr(seq_len(nrow(site_map)), function(i){
  message("Processing ", site_map$site_label[i], " ...")
  annual_for_site(
    site_label = site_map$site_label[i],
    csv_path   = file.path(csv_folder, site_map$csv_file[i]),
    nc_path    = file.path(ncd_folder, site_map$ncd_file[i])
  )
})






# IAV metrics (after coverage filter) ----
NSE <- function(o, m){ 1 - sum((m - o)^2) / sum((o - mean(o))^2) }
KGE <- function(o, m){
  r  <- cor(o, m)
  alpha <- sd(m)/sd(o)
  beta  <- mean(m)/mean(o)
  1 - sqrt((r - 1)^2 + (alpha - 1)^2 + (beta - 1)^2)
}

metrics_all <- annual_all %>%
  filter(!is.na(GPP_obs_y) & !is.na(GPP_mod_y)) %>%
  group_by(SiteID) %>%
  summarise(
    n_years = n(),
    r       = if (n() >= 2) cor(GPP_obs_y, GPP_mod_y) else NA_real_,
    rmse    = sqrt(mean((GPP_mod_y - GPP_obs_y)^2)),
    mae     = mean(abs(GPP_mod_y - GPP_obs_y)),
    bias    = mean(GPP_mod_y - GPP_obs_y),
    cv_obs  = sd(GPP_obs_y)/mean(GPP_obs_y),
    cv_mod  = sd(GPP_mod_y)/mean(GPP_mod_y),
    cv_ratio= cv_mod / cv_obs,
    nrmse_pct   = (rmse/mean(GPP_obs_y)) * 100,
    nrmse   = rmse / mean(GPP_obs_y),      # normalized by obs mean
    nse     = if (n() >= 2) NSE(GPP_obs_y, GPP_mod_y) else NA_real_,
    kge     = if (n() >= 2) KGE(GPP_obs_y, GPP_mod_y) else NA_real_,
    slope   = if (n() >= 2) coef(lm(GPP_mod_y ~ GPP_obs_y))[2] else NA_real_,
    .groups = "drop"
  ) %>%
  arrange(desc(n_years), SiteID)


metrics_ge4 <- metrics_all %>% filter(n_years >= 4)

write_csv(metrics_all, "IAV_metrics_cov80_all_sites.csv")
write_csv(metrics_ge4, "IAV_metrics_cov80_sites_ge4.csv")



## build facet-strip labels from your metrics ----
lab_stats_all <- metrics_all %>%
  mutate(
    strip_lab = sprintf("%s\nr=%.2f, RMSE=%.2f, CVr=%.2f",
                        SiteID, r, rmse, cv_ratio)
  ) %>%
  select(SiteID, strip_lab)

strip_map_all <- setNames(lab_stats_all$strip_lab, lab_stats_all$SiteID)

# sites with ≥4 years (compute BEFORE strip_map_ge4)
sites_ge4 <- annual_all %>%
  count(SiteID, name="n_years") %>%
  filter(n_years >= 4) %>%
  pull(SiteID)

strip_map_ge4 <- lab_stats_all %>%
  filter(SiteID %in% sites_ge4) %>%
  { setNames(.$strip_lab, .$SiteID) }





# Plots ----
ann_long <- annual_all %>%
  pivot_longer(c(GPP_obs_y, GPP_mod_y), names_to = "series", values_to = "GPP_y") %>%
  mutate(series = recode(series, GPP_obs_y = "Observed", GPP_mod_y = "JULES"),
         year   = as.integer(year))

# (1) All sites (after 80% filter)
p_all <- ggplot(ann_long, aes(year, GPP_y, color = series, group = series)) +
  geom_line(na.rm = TRUE) +
  geom_point(size = 2, na.rm = TRUE) +
  facet_wrap(~ SiteID, scales = "free", ncol = 3,
             labeller = labeller(SiteID = strip_map_all)) +
  scale_x_continuous(breaks = function(lims) seq(floor(lims[1]), ceiling(lims[2]), 1), minor_breaks = NULL) +
  labs(#title = "Annual GPP (calendar years with ≥80% joint coverage)",
       #subtitle = "Obs µmol→kgC (×1.2e−8); daily totals; JULES masked via inner-join on dates",
       x = "Year", y = "Annual GPP (kg C m⁻² yr⁻¹)", color = NULL) +
  theme_minimal(base_size = 11) + theme(legend.position = "top",
                                        legend.text = element_text(size = 16, face = "bold"),  # ← make legend labels bigger
                                        legend.title = element_blank(),
                                        strip.text = element_text(size = 14, face = "bold"),
                                        axis.title = element_text(size = 16, face = "bold"),
                                        axis.text = element_text(size = 10, face = "bold"))

# (2) Only sites with >= 4 kept years
sites_ge4 <- annual_all %>% count(SiteID, name="n_years") %>% filter(n_years >= 4) %>% pull(SiteID)
p_ge4 <- ann_long %>% filter(SiteID %in% sites_ge4) %>%
  ggplot(aes(year, GPP_y, color = series, group = series)) +
  geom_line(na.rm = TRUE) +
  geom_point(size = 2, na.rm = TRUE) +
  facet_wrap(~ SiteID, scales = "free", ncol = 3,
             labeller = labeller(SiteID = strip_map_ge4)) +
  scale_x_continuous(breaks = function(lims) seq(floor(lims[1]), ceiling(lims[2]), 1), minor_breaks = NULL) +
  labs(#title = "Annual GPP (sites with ≥4 calendar years, ≥80% coverage each)",
       x = "Year", y = "Annual GPP (kg C m⁻² yr⁻¹)", color = NULL) +
  theme_minimal(base_size = 11) + theme(legend.position = "top",
                                        legend.text = element_text(size = 16, face = "bold"),  # ← make legend labels bigger
                                        legend.title = element_blank(),
                                        strip.text = element_text(size = 14, face = "bold"),
                                        axis.title = element_text(size = 16, face = "bold"),
                                        axis.text = element_text(size = 10, face = "bold"))


print(p_all); print(p_ge4)

ggsave("AnnualGPP_AllSites_cov80.png", p_all, width=16, height=10, dpi=300, bg="white")
ggsave("AnnualGPP_Sites_GE4_cov80.png", p_ge4, width=16, height=10, dpi=300, bg="white")




# more plots ----

# use overlapping (should already be so)
ann_overlap <- annual_all %>% filter(!is.na(GPP_obs_y), !is.na(GPP_mod_y))


## summary (CV ratio) + mean bias (lollipop) ----
metrics_cv_bias <- ann_overlap %>%
  group_by(SiteID) %>%
  summarise(
    n_years = n(),
    cv_obs  = sd(GPP_obs_y)/mean(GPP_obs_y),
    cv_mod  = sd(GPP_mod_y)/mean(GPP_mod_y),
    cv_ratio = cv_mod / cv_obs,
    bias     = mean(GPP_mod_y - GPP_obs_y),
    .groups = "drop"
  )


metrics_cv_bias <- metrics_cv_bias %>% semi_join(metrics_all %>% filter(n_years>=4), by="SiteID")


# (A) CV ratio lollipop (1 = perfect)
p_cv <- metrics_cv_bias %>%
  ggplot(aes(x = cv_ratio, y = reorder(SiteID, cv_ratio))) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey60") +
  geom_segment(aes(x = 1, xend = cv_ratio, yend = SiteID), color = "grey70") +
  geom_point(size = 3) +
  labs(title = "Interannual variability amplitude",
       subtitle = "CV ratio = CV(JULES)/CV(Obs); 1 = perfect",
       x = "CV ratio", y = "Site") +
  theme_minimal(base_size = 11)

# (B) Mean bias lollipop (JULES − Obs)
p_bias <- metrics_cv_bias %>%
  ggplot(aes(x = bias, y = reorder(SiteID, bias))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_segment(aes(x = 0, xend = bias, yend = SiteID), color = "grey70") +
  geom_point(size = 3) +
  labs(title = "Mean annual bias",
       subtitle = "Positive = JULES higher than Obs",
       x = "Bias (kg C m⁻² yr⁻¹)", y = "Site") +
  theme_minimal(base_size = 11)



print(p_cv); print(p_bias)

ggsave("CV_ratio_lollipop.png", p_cv, width=16, height=10, dpi=300, bg="white")
ggsave("mean_bias_lollipop.png", p_bias, width=16, height=10, dpi=300, bg="white")



## Taylor's diagram ----

ann_ge4 <- annual_all %>%
  filter(is.finite(GPP_obs_y), is.finite(GPP_mod_y)) %>%
  group_by(SiteID) %>%
  mutate(n_years = n()) %>%
  ungroup() %>%
  filter(n_years >= 4)

# keep only sites with variability (sd > 0) and at least 2 points (true for n_years>=4, but we check)
site_ok <- ann_ge4 %>%
  group_by(SiteID) %>%
  summarise(
    sd_obs = sd(GPP_obs_y), sd_mod = sd(GPP_mod_y),
    n      = n()
  ) %>%
  filter(n >= 2, is.finite(sd_obs), is.finite(sd_mod), sd_obs > 0, sd_mod > 0) %>%
  pull(SiteID)

td_dat_ok <- ann_ge4 %>%
  filter(SiteID %in% site_ok) %>%
  transmute(SiteID, obs = GPP_obs_y, mod = GPP_mod_y)




td <- openair::TaylorDiagram(
  td_dat_ok,
  obs = "obs", mod = "mod", group = "SiteID",
  normalise = TRUE, col = "brewer1", key = TRUE,
  main = "Taylor diagram: annual GPP (sites ≥4 yrs; calendar years; ≥80% coverage)"
)



png("Taylor_annual_GPP_cov80_sites_ge4.png", width = 2400, height = 1800, res = 300)
print(td)      # lattice plots must be printed to the device
dev.off()


pdf("Taylor_annual_GPP_cov80_sites_ge4.pdf", width = 8, height = 6)
print(td)
dev.off()


tiff("Taylor_annual_GPP_cov80_sites_ge4.tiff", width = 2400, height = 1800, res = 300, compression = "lzw")
print(td)
dev.off()




td_abs <- openair::TaylorDiagram(
  td_dat_ok,
  obs = "obs", mod = "mod", group = "SiteID",
  normalise = FALSE,                 # <-- absolute SD (units of kg C m^-2 yr^-1)
  col = "brewer1", key = TRUE
  #main = "Taylor diagram: annual GPP (absolute SD; sites ≥4 yrs; ≥80% coverage)"
)


png("Taylor_annual_GPP_absSD_openair.png", width = 2400, height = 1800, res = 300)
print(td_abs)   # lattice object -> must print to device
dev.off()



# sanity check:
taylor_numbers <- td_dat_ok %>%
  group_by(SiteID) %>%
  summarise(
    n_years = n(),
    r       = cor(obs, mod),
    sd_obs  = sd(obs),
    sd_mod  = sd(mod),
    # centered RMSD (what the grey/orange arcs represent)
    crmsd   = sqrt(mean(((mod - mean(mod)) - (obs - mean(obs)))^2)),
    .groups = "drop"
  ) %>%
  arrange(SiteID)

print(taylor_numbers, n = Inf)

# sanity check: set equality with your ≥4-year set
sites_ge4 <- annual_all %>% count(SiteID) %>% filter(n >= 4) %>% pull(SiteID)
setdiff(sites_ge4, unique(td_dat_ok$SiteID))  # should be character(0)


####Sites with r < 0 are not displayed (NE_WAM, ZA_CATH2016). See plotrix panel for full r range
write_csv(taylor_numbers, "taylor_numbers.csv")

