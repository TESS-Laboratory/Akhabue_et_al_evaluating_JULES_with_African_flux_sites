# pipeline ----
# Annual GPP by HYDROLOGICAL YEAR (end-year convention)
# Uses hydro_year_starts.csv (start_md per site) from earlier step
# Keeps Obs/JULES daily pipeline and coverage rule, but per HY


# libraries ----
library(tidyverse)
library(dplyr)
library(lubridate)
library(ncdf4)
library(purrr)
library(stringr)
library(openair)
library(ggplot2)
library(RColorBrewer)
library(plotrix)
library(scales)
library(tidyr)
library(patchwork)



# config ----
csv_folder <- "C:/Users/efa206/OneDrive - University of Exeter/Desktop/combined_data_with_ET"
ncd_folder <- "C:/Users/efa206/OneDrive - University of Exeter/Desktop/JULES_Output/File_1000"
hydro_dir  <- "C:/Users/efa206/OneDrive - University of Exeter/Desktop/hydrological_year_data"
COVERAGE_THRESH <- 1.00

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
  "ZA_CATH2016",     "ZA_CATH2016_with_ET.csv",   "part6_ZA_CATH-JULES_vn7.4.D.nc",
  "ZA_KRU",          "ZA_KRU_with_ET.csv",        "part6_ZA_KRU-JULES_vn7.4.D.nc",
  "ZA_WGN",          "ZA_WGN_with_ET.csv",        "part6_ZA_WGN-JULES_vn7.4.D.nc",
  "ZM_MON",          "ZM_MON_with_ET.csv",        "part6_ZM_MON-JULES_vn7.4.D.nc"
)





# bring in HY start month/day per site ----
hydro_starts <- readr::read_csv(file.path(hydro_dir, "hydro_year_starts.csv"), show_col_types = FALSE) %>%
  select(site, start_md) %>%
  # parse "Oct-12" -> month/day
  mutate(start_date_ref = as.Date(paste0("2001-", start_md), format = "%Y-%b-%d"),
         HY_start_month = month(start_date_ref),
         HY_start_day   = mday(start_date_ref)) %>%
  select(site, HY_start_month, HY_start_day)

# sanity join: make sure we have a start for every site
site_map <- site_map %>%
  left_join(hydro_starts, by = c("site_label" = "site"))
if (any(is.na(site_map$HY_start_month))) {
  stop("Missing hydrological-year start for: ",
       paste(site_map$site_label[is.na(site_map$HY_start_month)], collapse = ", "),
       "\nCheck hydro_year_starts.csv")
}


site_map <- site_map %>%
  mutate(site_label = ifelse(site_label == "ZA_CATH2016", "ZA_CATH", site_label))

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



# assign hydrological year (end-year) from month/day start ----
label_hy <- function(date, m_start, d_start){
  y <- year(date)
  add1 <- (month(date) > m_start) | (month(date) == m_start & mday(date) >= d_start)
  y + ifelse(add1, 1L, 0L)  # end-year convention
}

# expected #days for a given HY (handles leap years naturally)
expected_days_hy <- function(hy, m_start, d_start){
  start <- as.Date(sprintf("%04d-%02d-%02d", hy-1, m_start, d_start))
  end   <- as.Date(sprintf("%04d-%02d-%02d", hy,   m_start, d_start)) - days(1)
  as.integer(end - start + 1)
}



# daily join (Obs + JULES) ----
daily_joined <- function(csv_path, nc_path) {
  # JULES daily totals (kg C m^-2 day^-1)
  jules_daily <- ncd_to_df(nc_path) %>%
    transmute(
      date   = as_date(time_bounds_2),
      dt_sec = as.numeric(difftime(time_bounds_2, time_bounds_1, units = "secs")),
      gpp_gb = gpp_gb,
      GPP_mod_day = gpp_gb * dt_sec
    ) %>%
    group_by(date) %>%
    summarise(GPP_mod_day = sum(GPP_mod_day, na.rm = TRUE), .groups = "drop")
  
  # Obs: µmol CO2 m^-2 s^-1 -> kg C m^-2 s^-1 (×1.2e-8); daily mean * 86400
  obs_daily <- readr::read_csv(csv_path, show_col_types = FALSE) %>%
    transmute(
      TIMESTAMP = ymd_hms(TIMESTAMP),
      date      = as_date(TIMESTAMP),
      GPP_kgCs  = replace_na(GPP, 0) * 1.2e-8
    ) %>%
    group_by(date) %>%
    summarise(GPP_obs_day = 86400 * mean(GPP_kgCs, na.rm = TRUE), .groups = "drop")
  
  inner_join(obs_daily, jules_daily, by = "date")
}




# HY aggregation per site ----
annual_hy_for_site <- function(site_label, csv_path, nc_path, m_start, d_start, coverage_thresh = COVERAGE_THRESH){
  dj <- daily_joined(csv_path, nc_path) %>%
    mutate(hydro_year = label_hy(date, m_start, d_start))
  
  # coverage per HY based on joint days and true expected days in that HY
  keep_hy <- dj %>%
    count(hydro_year, name = "joint_days") %>%
    mutate(exp_days = vapply(hydro_year, expected_days_hy, integer(1), m_start, d_start),
           coverage = joint_days / exp_days) %>%
    filter(coverage >= coverage_thresh)
  
  dj %>%
    semi_join(keep_hy, by = "hydro_year") %>%
    group_by(hydro_year) %>%
    summarise(
      GPP_obs_y = sum(GPP_obs_day, na.rm = TRUE),
      GPP_mod_y = sum(GPP_mod_day, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(SiteID = site_label,
           coverage = keep_hy$coverage[match(hydro_year, keep_hy$hydro_year)]) %>%
    relocate(SiteID, hydro_year, coverage)
}



# run all sites (HY) ----
annual_all_hy <- purrr::map_dfr(seq_len(nrow(site_map)), function(i){
  message("Processing (HY) ", site_map$site_label[i], " ...")
  annual_hy_for_site(
    site_label = site_map$site_label[i],
    csv_path   = file.path(csv_folder, site_map$csv_file[i]),
    nc_path    = file.path(ncd_folder, site_map$ncd_file[i]),
    m_start    = site_map$HY_start_month[i],
    d_start    = site_map$HY_start_day[i]
  )
})





# Metrics 'hydro_year' instead of 'year' ----
NSE <- function(o, m){ 1 - sum((m - o)^2) / sum((o - mean(o))^2) }
KGE <- function(o, m){
  r  <- cor(o, m)
  alpha <- sd(m)/sd(o)
  beta  <- mean(m)/mean(o)
  1 - sqrt((r - 1)^2 + (alpha - 1)^2 + (beta - 1)^2)
}

metrics_all_hy <- annual_all_hy %>%
  filter(!is.na(GPP_obs_y) & !is.na(GPP_mod_y)) %>%
  group_by(SiteID) %>%
  summarise(
    n_years = n(),
    r       = if (n() >= 2) cor(GPP_obs_y, GPP_mod_y) else NA_real_,
    rho     = if (n() >= 2) cor(GPP_obs_y, GPP_mod_y, method = "spearman") else NA_real_,
    rmse    = sqrt(mean((GPP_mod_y - GPP_obs_y)^2)),
    mae     = mean(abs(GPP_mod_y - GPP_obs_y)),
    bias    = mean(GPP_mod_y - GPP_obs_y),
    cv_obs  = sd(GPP_obs_y)/mean(GPP_obs_y),
    cv_mod  = sd(GPP_mod_y)/mean(GPP_mod_y),
    cv_obs_per  = sd(GPP_obs_y)/mean(GPP_obs_y) * 100,
    cv_mod_per  = sd(GPP_mod_y)/mean(GPP_mod_y) * 100,
    cv_ratio= cv_mod / cv_obs,
    nrmse_pct   = (rmse/mean(GPP_obs_y)) * 100,
    nrmse   = rmse / mean(GPP_obs_y),
    nse     = if (n() >= 2) NSE(GPP_obs_y, GPP_mod_y) else NA_real_,
    kge     = if (n() >= 2) KGE(GPP_obs_y, GPP_mod_y) else NA_real_,
    slope   = if (n() >= 2) coef(lm(GPP_mod_y ~ GPP_obs_y))[2] else NA_real_,
    .groups = "drop"
  ) %>%
  arrange(desc(n_years), SiteID)

metrics_ge4_hy <- metrics_all_hy %>% filter(n_years >= 5)

readr::write_csv(metrics_all_hy, "A_IAV_metrics_cov100_all_sites_HY.csv")
readr::write_csv(metrics_ge4_hy, "A_IAV_metrics_cov100_sites_ge4_HY.csv")





# Plots ----
ann_long_hy <- annual_all_hy %>%
  pivot_longer(c(GPP_obs_y, GPP_mod_y), names_to = "series", values_to = "GPP_y") %>%
  mutate(series = recode(series, GPP_obs_y = "Observed", GPP_mod_y = "JULES"))

lab_stats_all_hy <- metrics_all_hy %>%
  mutate(strip_lab = sprintf("%s\nRMSE=%.2f", SiteID, rmse)) %>%
  select(SiteID, strip_lab)
strip_map_all_hy <- setNames(lab_stats_all_hy$strip_lab, lab_stats_all_hy$SiteID)

sites_ge4_hy <- annual_all_hy %>% count(SiteID, name="n_years") %>% filter(n_years >= 5) %>% pull(SiteID)
strip_map_ge4_hy <- lab_stats_all_hy %>% filter(SiteID %in% sites_ge4_hy) %>% { setNames(.$strip_lab, .$SiteID) }



# (1) All sites
p_all_hy <- ggplot(ann_long_hy, aes(hydro_year, GPP_y, color = series, group = series)) +
  geom_line(linewidth = 1, na.rm = TRUE) +
  geom_point(size = 3, na.rm = TRUE) +
  facet_wrap(~ SiteID, scales = "free", ncol = 3,
             labeller = labeller(SiteID = strip_map_all_hy)) +
  scale_x_continuous(breaks = function(lims) seq(floor(lims[1]), ceiling(lims[2]), 1), minor_breaks = NULL) +
  scale_y_continuous(limits = c(0, NA)) +   # << Force y-axis to start at 0
  scale_color_manual(values = c("JULES" = "#00BFA3", "Observed" = "#F8764D")) +   # << here
  labs(x = "Year", y = "GPP (kg C m⁻² yr⁻¹)", color = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top",
        legend.text = element_text(size = 18, face = "bold"),
        legend.title = element_blank(),
        legend.key.size = unit(3, "cm"),        # increase size of legend keys
        legend.key.width = unit(3, "cm"),       # widen the legend line
        strip.text = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 18, face = "bold"))



# (2) Sites with ≥5 HYs kept
p_ge5_hy_2 <- ann_long_hy %>% filter(SiteID %in% sites_ge4_hy) %>%
  ggplot(aes(hydro_year, GPP_y, color = series, group = series)) +
  geom_line(linewidth = 1, na.rm = TRUE) +
  geom_point(size = 3, na.rm = TRUE) +
  facet_wrap(~ SiteID, scales = "free", ncol = 2,
             labeller = labeller(SiteID = strip_map_ge4_hy)) +
  scale_x_continuous(breaks = function(lims) seq(floor(lims[1]), ceiling(lims[2]), 1), minor_breaks = NULL) +
  scale_y_continuous(limits = c(0, NA)) +   # << Force y-axis to start at 0
  scale_color_manual(values = c("JULES" = "#00BFA3", "Observed" = "#F8764D")) +   # << here
  labs(x = "Year", y = "GPP (kg C m⁻² yr⁻¹)", color = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top",
        legend.text = element_text(size = 18, face = "bold"),
        legend.title = element_blank(),
        legend.key.size = unit(3, "cm"),        # increase size of legend keys
        legend.key.width = unit(3, "cm"),       # widen the legend line
        strip.text = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 17, face = "bold"),
        axis.text = element_text(size = 17, face = "bold"))


print(p_all_hy); print(p_ge5_hy_2)

ggsave("AnnualGPP_AllSites_cov100_HY.png", p_all_hy, width=16, height=10, dpi=300, bg="white")
ggsave("AnnualGPP_Sites_GE4_cov100_HY.png", p_ge5_hy_2, width=16, height=10, dpi=300, bg="white")




## CV ratio & bias (HY) ----
ann_overlap_hy <- annual_all_hy %>% filter(!is.na(GPP_obs_y), !is.na(GPP_mod_y))

metrics_cv_bias_hy <- ann_overlap_hy %>%
  group_by(SiteID) %>%
  summarise(
    n_years = n(),
    cv_obs  = sd(GPP_obs_y)/mean(GPP_obs_y),
    cv_mod  = sd(GPP_mod_y)/mean(GPP_mod_y),
    cv_ratio = cv_mod / cv_obs,
    cv_obs_per  = sd(GPP_obs_y)/mean(GPP_obs_y) * 100,
    cv_mod_per  = sd(GPP_mod_y)/mean(GPP_mod_y) * 100,
    bias     = mean(GPP_mod_y - GPP_obs_y),
    .groups = "drop"
  ) %>%
  semi_join(metrics_all_hy %>% filter(n_years>=5), by="SiteID")

p_cv_hy <- metrics_cv_bias_hy %>%
  ggplot(aes(x = cv_ratio, y = reorder(SiteID, cv_ratio))) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey60") +
  geom_segment(aes(x = 1, xend = cv_ratio, yend = SiteID), color = "grey70") +
  geom_point(size = 3) +
  labs(#title = "Interannual variability amplitude (Hydrological years)",
       subtitle = "CV ratio = CV(JULES) / CV(Obs); 1 = perfect",
       x = "CV ratio", y = "Site") +
  theme_minimal(base_size = 11)

p_bias_hy <- metrics_cv_bias_hy %>%
  ggplot(aes(x = bias, y = reorder(SiteID, bias))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_segment(aes(x = 0, xend = bias, yend = SiteID), color = "grey70") +
  geom_point(aes(size = n_years), color = "black") +   # scale dot size by years
  scale_size_continuous(range = c(5, 14), breaks = c(6, 9, 10, 12, 13),
                        name = "Years of data") +
  labs(#subtitle = "Positive = JULES higher than Obs",
       x = "Bias (kg C m⁻² yr⁻¹)", y = "Site", size = "Years of data") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.text = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 16, face = "bold"),
        strip.text = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 17, face = "bold"))



p_cv_obs_per_hy <- metrics_cv_bias_hy %>%
  ggplot(aes(x = cv_obs_per, y = reorder(SiteID, cv_obs_per))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_segment(aes(x = 0, xend = cv_obs_per, yend = SiteID), color = "grey70") +
  geom_point(size = 3) +
  labs(#title = "Mean annual cv_obs_per (Hydrological years)",
       x = "cv_obs_per (kg C m⁻² yr⁻¹)", y = "Site") +
  theme_minimal(base_size = 11)

p_cv_mod_per_hy <- metrics_cv_bias_hy %>%
  ggplot(aes(x = cv_mod_per, y = reorder(SiteID, cv_mod_per))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_segment(aes(x = 0, xend = cv_mod_per, yend = SiteID), color = "grey70") +
  geom_point(size = 3) +
  labs(#title = "Mean annual cv_mod_per (Hydrological years)",
       x = "cv_mod_per (kg C m⁻² yr⁻¹)", y = "Site") +
  theme_minimal(base_size = 11)


print(p_cv_hy); print(p_bias_hy); print(p_cv_mod_per_hy); print(p_cv_obs_per_hy)


ggsave("CV_ratio_lollipop_HY.png", p_cv_hy, width=16, height=10, dpi=300, bg="white")
ggsave("mean_bias_lollipop_HY.png", p_bias_hy, width=16, height=10, dpi=300, bg="white")
ggsave("mean_cv_mod_per_lollipop_HY.png", p_cv_mod_per_hy, width=16, height=10, dpi=300, bg="white")
ggsave("mean_cv_obs_per_lollipop_HY.png", p_cv_obs_per_hy, width=16, height=10, dpi=300, bg="white")




cv_long <- metrics_ge4_hy %>%
  select(SiteID, cv_obs, cv_mod) %>%         # <- raw CV (0–1), not *_per
  tidyr::pivot_longer(c(cv_obs, cv_mod),
                      names_to = "series", values_to = "cv") %>%
  dplyr::mutate(series = dplyr::recode(series,
                                       cv_obs = "OBS", cv_mod = "JULES"))

# (optional) order sites by OBS CV
cv_long <- cv_long %>%
  group_by(SiteID) %>% mutate(cv_obs_only = cv[series == "OBS"][1]) %>% ungroup() %>%
  mutate(SiteID = factor(SiteID, levels = unique(SiteID[order(-cv_obs_only)])))

p_cv_bars <- ggplot(cv_long, aes(x = SiteID, y = cv, fill = series)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  scale_y_continuous(limits = c(0, NA), breaks = seq(0, 0.6, 0.1)) +  # adjust max as needed
  scale_fill_manual(values = c("JULES" = "#00BFA3", "OBS" = "#F8764D")) +  # swapped
  labs(x = NULL, y = "CV of annual GPP", fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, face = "bold", size = 16),  # x-axis labels
    axis.text.y = element_text(face = "bold", size = 18),                                   # y-axis numbers
    axis.title.y = element_text(face = "bold", size = 18),                                  # y-axis title
    plot.title = element_text(face = "bold", size = 18)
  )

p_cv_bars


ggsave("CV_barplot.png", p_cv_bars, width=13, height=10, dpi=300, bg="white")


#### Combine: big faceted plot on top ----
final_fig <- p_ge5_hy_2 / plot_spacer() / (p_bias_hy | p_cv_bars) +
  plot_layout(heights = c(3, 0.2, 1))  # 0.2 = empty space

final_fig


# Save
ggsave("Figure_fullpage_combined.png", final_fig, width = 19, height = 18, dpi = 300)




## Taylor diagram (HY) ----
ann_ge4_hy <- annual_all_hy %>%
  filter(is.finite(GPP_obs_y), is.finite(GPP_mod_y)) %>%
  group_by(SiteID) %>% mutate(n_years = n()) %>% ungroup() %>%
  filter(n_years >= 5)

site_ok_hy <- ann_ge4_hy %>%
  group_by(SiteID) %>%
  summarise(sd_obs = sd(GPP_obs_y), sd_mod = sd(GPP_mod_y), n = n()) %>%
  filter(n >= 2, is.finite(sd_obs), is.finite(sd_mod), sd_obs > 0, sd_mod > 0) %>%
  pull(SiteID)

td_dat_ok_hy <- ann_ge4_hy %>%
  filter(SiteID %in% site_ok_hy) %>%
  transmute(SiteID, obs = GPP_obs_y, mod = GPP_mod_y)

td_hy <- openair::TaylorDiagram(
  td_dat_ok_hy,
  obs = "obs", mod = "mod", group = "SiteID",
  normalise = TRUE, col = "brewer1", key = TRUE,
  main = "Taylor diagram: annual GPP (hydrological years; sites ≥4 yrs; ≥50% coverage)"
)

png("Taylor_annual_GPP_HY.png", width = 2400, height = 1800, res = 300)
print(td_hy)
dev.off()

# absolute SD version
td_abs_hy <- openair::TaylorDiagram(
  td_dat_ok_hy,
  obs = "obs", mod = "mod", group = "SiteID",
  normalise = FALSE, col = "brewer1", key = TRUE
)
png("Taylor_annual_GPP_absSD_HY.png", width = 2400, height = 1800, res = 300)
print(td_abs_hy)
dev.off()

# sanity table
taylor_numbers_hy <- td_dat_ok_hy %>%
  group_by(SiteID) %>%
  summarise(
    n_years = n(),
    r       = cor(obs, mod),
    sd_obs  = sd(obs),
    sd_mod  = sd(mod),
    crmsd   = sqrt(mean(((mod - mean(mod)) - (obs - mean(obs)))^2)),
    .groups = "drop"
  )
readr::write_csv(taylor_numbers_hy, "taylor_numbers_HY.csv")







# keep sites with ≥4 HY and finite variability
ann_ge4_hy <- annual_all_hy %>%
  filter(is.finite(GPP_obs_y), is.finite(GPP_mod_y)) %>%
  group_by(SiteID) %>% mutate(n_years = n()) %>% ungroup() %>%
  filter(n_years >= 5)


site_ok <- ann_ge4_hy %>%
  group_by(SiteID) %>%
  summarise(r = cor(GPP_obs_y, GPP_mod_y),
            sd_obs = sd(GPP_obs_y),
            sd_mod = sd(GPP_mod_y),
            .groups = "drop") %>%
  filter(is.finite(r), sd_obs > 0, sd_mod > 0) %>%
  pull(SiteID)

# color palette long enough for all sites
base_cols <- brewer.pal(8, "Set2")
cols <- colorRampPalette(base_cols)(length(site_ok))

# SD arcs to cover range
sd_max <- ann_ge4_hy %>% filter(SiteID %in% site_ok) %>%
  summarise(max_sd = max(c(sd(GPP_obs_y), sd(GPP_mod_y)))) %>% pull(max_sd)
sd_arcs <- pretty(c(0, sd_max))

png("Taylor_plotrix_HY_absSD.png", width = 2500, height = 2000, res = 300)
par(mar = c(4,4,1,1),
    cex = 0.7)

# first site (creates the diagram)
g <- ann_ge4_hy %>% filter(SiteID == site_ok[1])
taylor.diagram(g$GPP_obs_y, g$GPP_mod_y,
               normalize = FALSE, pos.cor = FALSE,
               sd.arcs = sd_arcs,
               pch = 21, cex = 1.8, lwd = 1.0,
               col = cols[1],
               ngamma = 0,          # draw zero γ contours
               gamma.col = NA)   # <-- hide CRMSD rings/labels

               #main = "Taylor diagram — Hydrological years (absolute SD)")

# add remaining sites (no text labels)
if (length(site_ok) > 1) {
  for (i in 2:length(site_ok)) {
    g <- ann_ge4_hy %>% filter(SiteID == site_ok[i])
    taylor.diagram(g$GPP_obs_y, g$GPP_mod_y,
                   normalize = FALSE, pos.cor = FALSE,
                   sd.arcs = sd_arcs,
                   pch = 17, cex = 1.8, col = cols[i], add = TRUE,
                   ngamma = 0,          # draw zero γ contours
                   gamma.col = NA)   # <-- here too
  }
}

legend("topright", legend = site_ok, pch = 17, pt.cex = 1.3, col = cols, bty = "n", cex = 0.8)
dev.off()





# use this taylor's diagram 


# Open a PNG device
png("TaylorDiagram_HY.png", width = 2400, height = 1800, res = 300)

sites <- unique(td_dat_ok_hy$SiteID)
cols  <- grDevices::rainbow(length(sites))  # or your palette

par(mar = c(4,4,1,6))  # add space for legend
first <- TRUE
for (i in seq_along(sites)) {
  df <- td_dat_ok_hy %>% dplyr::filter(SiteID == sites[i])
  plotrix::taylor.diagram(df$obs, df$mod,
                          add       = !first,
                          pos.cor   = FALSE,
                          normalize = TRUE,
                          pch = 16, col = cols[i])
  first <- FALSE
}
legend("topleft", legend = sites, col = cols, pch = 18, cex = 0.8, bty = "n")

dev.off()  # close the device and save






# Safe-keeping ----
site_map <- site_map %>%
mutate(site_label = dplyr::recode(site_label,
"BW_GUM"      = "BW_GUM - Wetland",
"BW_NXR"      = "BW_NXR - Wetland",
"CG_TCH"      = "CG_TCH - Grassland",
"GH_ANK"      = "GH_ANK - Forest",
"ML_AGG"      = "ML_AGG - Grassland",
"NE_WAF"      = "NE_WAF - Savanna",
"NE_WAM"      = "NE_WAM - Savanna",
"SD_DEM"      = "SD_DEM - Grassland",
"SN_DHR"      = "SN_DHR - Grassland",
"SN_NKR"      = "SN_NKR - Cropland",
"SN_RAG"      = "SN_RAG - Cropland",
"UG_JIN"      = "UG_JIN - Wetland",
"ZA_CATH"     = "ZA_CATH - Grassland",
"ZA_KRU"      = "ZA_KRU - Savanna",
"ZA_WGN"      = "ZA_WGN - Grassland",
"ZM_MON"      = "ZM_MON - Forest"
))

