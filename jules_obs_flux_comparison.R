# ----------- Load Required Libraries ---------------
library(ncdf4)
library(tidyverse)
library(lubridate)
library(gridExtra)
library(patchwork)

# ----------- Functions ---------------
ncd_start <- function(ncd){
  if (inherits(ncd, "character")){
    ncd <- nc_open(ncd)
    on.exit(nc_close(ncd))
  }
  time_units <- ncatt_get(ncd, "time_bounds")$units
  gsub("[^0-9:-]", " ", time_units) |> trimws() |> ymd_hms()
}

ncd_to_df <- function(ncd, label=NULL) {
  ncd <- nc_open(ncd)
  on.exit(nc_close(ncd))
  start_date <- ncd_start(ncd)
  
  multi_dim_to_df <- function(x, .name){
    x <- as_tibble(t(x), .name_repair="unique")
    rename_with(x, ~paste(.name, str_replace(colnames(x), "...", ""), sep="_"))
  }
  
  vars_to_df <- function(var){
    vals <- ncvar_get(ncd, var)
    if (length(dim(vals)) > 1) {
      suppressMessages(multi_dim_to_df(vals, var))
    } else {
      tibble(!!var := (vals))
    }
  }
  
  var_names <- setdiff(names(ncd$var), c("latitude", "longitude"))
  ncd_df <- map(var_names, vars_to_df) |> reduce(bind_cols) |> 
    mutate(across(c("time_bounds_1", "time_bounds_2"), ~start_date + .x))
  
  if (!is.null(label)) ncd_df <- mutate(ncd_df, label = label)
  attr(ncd_df, "lat") <- ncvar_get(ncd,"latitude")
  attr(ncd_df, "long") <- ncvar_get(ncd,"longitude")
  return(ncd_df)
}

stat_tab_multi <- function(df, .label, var, display_name) {
  df %>%
    filter(label %in% c(.label, "Observed")) %>%
    select(label, value = !!sym(var), time_bounds_2) %>%
    pivot_wider(names_from = label, values_from = value) %>%
    mutate(
      bias = (!!sym(.label) - Observed),
      sqr_err = (!!sym(.label) - Observed)^2
    ) %>%
    summarise(
      mse = mean(sqr_err, na.rm = TRUE),
      rmse = sqrt(mse),
      bias = mean(bias, na.rm = TRUE),
      cor = cor(!!sym(.label), Observed, use = "complete.obs"),
      obs_mean = mean(Observed, na.rm = TRUE),
      scaled_bias = bias / obs_mean,
      scaled_rmse = rmse / obs_mean
    ) %>%
    mutate(label = .label, variable = display_name)
}

stat_tab_multi_yearly <- function(df, .label, var, display_name) {
  df %>%
    filter(label %in% c(.label, "Observed")) %>%
    select(label, value = !!sym(var), time_bounds_2) %>%
    mutate(Year = year(time_bounds_2)) %>%
    pivot_wider(names_from = label, values_from = value) %>%
    mutate(
      bias = (!!sym(.label) - Observed),
      sqr_err = (!!sym(.label) - Observed)^2
    ) %>%
    group_by(Year) %>%
    summarise(
      mse = mean(sqr_err, na.rm = TRUE),
      rmse = sqrt(mse),
      bias = mean(bias, na.rm = TRUE),
      cor = cor(!!sym(.label), Observed, use = "complete.obs"),
      obs_mean = mean(Observed, na.rm = TRUE),
      scaled_bias = bias / obs_mean,
      scaled_rmse = rmse / obs_mean,
      .groups = "drop"
    ) %>%
    mutate(label = .label, variable = display_name) %>%
    relocate(label, variable, Year)
}

# ----------- Main Function ---------------
process_flux_site <- function(site_label, ncd_path, csv_path,
                              plot_dir = "site_plots", stats_dir = "stats", write_stats = TRUE) {
  
  dir.create(plot_dir, showWarnings = FALSE)
  dir.create(stats_dir, showWarnings = FALSE)
  
  def_df <- ncd_to_df(ncd_path, label = site_label)
  def_df <- def_df %>%
    mutate(
      Reco_gb = resp_p_gb + resp_s_gb,
      T_C = min_t1p5m_gb - 273.15,
      lambda_J_per_kg = (2.501 - 0.00237 * T_C) * 1e6,
      ET_kg_m2_s = latent_heat / lambda_J_per_kg
    )
  
  jules_df <- def_df %>%
    select(time_bounds_2, gpp_gb, Reco_gb, ftl_gb, latent_heat, ET_kg_m2_s, fao_et0) %>%
    mutate(label = site_label)
  
  obs_df <- read_csv(csv_path, show_col_types = FALSE) %>%
    mutate(
      GPP_kg_m2_s = replace_na(GPP * 1.201e-8, 0),
      Reco_kg_m2_s = Reco * 1.201e-8,
      NEE_kg_m2_s = NEE * 1.201e-8,
      TIMESTAMP = ymd_hms(TIMESTAMP),
      date = as_date(TIMESTAMP)
    )
  
  jules_start <- as_date(min(jules_df$time_bounds_2, na.rm = TRUE))
  jules_end   <- as_date(max(jules_df$time_bounds_2, na.rm = TRUE))
  
  obs_daily <- obs_df %>%
    group_by(date) %>%
    summarise(
      GPP_daily_mean = mean(GPP_kg_m2_s, na.rm = TRUE),
      Reco_daily_mean = mean(Reco_kg_m2_s, na.rm = TRUE),
      H_daily_mean = mean(H, na.rm = TRUE),
      LE_daily_mean = mean(LE, na.rm = TRUE),
      ET_daily_mean = mean(ET_kg_m2_s, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(date >= jules_start & date <= jules_end)
  
  obs_main <- obs_daily %>%
    rename_with(~c("time_bounds_2", "gpp_gb", "Reco_gb", "ftl_gb", "latent_heat", "ET_kg_m2_s")) %>%
    mutate(
      time_bounds_2 = as.POSIXct(time_bounds_2),
      label = "Observed"
    )
  
  obs_et0 <- obs_daily %>%
    select(time_bounds_2 = date, fao_et0 = ET_daily_mean) %>%
    mutate(
      time_bounds_2 = as.POSIXct(time_bounds_2),
      label = "Observed"
    )
  
  combined_all <- bind_rows(jules_df, obs_main)
  vars_of_interest <- c("gpp_gb", "Reco_gb", "ftl_gb", "latent_heat", "ET_kg_m2_s")
  
  stat_results <- map_dfr(vars_of_interest, ~{
    display_name <- recode(.x, gpp_gb = "GPP", Reco_gb = "Reco", ftl_gb = "SH", latent_heat = "LE", ET_kg_m2_s = "ET")
    stat_tab_multi(combined_all, site_label, .x, display_name)
  })
  
  stat_results_yearly <- map_dfr(vars_of_interest, ~{
    display_name <- recode(.x, gpp_gb = "GPP", Reco_gb = "Reco", ftl_gb = "SH", latent_heat = "LE", ET_kg_m2_s = "ET")
    stat_tab_multi_yearly(combined_all, site_label, .x, display_name)
  })
  
  combined_et0 <- bind_rows(jules_df %>% select(time_bounds_2, fao_et0, label), obs_et0)
  stat_fao_et <- stat_tab_multi(combined_et0, site_label, "fao_et0", "FAO_ET")
  stat_fao_et_yearly <- stat_tab_multi_yearly(combined_et0, site_label, "fao_et0", "FAO_ET")
  
  combined_long <- combined_all %>%
    pivot_longer(all_of(vars_of_interest), names_to = "variable", values_to = "value") %>%
    mutate(variable = recode(variable, ET_kg_m2_s = "ET", gpp_gb = "GPP", Reco_gb = "Reco", ftl_gb = "SH", latent_heat = "LE"))
  
  et0_long <- combined_et0 %>% rename(value = fao_et0) %>% mutate(variable = "FAO_ET")
  plot_df <- bind_rows(combined_long, et0_long) %>% mutate(Source = ifelse(label == site_label, "JULES", "Observed"))
  
  stats_df <- bind_rows(stat_results, stat_fao_et)
  stats_yearly_df <- bind_rows(stat_results_yearly, stat_fao_et_yearly)
  
  plot <- ggplot(plot_df) +
    aes(x = time_bounds_2, y = value, colour = Source) +
    geom_line(linewidth = 0.5) +
    facet_wrap(~variable, scales = "free_y", ncol = 3) +
    scale_color_brewer(palette = "Set2") +
    theme_minimal() +
    labs(x = "Year", y = "Fluxes", colour = site_label) +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 18),
      strip.text = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12)
    )
  
  full_plot <- plot / gridExtra::tableGrob(
    stats_df %>% select(-label),
    theme = ttheme_minimal(
      core = list(fg_params = list(fontface = "plain", cex = 1.3)),
      colhead = list(fg_params = list(fontface = "plain", cex = 1.4))
    )
  ) + plot_layout(heights = c(23, 7))
  
  ggsave(filename = file.path(plot_dir, paste0(site_label, "_Fluxes.png")),
         plot = full_plot, width = 19, height = 10, dpi = 600, units = "in")
  
  if (write_stats) {
    write_csv(stats_df, file.path(stats_dir, paste0(site_label, "_stats.csv")))
    write_csv(stats_yearly_df, file.path(stats_dir, paste0(site_label, "_stats_yearly.csv")))
  }
  
  message(paste("Finished processing:", site_label))
}

# ----------- Loop Across All Sites ---------------

csv_folder <- "C:/Users/efa206/OneDrive - University of Exeter/Desktop/combined_data_with_ET"
ncd_folder <- "C:/Users/efa206/OneDrive - University of Exeter/Desktop/JULES_Output/File_1000"

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

for (i in 1:nrow(site_map)) {
  cat("\nProcessing:", site_map$site_label[i], "...\n")
  process_flux_site(
    site_label = site_map$site_label[i],
    ncd_path = file.path(ncd_folder, site_map$ncd_file[i]),
    csv_path = file.path(csv_folder, site_map$csv_file[i])
  )
}
