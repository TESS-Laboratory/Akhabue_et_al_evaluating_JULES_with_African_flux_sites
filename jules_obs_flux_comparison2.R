# plots for each sites same as the Jules_obs_flux_comparison script
# this time separated by variable not sites
# i.e., each plot contains one variable and all sites
# also, only cor, RMSE, and Bias are computed in this
# meaning the other script is still very important
# especially for the GLMM analysis
# since it also computes yearly stats and others


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
      rmse = sqrt(mean(sqr_err, na.rm = TRUE)),
      bias = mean(bias, na.rm = TRUE),
      cor = cor(!!sym(.label), Observed, use = "complete.obs")
    ) %>%
    mutate(label = .label, variable = display_name)
}

# ----------- Collect All Plot Data ---------------
all_plot_data <- list()
all_stat_data <- list()

# ----------- Main Function ---------------
process_flux_site <- function(site_label, ncd_path, csv_path) {
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
    mutate(label = site_label,
           gpp_gb  = gpp_gb  * 1000 * 86400,   # kg → g / day JUST COMMENT THIS OUT IF  NO LONGER NEED THIS 
           Reco_gb = Reco_gb * 1000 * 86400    # kg → g / day JUST COMMENT THIS OUT IF  NO LONGER NEED THIS
           )
  
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
    mutate(time_bounds_2 = as.POSIXct(time_bounds_2), label = "Observed",
           gpp_gb  = gpp_gb  * 1000 * 86400,   # kg → g / day JUST COMMENT THIS OUT IF  NO LONGER NEED THIS
           Reco_gb = Reco_gb * 1000 * 86400    # kg → g / day JUST COMMENT THIS OUT IF  NO LONGER NEED THIS
           )
  
  obs_et0 <- obs_daily %>%
    select(time_bounds_2 = date, fao_et0 = ET_daily_mean) %>%
    mutate(time_bounds_2 = as.POSIXct(time_bounds_2), label = "Observed")
  
  combined_all <- bind_rows(jules_df, obs_main)
  combined_et0 <- bind_rows(jules_df %>% select(time_bounds_2, fao_et0, label), obs_et0)
  all_stat_data[[site_label]] <<- list(combined_all = combined_all, combined_et0 = combined_et0)
  
  vars_of_interest <- c("gpp_gb", "Reco_gb", "ftl_gb", "latent_heat", "ET_kg_m2_s")
  
  combined_long <- combined_all %>%
    pivot_longer(all_of(vars_of_interest), names_to = "variable", values_to = "value") %>%
    mutate(variable = recode(variable, ET_kg_m2_s = "ET", gpp_gb = "GPP", Reco_gb = "Reco", ftl_gb = "SH", latent_heat = "LE"))
  
  et0_long <- combined_et0 %>% rename(value = fao_et0) %>% mutate(variable = "FAO_ET")
  plot_df <- bind_rows(combined_long, et0_long) %>% mutate(Source = ifelse(label == site_label, "JULES", "Observed"), site = site_label)
  
  all_plot_data[[site_label]] <<- plot_df
  message(paste("Finished processing:", site_label))
}

# ----------- Loop Across All Sites ---------------
csv_folder <- "C:/Users/efa206/OneDrive - University of Exeter/Desktop/combined_data_with_ET"
ncd_folder <- "C:/Users/efa206/OneDrive - University of Exeter/Desktop/JULES_Output/File_1000"

site_map <- tribble(
  ~site_label,                 ~csv_file,                   ~ncd_file,
  "BW_GUM - Wetland",          "BW_GUM_with_ET.csv",        "part6_BW_GUM-JULES_vn7.4.D.nc",
  "BW_NXR - Wetland",          "BW_NXR_with_ET.csv",        "part6_BW_NXR-JULES_vn7.4.D.nc",
  "CG_TCH - Grassland",        "CG_TCH_with_ET.csv",        "part6_CG_TCH-JULES_vn7.4.D.nc",
  "GH_ANK - Forest",           "GH_ANK_with_ET.csv",        "part6_GH_ANK-JULES_vn7.4.D.nc",
  "ML_AGG - Grassland",        "ML_AGG_with_ET.csv",        "part6_ML_AGG-JULES_vn7.4.D.nc",
  "NE_WAF - Savanna",          "NE_WAF_with_ET.csv",        "part6_NE_WAF-JULES_vn7.4.D.nc",
  "NE_WAM - Savanna",          "NE_WAM_with_ET.csv",        "part6_NE_WAM-JULES_vn7.4.D.nc",
  "SD_DEM - Grassland",        "SD_DEM_with_ET.csv",        "part6_SD_DEM-JULES_vn7.4.D.nc",
  "SN_DHR - Grassland",        "SN_DHR_with_ET.csv",        "part6_SN_DHR-JULES_vn7.4.D.nc",
  "SN_NKR - Cropland",         "SN_NKR_with_ET.csv",        "part6_SN_NKR-JULES_vn7.4.D.nc",
  "SN_RAG - Cropland",         "SN_RAG_with_ET.csv",        "part6_SN_RAG-JULES_vn7.4.D.nc",
  "UG_JIN - Wetland",          "UG_JIN_with_ET.csv",        "part6_UG_JIN-JULES_vn7.4.D.nc",
  "ZA_CATH - Grassland",       "ZA_CATH2016_with_ET.csv",   "part6_ZA_CATH-JULES_vn7.4.D.nc",
  "ZA_KRU - Savanna",          "ZA_KRU_with_ET.csv",        "part6_ZA_KRU-JULES_vn7.4.D.nc",
  "ZA_WGN - Grassland",        "ZA_WGN_with_ET.csv",        "part6_ZA_WGN-JULES_vn7.4.D.nc",
  "ZM_MON - Forest",           "ZM_MON_with_ET.csv",        "part6_ZM_MON-JULES_vn7.4.D.nc"
)

for (i in 1:nrow(site_map)) {
  cat("\nProcessing:", site_map$site_label[i], "...\n")
  process_flux_site(
    site_label = site_map$site_label[i],
    ncd_path = file.path(ncd_folder, site_map$ncd_file[i]),
    csv_path = file.path(csv_folder, site_map$csv_file[i])
  )
}

# ----------- Plot Each Variable Across All Sites ---------------
plot_df_all_sites <- bind_rows(all_plot_data)
var_list <- unique(plot_df_all_sites$variable)
dir.create("variable_plots", showWarnings = FALSE)

for (var in var_list) {
  plot_var <- plot_df_all_sites %>% filter(variable == var)
  var_actual <- recode(var,
                       "GPP" = "gpp_gb",
                       "Reco" = "Reco_gb",
                       "SH" = "ftl_gb",
                       "LE" = "latent_heat",
                       "ET" = "ET_kg_m2_s",
                       "FAO_ET" = "fao_et0")
  
  label_with_units <- recode(var,
                             "GPP"     = "GPP (g C m⁻² day⁻¹)",
                             "Reco"    = "Reco (g C m⁻² day⁻¹)",
                             "SH"      = "SH (W m⁻²)",
                             "LE"      = "LE (W m⁻²)",
                             "ET"      = "ET (kg m⁻² s⁻¹)",
                             "FAO_ET"  = "FAO ET (kg m⁻² s⁻¹)"
  )
  
  stats_labels <- map_dfr(names(all_plot_data), function(site) {
    df <- if (var == "FAO_ET") {
      all_stat_data[[site]]$combined_et0
    } else {
      all_stat_data[[site]]$combined_all
    }
    stat <- stat_tab_multi(df, site, var_actual, var)
    stat %>% mutate(facet_label = paste0(
      site, "\n",
      "COR=", signif(cor, 2), ", Bias=", signif(bias, 2), ", RMSE=", signif(rmse, 2)
    ))
  })
  
  
  
  
  
  plot_var <- plot_var %>%
    left_join(stats_labels %>% select(site = label, facet_label), by = "site")
  
  
  p <- ggplot(plot_var) +
    aes(x = time_bounds_2, y = value, colour = Source) +
    geom_line(linewidth = 0.4) +
    guides(colour = guide_legend(override.aes = list(linewidth = 2))) +
    facet_wrap(~facet_label, scales = "free", ncol = 3) +
    scale_color_brewer(palette = "Set2") +
    theme_minimal(base_family = "") +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.position = "bottom",
      legend.text = element_text(size = 16),  # ← make legend labels bigger
      legend.title = element_blank(),
      strip.text = element_text(size = 16, face = "bold"),
      axis.title = element_text(size = 18, face = "bold"),
      axis.text = element_text(size = 16, face = "bold")
    ) +
    labs(x = "Date", y = label_with_units, colour = "")
  
  ggsave(
    filename = file.path("variable_plots", paste0("AllSites_", var, ".png")),
    plot = p, width = 18, height = 14, dpi = 300
  )
}
