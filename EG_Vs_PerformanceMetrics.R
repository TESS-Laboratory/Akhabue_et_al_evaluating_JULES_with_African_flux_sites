# load library ----
library(tidyverse)
library(lme4)
library(lmerTest)
library(ggeffects)
library(sjPlot)
library(patchwork)
library(ggplot2)
library(dplyr)
library(stargazer)
library(sjPlot)
library(broom.mixed)
library(gt)
library(purrr)

# prepare data (step similar to the one for GLMM, that is fine) ----




# Define path to the stats directory----
stats_dir <- "C:/workspace/Akhabue-dev/stat_err"

# List all CSV files
stat_files <- list.files(stats_dir, pattern = "_stats\\.csv$", full.names = TRUE)

# Read and combine all into one dataframe
combined_stats <- map_dfr(stat_files, read_csv, show_col_types = FALSE)

# Clean site label column 
combined_stats <- combined_stats %>%
  rename(SiteID = label, FluxType = variable) %>%
  select(SiteID, FluxType, bias, everything())  # optional: reorder

# Preview
glimpse(combined_stats)



# read environmental gradient data
env_data <- read_csv("MAP_MAT_AI_1991_2020.csv", show_col_types = FALSE)


env_data <- env_data %>%
  mutate(SiteID = str_trim(toupper(SiteID)))


# Merge the two by SiteID
glmm_data <- combined_stats %>%
  left_join(env_data, by = "SiteID")

# Step 4: Confirm structure
glimpse(glmm_data)



# remove FAO_ET because I am using the ET for the further analysis instead
glmm_data <- glmm_data %>%
  filter(FluxType != "FAO_ET")

glmm_data <- glmm_data %>%
  filter(FluxType != "LE")

glmm_data <- glmm_data %>%
  filter(FluxType != "SH")


my_data <- glmm_data %>% 
  mutate(scaled_cor = cor / obs_mean)


write_csv(my_data, "my_data.csv")






# make 15 plots for EG vs Flux for scaled and unscaled performance metrics ----

# keep SiteID colours consistent across all figures
my_data <- my_data %>%
  mutate(SiteID = factor(SiteID, levels = sort(unique(SiteID))))

grad_lab <- c(MAT = "Mean Annual Temp (°C)",
              MAP = "Mean Annual Precip (mm)",
              AI  = "Aridity Index (MAP/PET)")

# Generalized triptych (works for bias, rmse, cor; scaled applies to bias/rmse)
flux_triptych <- function(df, flux = c("GPP","Reco","ET"),
                          metric = c("bias","rmse","cor"),
                          scaled = FALSE,
                          gradients = c("MAT","MAP","AI"),
                          ncol = 2,
                          add_trend = TRUE,   # <--
                          show_slope = TRUE,  # <--
                          save_path = NULL) {
  
  flux   <- rlang::arg_match(flux)
  metric <- rlang::arg_match(metric)
  
  ycol <- switch(metric,
                 bias = if (scaled) "scaled_bias" else "bias",
                 rmse = if (scaled) "scaled_rmse" else "rmse",
                 cor  = "cor"
  )
  ylab <- switch(metric,
                 bias = if (scaled) "Scaled BIAS" else "BIAS",
                 rmse = if (scaled) "Scaled RMSE" else "RMSE",
                 cor  = "Correlation"
  )
  
  d <- df %>% dplyr::filter(FluxType == flux)
  
  ylims <- range(d[[ycol]], na.rm = TRUE); if (!all(is.finite(ylims))) ylims <- NULL
  
  make_panel <- function(g){
    # ranges for nice label placement
    xr <- range(d[[g]],    na.rm = TRUE); dx <- diff(xr)
    yr <- range(d[[ycol]], na.rm = TRUE); dy <- diff(yr)
    
    p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[[g]], y = .data[[ycol]], fill = SiteID)) +
      ggplot2::geom_point(shape = 21, size = 3.8, stroke = 1.05, color = "black", alpha = 0.95) +
      ggplot2::labs(x = grad_lab[[g]], y = NULL, fill = "Site") +
      ggplot2::theme_bw() + ggplot2::theme(legend.position = "right")
    
    if (metric == "bias") p <- p + ggplot2::geom_hline(yintercept = 0, linetype = "dashed")
    
    if (add_trend) {
      # single global trend line (doesn't inherit fill mapping, no legend)
      p <- p + ggplot2::geom_smooth(
        ggplot2::aes(x = .data[[g]], y = .data[[ycol]]),
        inherit.aes = FALSE, method = "lm", se = TRUE, formula = y ~ x,
        color = "black", fill = NA, linewidth = 0.9, show.legend = FALSE
      )
    }
    
    if (show_slope) {
      df_fit <- d %>%
        dplyr::transmute(x = .data[[g]], y = .data[[ycol]]) %>%
        dplyr::filter(is.finite(x), is.finite(y))
      if (nrow(df_fit) > 2) {
        fit <- stats::lm(y ~ x, data = df_fit)
        slope <- unname(stats::coef(fit)[2])
        pval  <- summary(fit)$coefficients[2, 4]
        p <- p + ggplot2::annotate(
          "label",
          x = xr[1] + 0.02 * dx, y = yr[2] - 0.02 * dy,  # top-left inside panel
          hjust = 0, vjust = 1, size = 3,
          label = sprintf("slope = %.3g\np = %.3f", slope, pval)
        )
      }
    }
    
    if (!is.null(ylims)) p <- p + ggplot2::coord_cartesian(ylim = ylims)
    p
  }
  
  panels <- lapply(gradients, make_panel)
  
  fig <- (patchwork::wrap_plots(panels, ncol = ncol) + patchwork::plot_layout(guides = "collect")) &
    ggplot2::theme(legend.position = "right") &
    ggplot2::labs(y = ylab)
  
  fig <- fig + patchwork::plot_annotation(title = flux)
  
  if (!is.null(save_path)) ggplot2::ggsave(save_path, fig, width = 10, height = 8, dpi = 300)
  fig
}






fluxes <- c("GPP","Reco","ET")
grad_order <- c("MAT","MAP","AI")  # if you changed the default

# Bias
walk(fluxes, ~ flux_triptych(my_data, .x, "bias", FALSE,
                             gradients = grad_order, add_trend = TRUE, show_slope = FALSE,
                             save_path = paste0(.x, "_bias.png")))
walk(fluxes, ~ flux_triptych(my_data, .x, "bias", TRUE,
                             gradients = grad_order, add_trend = TRUE, show_slope = FALSE,
                             save_path = paste0(.x, "_bias_scaled.png")))

# RMSE
walk(fluxes, ~ flux_triptych(my_data, .x, "rmse", FALSE,
                             gradients = grad_order, add_trend = TRUE, show_slope = FALSE,
                             save_path = paste0(.x, "_rmse.png")))
walk(fluxes, ~ flux_triptych(my_data, .x, "rmse", TRUE,
                             gradients = grad_order, add_trend = TRUE, show_slope = FALSE,
                             save_path = paste0(.x, "_rmse_scaled.png")))

# Correlation
walk(fluxes, ~ flux_triptych(my_data, .x, "cor",
                             gradients = grad_order, add_trend = TRUE, show_slope = FALSE,
                             save_path = paste0(.x, "_cor.png")))
