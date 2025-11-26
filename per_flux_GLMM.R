# Load libraries ----
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
library(performance)
library(see)

# Define path to the stats directory ----
stats_dir <- "C:/workspace/Akhabue-dev/stats_copy"

# List all CSV files
stat_files <- list.files(stats_dir, pattern = "_stats_yearly\\.csv$", full.names = TRUE)

# Read and combine all into one dataframe
combined_stats <- map_dfr(stat_files, read_csv, show_col_types = FALSE)

# Clean site label column 
combined_stats <- combined_stats %>%
  rename(SiteID = label, FluxType = variable) %>%
  select(SiteID, FluxType, Year, bias, everything())  # optional: reorder

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


# read precipitation anomaly data
precip_anomaly <- read_csv("precip_anomaly.csv", show_col_types = FALSE)



# First, clean the Site column in precip_anomaly to match SiteID
precip_anomaly_clean <- precip_anomaly %>%
  mutate(SiteID = toupper(gsub("_clim", "", Site)))

# A minor check to correct ZA_CATH name (to ZA_CATH2016)
precip_anomaly_clean <- precip_anomaly_clean %>%
  mutate(SiteID = if_else(SiteID == "ZA_CATH", "ZA_CATH2016", SiteID))


# Now join with glmm_data by SiteID and year
glmm_data_joined <- glmm_data %>%
  left_join(precip_anomaly_clean %>%
              select(SiteID, Year, Precip_Anomaly_Percent),
            by = c("SiteID", "Year"))



#write_csv(glmm_data, "glmm_data.csv")
#write_csv(glmm_data_joined, "glmm_data_joined.csv")
#my_data <- read.csv("glmm_data.csv")


glmm_data_joined <- glmm_data_joined %>%
  mutate(
    MAT_z = scale(MAT),
    MAP_z = scale(MAP),
    AI_z  = scale(AI),
    Precip_Anomaly_Percent_z = scale(Precip_Anomaly_Percent)
  )


# remove FAO_ET because I am using the ET for the further analysis instead
glmm_data_joined <- glmm_data_joined %>%
  filter(FluxType != "FAO_ET")

glmm_data_joined <- glmm_data_joined %>%
  filter(FluxType != "LE")

glmm_data_joined <- glmm_data_joined %>%
  filter(FluxType != "SH")





# Normalizing continuous response ----

## bias, rmse and corr ----
glmm_data_joined <- glmm_data_joined %>%
  group_by(FluxType) %>%
  mutate(
    # ---- Bias
    denom_bias = max(abs(bias)),
    denom_bias = if_else(denom_bias > 0, denom_bias, 1),
    performance_bias = 1 - abs(bias) / denom_bias,
    
    # ---- RMSE 
    denom_rmse = max(rmse),
    denom_rmse = if_else(denom_rmse > 0, denom_rmse, 1),
    performance_rmse = 1 - rmse / denom_rmse
  ) %>%
  ungroup() %>%
  mutate(
    performance_bias = pmin(1, pmax(0, performance_bias)),
    performance_rmse = pmin(1, pmax(0, performance_rmse)),
    
    # ---- Correlation r in [-1,1] → performance in [0,1]
    # -1 → 0 (worst), 0 → 0.5 (no linear assoc.), +1 → 1 (best)
    performance_cor  = pmin(1, pmax(0, (cor + 1) / 2))
    
    # Optional: Fisher z for modelling r with Gaussian errors
    #cor_z = atanh(pmin(pmax(cor, -0.999999), 0.999999))
  )



glmm_data_joined$performancebias2 <- 1 - abs(glmm_data_joined$bias) / max(abs(glmm_data_joined$bias))

glmm_data_joined <- glmm_data_joined %>%
  mutate(abs_bias = abs(bias))




# Separate into df per flux ----

dfs <- split(glmm_data_joined, glmm_data_joined$FluxType)

glmm_GPP  <- droplevels(dfs[["GPP"]])
glmm_Reco <- droplevels(dfs[["Reco"]])
glmm_ET   <- droplevels(dfs[["ET"]])


lapply(dfs, \(x) unique(x$FluxType))
# should return $GPP -> "GPP", $Reco -> "Reco", $ET -> "ET"




# write_csv(glmm_data_joined, "glmm_input_data.csv")

# write_csv(glmm_data, "glmm_data_2.csv")




# GLMM ----

## model Bias ----

GPP_Bias <-lmer(scaled_bias ~ MAT + AI + Precip_Anomaly_Percent + (1 | SiteID), data = glmm_GPP)
summary(GPP_Bias)

ET_Bias <-lmer(scaled_bias ~ MAT + AI + Precip_Anomaly_Percent + (1 | SiteID), data = glmm_ET)
summary(ET_Bias)

Reco_Bias <-lmer(scaled_bias ~ MAT + AI + Precip_Anomaly_Percent + (1 | SiteID), data = glmm_Reco)
summary(Reco_Bias)




### performance and diagnostic check ----
cm_model_GPP_bias <- performance::check_model(GPP_Bias)

cm_model_ET_bias <- performance::check_model(ET_Bias)

cm_model_Reco_bias <- performance::check_model(Reco_Bias)



# 3 columns layout
plot_cm_model_GPP_bias <- plot(cm_model_GPP_bias, n_columns = 3)

plot_cm_model_ET_bias <- plot(cm_model_ET_bias, n_columns = 3)

plot_cm_model_Reco_bias <- plot(cm_model_Reco_bias, n_columns = 3)




# save
ggplot2::ggsave("model_check_model_GPP_bias_3col.png", plot_cm_model_GPP_bias,
                width = 18, height = 12, dpi = 600, bg = "white")


# save
ggplot2::ggsave("model_check_model_ET_bias_3col.png", plot_cm_model_ET_bias,
                width = 18, height = 12, dpi = 600, bg = "white")


# save
ggplot2::ggsave("model_check_model_Reco_bias_3col.png", plot_cm_model_Reco_bias,
                width = 18, height = 12, dpi = 600, bg = "white")




# summary/diagnostic
# 1
tab_model(GPP_Bias, file = "GLMM_model_GPP_Bias_summary.html")


tidy(GPP_Bias, effects = "fixed", conf.int = TRUE) |>
  gt() |>
  tab_header(title = "GLMM Summary: JULES GPP Bias across Environmental Gradients") |>
  fmt_number(
    columns = c(estimate, std.error, statistic, df, `p.value`, conf.low, conf.high),
    decimals = 4
  ) |>
  gtsave("GLMM_model_GPP_Bias_summary.rtf")  # or use .pdf


# 2
tab_model(ET_Bias, file = "GLMM_model_ET_Bias_summary.html")


tidy(ET_Bias, effects = "fixed", conf.int = TRUE) |>
  gt() |>
  tab_header(title = "GLMM Summary: JULES ET Bias across Environmental Gradients") |>
  fmt_number(
    columns = c(estimate, std.error, statistic, df, `p.value`, conf.low, conf.high),
    decimals = 4
  ) |>
  gtsave("GLMM_model_ET_Bias_summary.rtf")  # or use .pdf


# 3
tab_model(Reco_Bias, file = "GLMM_model_Reco_Bias_summary.html")


tidy(Reco_Bias, effects = "fixed", conf.int = TRUE) |>
  gt() |>
  tab_header(title = "GLMM Summary: JULES Reco Bias across Environmental Gradients") |>
  fmt_number(
    columns = c(estimate, std.error, statistic, df, `p.value`, conf.low, conf.high),
    decimals = 4
  ) |>
  gtsave("GLMM_model_Reco_Bias_summary.rtf")  # or use .pdf





### plots ----


# helpers (once) ----------------------------------------------------------
# --- p-values + stars  ---
get_term_p <- function(mod, term) {
  if (requireNamespace("lmerTest", quietly = TRUE)) {
    a <- try(lmerTest::anova(mod, type = 3), silent = TRUE)
    if (!inherits(a, "try-error") && "Pr(>F)" %in% colnames(a)) {
      rn <- rownames(a); idx <- which(rn %in% c(term, paste0("`", term, "`")))
      if (length(idx) == 1) return(as.numeric(a[idx, "Pr(>F)"]))
    }
  }
  cf <- coef(summary(mod)); rn <- rownames(cf)
  idx <- which(rn %in% c(term, paste0("`", term, "`")))
  if (length(idx) == 1 && "Pr(>|t|)" %in% colnames(cf)) as.numeric(cf[idx, "Pr(>|t|)"]) else NA_real_
}

p_stars <- function(p) {
  if (is.na(p)) "" else if (p < .001) "***"
  else if (p < .01) "**" else if (p < .05) "*" else ""
}



add_pval <- function(plt, mod, term, show = c("stars","p"), digits = 2) {
  show <- match.arg(show)
  p <- get_term_p(mod, term)
  lab <- if (show == "stars") p_stars(p) else paste0("p = ", format.pval(p, digits = digits, eps = 1e-3))
  plt +
    annotate("text", x = Inf, y = Inf, label = lab, hjust = 1.1, vjust = 1.3,
             size = 5, fontface = "bold") +
    coord_cartesian(clip = "off")
}



# draw a larger p-label on top (stars or numeric)
add_pval <- function(plt, mod, term, show = c("stars","p"),
                         size = 10, colour = "black",
                         hjust = 1.1, vjust = 1.3) {
  show <- match.arg(show)
  p <- get_term_p(mod, term)
  lab <- if (show == "stars") p_stars(p) else paste0("p = ", format.pval(p, digits = 2, eps = 1e-3))
  plt +
    annotate("text", x = Inf, y = Inf, label = lab,
             hjust = hjust, vjust = vjust, size = size,
             fontface = "bold", colour = colour) +
    coord_cartesian(clip = "off")
}




# --- shared y-lims from preds + CIs (prevents ribbon clipping) (once) ---
row_ylims_ci <- function(mod, terms = c("MAT","AI","Precip_Anomaly_Percent"),
                         expand = 0.05, clamp = NULL) {
  dfs <- lapply(terms, function(t) as.data.frame(ggpredict(mod, terms = t)))
  y   <- unlist(lapply(dfs, function(d) c(d$predicted, d$conf.low, d$conf.high)))
  rng <- range(y, na.rm = TRUE)
  if (!is.null(clamp)) rng <- c(max(rng[1], clamp[1]), min(rng[2], clamp[2]))
  pad <- diff(rng) * expand
  c(rng[1] - pad, rng[2] + pad)
}

# --- layout helpers (once) ---
hide_y <- theme(axis.title.y = element_blank(),
                axis.text.y  = element_blank(),
                axis.ticks.y = element_blank())

panel_box <- theme(
  panel.border     = element_rect(colour = "black", fill = NA, linewidth = 1.1),
  panel.background = element_blank(),
  panel.grid       = element_blank(),
  axis.line        = element_blank(),  # avoid double-thick edges
  axis.line.x      = element_blank(),
  axis.line.y      = element_blank()
)

combine_row <- function(p_mat, p_ai, p_prec, model,
                        title = NULL, clamp = NULL, x_on_top = FALSE,
                        include = NULL,        # <— NEW: force-included y value(s)
                        ylim_floor = NULL,     # <— optional lower floor
                        ylim_ceiling = NULL) { # <— optional upper ceiling
  ylims <- row_ylims_ci(model, clamp = clamp)
  
  if (!is.null(include))     ylims <- range(c(ylims, include))
  if (!is.null(ylim_floor))  ylims[1] <- min(ylims[1], ylim_floor)
  if (!is.null(ylim_ceiling))ylims[2] <- max(ylims[2], ylim_ceiling)
  
  row <- (p_mat | (p_ai & hide_y) | (p_prec & hide_y)) &
    coord_cartesian(ylim = ylims, clip = "off") & panel_box
  
  row <- row + plot_layout(ncol = 3, guides = "collect")
  if (x_on_top) row <- row & scale_x_continuous(position = "top")
  if (!is.null(title)) {
    row <- row + plot_annotation(
      title = title,
      theme = theme(plot.title = element_text(size = 28, face = "bold"),
                    plot.title.position = "plot")
    )
  }
  row
}


no_xaxis_all <- theme(
  axis.title.x        = element_blank(),
  axis.text.x         = element_blank(),
  axis.ticks.x        = element_blank(),
  axis.title.x.top    = element_blank(),
  axis.text.x.top     = element_blank(),
  axis.ticks.x.top    = element_blank(),
  axis.title.x.bottom = element_blank(),
  axis.text.x.bottom  = element_blank(),
  axis.ticks.x.bottom = element_blank()
)



# draw a vertical group label
group_y_label <- function(text, size = 18, face = "bold", vjust = 0.5) {
  ggplot() +
    annotate("text", x = 0, y = 0.5, label = text, angle = 90,
             size = size, fontface = face, hjust = 0.5, vjust = vjust) +
    theme_void() +
    theme(plot.margin = margin(5.5, 8, 5.5, 8))  # a little breathing room
}





#### GPP_Bias ----

# Plot for MAT
plot_GPP_bias_mat <- plot(ggpredict(GPP_Bias, terms = c("MAT"))) +
  geom_line(linewidth = 1.3) +
  labs(x = "Mean Annual Temperature (°C)", y = "GPP") + ggtitle(NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y   = element_text(size = 19, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"))
  



# Repeat for AI
plot_GPP_bias_ai <- plot(ggpredict(GPP_Bias, terms = c("AI"))) +
  geom_line(linewidth = 1.3) +
  labs(x = "Aridity Index", y = "GPP") + ggtitle(NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  



# Repeat for Precipitation Anomaly
plot_GPP_bias_Precip_Anomaly <- plot(ggpredict(GPP_Bias, terms = c("Precip_Anomaly_Percent"))) +
  geom_line(linewidth = 1.3) +
  labs(x = "Precipitation Anomaly (%)", y = "GPP") + ggtitle(NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  


# GPP — Bias
plot_GPP_bias_mat            <- add_pval(plot_GPP_bias_mat,            GPP_Bias, "MAT",                    show = "stars")
plot_GPP_bias_ai             <- add_pval(plot_GPP_bias_ai,             GPP_Bias, "AI",                     show = "stars")
plot_GPP_bias_Precip_Anomaly <- add_pval(plot_GPP_bias_Precip_Anomaly, GPP_Bias, "Precip_Anomaly_Percent", show = "stars")





# Combine
combined_GPP_bias_plot <- combine_row(
  plot_GPP_bias_mat, plot_GPP_bias_ai, plot_GPP_bias_Precip_Anomaly,
  model = GPP_Bias, x_on_top = TRUE
) & theme(legend.position = "right",
          plot.title        = element_text(size = 19, face = "bold"),
          axis.title.x.top  = element_text(size = 19, face = "bold", margin = margin(b = 6)),
          axis.text.x.top   = element_text(size = 19, face = "bold"),
          axis.text.x       = element_text(size = 19, face = "bold")
)


# Show
combined_GPP_bias_plot


# Save the combined plot as a high-res PNG
ggsave(
  filename = "combined_GPP_bias_plot_2.png",
  plot = combined_GPP_bias_plot,
  width = 14,       # in inches
  height = 10,      # adjust as needed
  dpi = 600         # high resolution
)







#### ET_Bias ----

# Plot for MAT
plot_ET_bias_mat <- plot(ggpredict(ET_Bias, terms = c("MAT"))) +
  geom_line(linewidth = 1.3) +  # <- if needed manually
  labs(x = "Mean Annual Temperature (°C)", y = "ET") + ggtitle(NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y   = element_text(size = 19, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"))




# Repeat for AI
plot_ET_bias_ai <- plot(ggpredict(ET_Bias, terms = c("AI"))) +
  geom_line(linewidth = 1.3) +
  labs(x = "Aridity Index", y = "ET") + ggtitle(NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())




# Repeat for Precipitation Anomaly
plot_ET_bias_Precip_Anomaly <- plot(ggpredict(ET_Bias, terms = c("Precip_Anomaly_Percent"))) +
  geom_line(linewidth = 1.3) +
  labs(x = "Precipitation Anomaly (%)", y = "ET") + ggtitle(NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())





# ET — Bias
plot_ET_bias_mat            <- add_pval(plot_ET_bias_mat,            ET_Bias, "MAT",                    show = "stars")
plot_ET_bias_ai             <- add_pval(plot_ET_bias_ai,             ET_Bias, "AI",                     show = "stars")
plot_ET_bias_Precip_Anomaly <- add_pval(plot_ET_bias_Precip_Anomaly, ET_Bias, "Precip_Anomaly_Percent", show = "stars")





# Combine
combined_ET_bias_plot <- combine_row(
  plot_ET_bias_mat, plot_ET_bias_ai, plot_ET_bias_Precip_Anomaly,
  model = ET_Bias, x_on_top = TRUE
) & theme(legend.position = "right",
          plot.title        = element_text(size = 19, face = "bold"),
          axis.title.x.top  = element_text(size = 19, margin = margin(b = 6)),
          axis.text.x.top   = element_text(size = 19)
) & no_xaxis_all

# Show
combined_ET_bias_plot




# Save the combined plot as a high-res PNG
ggsave(
  filename = "combined_ET_bias_plot_2.png",
  plot = combined_ET_bias_plot,
  width = 14,       # in inches
  height = 10,      # adjust as needed
  dpi = 600         # high resolution
)







#### Reco_Bias ----

# Plot for MAT
plot_Reco_bias_mat <- plot(ggpredict(Reco_Bias, terms = c("MAT"))) +
  geom_line(linewidth = 1.3) +  # <- if needed manually
  labs(x = "Mean Annual Temperature (°C)", y = "Reco") + ggtitle(NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y   = element_text(size = 19, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"))





# Repeat for AI
plot_Reco_bias_ai <- plot(ggpredict(Reco_Bias, terms = c("AI"))) +
  geom_line(linewidth = 1.3) +
  labs(x = "Aridity Index", y = "Reco") + ggtitle(NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())





# Repeat for Precipitation Anomaly
plot_Reco_bias_Precip_Anomaly <- plot(ggpredict(Reco_Bias, terms = c("Precip_Anomaly_Percent"))) +
  geom_line(linewidth = 1.3) +
  labs(x = "Precipitation Anomaly (%)", y = "Reco") + ggtitle(NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())





# RECO — Bias
plot_Reco_bias_mat            <- add_pval(plot_Reco_bias_mat,            Reco_Bias, "MAT",                    show = "stars")
plot_Reco_bias_ai             <- add_pval(plot_Reco_bias_ai,             Reco_Bias, "AI",                     show = "stars")
plot_Reco_bias_Precip_Anomaly <- add_pval(plot_Reco_bias_Precip_Anomaly, Reco_Bias, "Precip_Anomaly_Percent", show = "stars")




# Combine
combined_Reco_bias_plot <- combine_row(
  plot_Reco_bias_mat, plot_Reco_bias_ai, plot_Reco_bias_Precip_Anomaly,
  model = Reco_Bias, x_on_top = TRUE
) & theme(legend.position = "right",
          plot.title        = element_text(size = 19, face = "bold"),
          axis.title.x.top  = element_text(size = 19, margin = margin(b = 6)),
          axis.text.x.top   = element_text(size = 19)
) & no_xaxis_all


# Show
combined_Reco_bias_plot




# Save the combined plot as a high-res PNG
ggsave(
  filename = "combined_Reco_bias_plot_2.png",
  plot = combined_Reco_bias_plot,
  width = 14,       # in inches
  height = 10,      # adjust as needed
  dpi = 600         # high resolution
)









## model Rmse ----

GPP_Rmse <-lmer(scaled_rmse ~ MAT + AI + Precip_Anomaly_Percent + (1 | SiteID), data = glmm_GPP)
summary(GPP_Rmse)

ET_Rmse <-lmer(scaled_rmse ~ MAT + AI + Precip_Anomaly_Percent + (1 | SiteID), data = glmm_ET)
summary(ET_Rmse)

Reco_Rmse <-lmer(scaled_rmse ~ MAT + AI + Precip_Anomaly_Percent + (1 | SiteID), data = glmm_Reco)
summary(Reco_Rmse)




### performance and diagnostic check ----
cm_model_GPP_Rmse <- performance::check_model(GPP_Rmse)

cm_model_ET_Rmse <- performance::check_model(ET_Rmse)

cm_model_Reco_Rmse <- performance::check_model(Reco_Rmse)



# 3 columns layout
plot_cm_model_GPP_Rmse <- plot(cm_model_GPP_Rmse, n_columns = 3)

plot_cm_model_ET_Rmse <- plot(cm_model_ET_Rmse, n_columns = 3)

plot_cm_model_Reco_Rmse <- plot(cm_model_Reco_Rmse, n_columns = 3)



# save
ggplot2::ggsave("model_check_model_GPP_Rmse_3col.png", plot_cm_model_GPP_Rmse,
                width = 18, height = 12, dpi = 600, bg = "white")


# save
ggplot2::ggsave("model_check_model_ET_Rmse_3col.png", plot_cm_model_ET_Rmse,
                width = 18, height = 12, dpi = 600, bg = "white")


# save
ggplot2::ggsave("model_check_model_Reco_Rmse_3col.png", plot_cm_model_Reco_Rmse,
                width = 18, height = 12, dpi = 600, bg = "white")





# summary/diagnostic
# 1
tab_model(GPP_Rmse, file = "GLMM_model_GPP_Rmse_summary.html")


tidy(GPP_Rmse, effects = "fixed", conf.int = TRUE) |>
  gt() |>
  tab_header(title = "GLMM Summary: JULES GPP Rmse across Environmental Gradients") |>
  fmt_number(
    columns = c(estimate, std.error, statistic, df, `p.value`, conf.low, conf.high),
    decimals = 4
  ) |>
  gtsave("GLMM_model_GPP_Rmse_summary.rtf")  # or use .pdf


# 2
tab_model(ET_Rmse, file = "GLMM_model_ET_Rmse_summary.html")


tidy(ET_Rmse, effects = "fixed", conf.int = TRUE) |>
  gt() |>
  tab_header(title = "GLMM Summary: JULES ET Rmse across Environmental Gradients") |>
  fmt_number(
    columns = c(estimate, std.error, statistic, df, `p.value`, conf.low, conf.high),
    decimals = 4
  ) |>
  gtsave("GLMM_model_ET_Rmse_summary.rtf")  # or use .pdf


# 3
tab_model(Reco_Rmse, file = "GLMM_model_Reco_Rmse_summary.html")


tidy(Reco_Rmse, effects = "fixed", conf.int = TRUE) |>
  gt() |>
  tab_header(title = "GLMM Summary: JULES Reco Rmse across Environmental Gradients") |>
  fmt_number(
    columns = c(estimate, std.error, statistic, df, `p.value`, conf.low, conf.high),
    decimals = 4
  ) |>
  gtsave("GLMM_model_Reco_Rmse_summary.rtf")  # or use .pdf




### plots ----

#### GPP_Rmse ----

# Plot for MAT
plot_GPP_Rmse_mat <- plot(ggpredict(GPP_Rmse, terms = c("MAT"))) +
  geom_line(linewidth = 1.3) +  # <- if needed manually
  labs(x = "Mean Annual Temperature (°C)", y = "GPP") + ggtitle(NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y   = element_text(size = 19, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"))




# Repeat for AI
plot_GPP_Rmse_ai <- plot(ggpredict(GPP_Rmse, terms = c("AI"))) +
  geom_line(linewidth = 1.3) +
  labs(x = "Aridity Index", y = "GPP") + ggtitle(NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())




# Repeat for Precipitation Anomaly
plot_GPP_Rmse_Precip_Anomaly <- plot(ggpredict(GPP_Rmse, terms = c("Precip_Anomaly_Percent"))) +
  geom_line(linewidth = 1.3) +
  labs(x = "Precipitation Anomaly (%)", y = "GPP") + ggtitle(NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())






# GPP — RMSE
plot_GPP_Rmse_mat            <- add_pval(plot_GPP_Rmse_mat,            GPP_Rmse, "MAT",                    show = "stars")
plot_GPP_Rmse_ai             <- add_pval(plot_GPP_Rmse_ai,             GPP_Rmse, "AI",                     show = "stars")
plot_GPP_Rmse_Precip_Anomaly <- add_pval(plot_GPP_Rmse_Precip_Anomaly, GPP_Rmse, "Precip_Anomaly_Percent", show = "stars")




# Combine
combined_GPP_Rmse_plot <- combine_row(
  plot_GPP_Rmse_mat, plot_GPP_Rmse_ai, plot_GPP_Rmse_Precip_Anomaly,
  model = GPP_Rmse, x_on_top = TRUE, ylim_floor = -1
) & theme(legend.position = "right",
          plot.title        = element_text(size = 19, face = "bold"),
          axis.title.x.top  = element_text(size = 19, margin = margin(b = 6)),
          axis.text.x.top   = element_text(size = 19)
) & no_xaxis_all


# Show
combined_GPP_Rmse_plot


# Save the combined plot as a high-res PNG
ggsave(
  filename = "combined_GPP_Rmse_plot_2.png",
  plot = combined_GPP_Rmse_plot,
  width = 14,       # in inches
  height = 10,      # adjust as needed
  dpi = 600         # high resolution
)






#### ET_Rmse ----

# Plot for MAT
plot_ET_Rmse_mat <- plot(ggpredict(ET_Rmse, terms = c("MAT"))) +
  geom_line(linewidth = 1.3) +  # <- if needed manually
  labs(x = "Mean Annual Temperature (°C)", y = "ET") + ggtitle(NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y   = element_text(size = 19, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"))





# Repeat for AI
plot_ET_Rmse_ai <- plot(ggpredict(ET_Rmse, terms = c("AI"))) +
  geom_line(linewidth = 1.3) +
  labs(x = "Aridity Index", y = "ET") + ggtitle(NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())





# Repeat for Precipitation Anomaly
plot_ET_Rmse_Precip_Anomaly <- plot(ggpredict(ET_Rmse, terms = c("Precip_Anomaly_Percent"))) +
  geom_line(linewidth = 1.3) +
  labs(x = "Precipitation Anomaly (%)", y = "ET") + ggtitle(NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())






# ET — RMSE
plot_ET_Rmse_mat            <- add_pval(plot_ET_Rmse_mat,            ET_Rmse, "MAT",                    show = "stars")
plot_ET_Rmse_ai             <- add_pval(plot_ET_Rmse_ai,             ET_Rmse, "AI",                     show = "stars")
plot_ET_Rmse_Precip_Anomaly <- add_pval(plot_ET_Rmse_Precip_Anomaly, ET_Rmse, "Precip_Anomaly_Percent", show = "stars")




# Combine
combined_ET_Rmse_plot <- combine_row(
  plot_ET_Rmse_mat, plot_ET_Rmse_ai, plot_ET_Rmse_Precip_Anomaly,
  model = ET_Rmse, x_on_top = TRUE, ylim_floor = -0.5
) & theme(legend.position = "right",
          plot.title        = element_text(size = 19, face = "bold"),
          axis.title.x.top  = element_text(size = 19, margin = margin(b = 6)),
          axis.text.x.top   = element_text(size = 19)
) & no_xaxis_all

# Show
combined_ET_Rmse_plot




# Save the combined plot as a high-res PNG
ggsave(
  filename = "combined_ET_Rmse_plot_2.png",
  plot = combined_ET_Rmse_plot,
  width = 14,       # in inches
  height = 10,      # adjust as needed
  dpi = 600         # high resolution
)










#### Reco_Rmse ----

# Plot for MAT
plot_Reco_Rmse_mat <- plot(ggpredict(Reco_Rmse, terms = c("MAT"))) +
  geom_line(linewidth = 1.3) +  # <- if needed manually
  labs(x = "Mean Annual Temperature (°C)", y = "Reco") + ggtitle(NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y   = element_text(size = 19, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"))






# Repeat for AI
plot_Reco_Rmse_ai <- plot(ggpredict(Reco_Rmse, terms = c("AI"))) +
  geom_line(linewidth = 1.3) +
  labs(x = "Aridity Index", y = "Reco") + ggtitle(NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())





# Repeat for Precipitation Anomaly
plot_Reco_Rmse_Precip_Anomaly <- plot(ggpredict(Reco_Rmse, terms = c("Precip_Anomaly_Percent"))) +
  geom_line(linewidth = 1.3) +
  labs(x = "Precipitation Anomaly (%)", y = "Reco") + ggtitle(NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())





# RECO — RMSE
plot_Reco_Rmse_mat            <- add_pval(plot_Reco_Rmse_mat,            Reco_Rmse, "MAT",                    show = "stars")
plot_Reco_Rmse_ai             <- add_pval(plot_Reco_Rmse_ai,             Reco_Rmse, "AI",                     show = "stars")
plot_Reco_Rmse_Precip_Anomaly <- add_pval(plot_Reco_Rmse_Precip_Anomaly, Reco_Rmse, "Precip_Anomaly_Percent", show = "stars")




# Combine
combined_Reco_Rmse_plot <- combine_row(
  plot_Reco_Rmse_mat, plot_Reco_Rmse_ai, plot_Reco_Rmse_Precip_Anomaly,
  model = Reco_Rmse, x_on_top = TRUE
) & theme(legend.position = "right",
          plot.title        = element_text(size = 19, face = "bold"),
          axis.title.x.top  = element_text(size = 19, margin = margin(b = 6)),
          axis.text.x.top   = element_text(size = 19)
) & no_xaxis_all


# Show
combined_Reco_Rmse_plot




# Save the combined plot as a high-res PNG
ggsave(
  filename = "combined_Reco_Rmse_plot_2.png",
  plot = combined_Reco_Rmse_plot,
  width = 14,       # in inches
  height = 10,      # adjust as needed
  dpi = 600         # high resolution
)







## model Corr ----

GPP_Corr <-lmer(cor ~ MAT + AI + Precip_Anomaly_Percent + (1 | SiteID), data = glmm_GPP)
summary(GPP_Corr)

ET_Corr <-lmer(cor ~ MAT + AI + Precip_Anomaly_Percent + (1 | SiteID), data = glmm_ET)
summary(ET_Corr)

Reco_Corr <-lmer(cor ~ MAT + AI + Precip_Anomaly_Percent + (1 | SiteID), data = glmm_Reco)
summary(Reco_Corr)




### performance and diagnostic check ----
cm_model_GPP_Corr <- performance::check_model(GPP_Corr)

cm_model_ET_Corr <- performance::check_model(ET_Corr)

cm_model_Reco_Corr <- performance::check_model(Reco_Corr)




# 3 columns layout
plot_cm_model_GPP_Corr <- plot(cm_model_GPP_Corr, n_columns = 3)

plot_cm_model_ET_Corr <- plot(cm_model_ET_Corr, n_columns = 3)

plot_cm_model_Reco_Corr <- plot(cm_model_Reco_Corr, n_columns = 3)




# save
ggplot2::ggsave("model_check_model_GPP_Corr_3col.png", plot_cm_model_GPP_Corr,
                width = 18, height = 12, dpi = 600, bg = "white")


# save
ggplot2::ggsave("model_check_model_ET_Corr_3col.png", plot_cm_model_ET_Corr,
                width = 18, height = 12, dpi = 600, bg = "white")


# save
ggplot2::ggsave("model_check_model_Reco_Corr_3col.png", plot_cm_model_Reco_Corr,
                width = 18, height = 12, dpi = 600, bg = "white")





# summary/diagnostic
# 1
tab_model(GPP_Corr, file = "GLMM_model_GPP_Corr_summary.html")


tidy(GPP_Corr, effects = "fixed", conf.int = TRUE) |>
  gt() |>
  tab_header(title = "GLMM Summary: JULES GPP Corr across Environmental Gradients") |>
  fmt_number(
    columns = c(estimate, std.error, statistic, df, `p.value`, conf.low, conf.high),
    decimals = 4
  ) |>
  gtsave("GLMM_model_GPP_Corr_summary.rtf")  # or use .pdf


# 2
tab_model(ET_Corr, file = "GLMM_model_ET_Corr_summary.html")


tidy(ET_Corr, effects = "fixed", conf.int = TRUE) |>
  gt() |>
  tab_header(title = "GLMM Summary: JULES ET Corr across Environmental Gradients") |>
  fmt_number(
    columns = c(estimate, std.error, statistic, df, `p.value`, conf.low, conf.high),
    decimals = 4
  ) |>
  gtsave("GLMM_model_ET_Corr_summary.rtf")  # or use .pdf


# 3
tab_model(Reco_Corr, file = "GLMM_model_Reco_Corr_summary.html")


tidy(Reco_Corr, effects = "fixed", conf.int = TRUE) |>
  gt() |>
  tab_header(title = "GLMM Summary: JULES Reco Corr across Environmental Gradients") |>
  fmt_number(
    columns = c(estimate, std.error, statistic, df, `p.value`, conf.low, conf.high),
    decimals = 4
  ) |>
  gtsave("GLMM_model_Reco_Corr_summary.rtf")  # or use .pdf





### plots ----
#### GPP_Corr ----

# Plot for MAT
plot_GPP_Corr_mat <- plot(ggpredict(GPP_Corr, terms = c("MAT"))) +
  geom_line(linewidth = 1.3) +  # <- if needed manually
  labs(x = "Mean Annual Temperature (°C)", y = "GPP") + ggtitle(NULL) + 
  geom_hline(yintercept = 1, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y   = element_text(size = 19, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"))





# Repeat for AI
plot_GPP_Corr_ai <- plot(ggpredict(GPP_Corr, terms = c("AI"))) +
  geom_line(linewidth = 1.3) +
  labs(x = "Aridity Index", y = "GPP") + ggtitle(NULL) + 
  geom_hline(yintercept = 1, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())





# Repeat for Precipitation Anomaly
plot_GPP_Corr_Precip_Anomaly <- plot(ggpredict(GPP_Corr, terms = c("Precip_Anomaly_Percent"))) +
  geom_line(linewidth = 1.3) +
  labs(x = "Precipitation Anomaly (%)", y = "GPP") + ggtitle(NULL) + 
  geom_hline(yintercept = 1, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())





# GPP — Corr
plot_GPP_Corr_mat            <- add_pval(plot_GPP_Corr_mat,            GPP_Corr, "MAT",                    show = "stars")
plot_GPP_Corr_ai             <- add_pval(plot_GPP_Corr_ai,             GPP_Corr, "AI",                     show = "stars")
plot_GPP_Corr_Precip_Anomaly <- add_pval(plot_GPP_Corr_Precip_Anomaly, GPP_Corr, "Precip_Anomaly_Percent", show = "stars")




# Combine
combined_GPP_Corr_plot <- combine_row(
  plot_GPP_Corr_mat, plot_GPP_Corr_ai, plot_GPP_Corr_Precip_Anomaly,
  model = GPP_Corr, x_on_top = TRUE, clamp = c(0, 1), include = 0
) & theme(legend.position = "right",
          plot.title        = element_text(size = 19, face = "bold"),
          axis.title.x.top  = element_text(size = 19, margin = margin(b = 6)),
          axis.text.x.top   = element_text(size = 19)
) & no_xaxis_all


# Show
combined_GPP_Corr_plot


# Save the combined plot as a high-res PNG
ggsave(
  filename = "combined_GPP_Corr_plot_2.png",
  plot = combined_GPP_Corr_plot,
  width = 14,       # in inches
  height = 10,      # adjust as needed
  dpi = 600         # high resolution
)









#### ET_Corr ----

# Plot for MAT
plot_ET_Corr_mat <- plot(ggpredict(ET_Corr, terms = c("MAT"))) +
  geom_line(linewidth = 1.3) +  # <- if needed manually
  labs(x = "Mean Annual Temperature (°C)", y = "ET") + ggtitle(NULL) + 
  geom_hline(yintercept = 1, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y   = element_text(size = 19, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"))






# Repeat for AI
plot_ET_Corr_ai <- plot(ggpredict(ET_Corr, terms = c("AI"))) +
  geom_line(linewidth = 1.3) +
  labs(x = "Aridity Index", y = "ET") + ggtitle(NULL) + 
  geom_hline(yintercept = 1, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())




# Repeat for Precipitation Anomaly
plot_ET_Corr_Precip_Anomaly <- plot(ggpredict(ET_Corr, terms = c("Precip_Anomaly_Percent"))) +
  geom_line(linewidth = 1.3) +
  labs(x = "Precipitation Anomaly (%)", y = "ET") + ggtitle(NULL) + 
  geom_hline(yintercept = 1, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())





# ET — Corr
plot_ET_Corr_mat            <- add_pval(plot_ET_Corr_mat,            ET_Corr, "MAT",                    show = "stars")
plot_ET_Corr_ai             <- add_pval(plot_ET_Corr_ai,             ET_Corr, "AI",                     show = "stars")
plot_ET_Corr_Precip_Anomaly <- add_pval(plot_ET_Corr_Precip_Anomaly, ET_Corr, "Precip_Anomaly_Percent", show = "stars")




# Combine
combined_ET_Corr_plot <- combine_row(
  plot_ET_Corr_mat, plot_ET_Corr_ai, plot_ET_Corr_Precip_Anomaly,
  model = ET_Corr, x_on_top = FALSE, clamp = c(0, 1), include = 0
) & theme(legend.position = "right",
          plot.title        = element_text(size = 19, face = "bold"),
          axis.title.x.top  = element_text(size = 19, margin = margin(b = 6)),
          axis.text.x.top   = element_text(size = 19),
          axis.text.x       = element_text(size = 19, face = "bold"),
          axis.title.x      =element_text(size = 19, face = "bold") 
)



# Show
combined_ET_Corr_plot




# Save the combined plot as a high-res PNG
ggsave(
  filename = "combined_ET_Corr_plot_2.png",
  plot = combined_ET_Corr_plot,
  width = 14,       # in inches
  height = 10,      # adjust as needed
  dpi = 600         # high resolution
)









#### Reco_Corr ----

# Plot for MAT
plot_Reco_Corr_mat <- plot(ggpredict(Reco_Corr, terms = c("MAT"))) +
  geom_line(linewidth = 1.3) +  # <- if needed manually
  labs(x = "Mean Annual Temperature (°C)", y = "Reco") + ggtitle(NULL) + 
  geom_hline(yintercept = 1, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y   = element_text(size = 19, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"))






# Repeat for AI
plot_Reco_Corr_ai <- plot(ggpredict(Reco_Corr, terms = c("AI"))) +
  geom_line(linewidth = 1.3) +
  labs(x = "Aridity Index", y = "Reco") + ggtitle(NULL) + 
  geom_hline(yintercept = 1, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())





# Repeat for Precipitation Anomaly
plot_Reco_Corr_Precip_Anomaly <- plot(ggpredict(Reco_Corr, terms = c("Precip_Anomaly_Percent"))) +
  geom_line(linewidth = 1.3) +
  labs(x = "Precipitation Anomaly (%)", y = "Reco") + ggtitle(NULL) + 
  geom_hline(yintercept = 1, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())






# RECO — Corr
plot_Reco_Corr_mat            <- add_pval(plot_Reco_Corr_mat,            Reco_Corr, "MAT",                    show = "stars")
plot_Reco_Corr_ai             <- add_pval(plot_Reco_Corr_ai,             Reco_Corr, "AI",                     show = "stars")
plot_Reco_Corr_Precip_Anomaly <- add_pval(plot_Reco_Corr_Precip_Anomaly, Reco_Corr, "Precip_Anomaly_Percent", show = "stars")





# Combine
combined_Reco_Corr_plot <- combine_row(
  plot_Reco_Corr_mat, plot_Reco_Corr_ai, plot_Reco_Corr_Precip_Anomaly,
  model = Reco_Corr, x_on_top = TRUE, clamp = c(0, 1), include = 1
) & theme(legend.position = "right",
          plot.title        = element_text(size = 19, face = "bold"),
          axis.title.x.top  = element_text(size = 19, margin = margin(b = 6)),
          axis.text.x.top   = element_text(size = 19)
)  & no_xaxis_all


# Show
combined_Reco_Corr_plot




# Save the combined plot as a high-res PNG
ggsave(
  filename = "combined_Reco_Corr_plot_2.png",
  plot = combined_Reco_Corr_plot,
  width = 14,       # in inches
  height = 10,      # adjust as needed
  dpi = 600         # high resolution
)








# Combining and saving plots ----

bias_grid <- combined_GPP_bias_plot / combined_Reco_bias_plot / combined_ET_bias_plot
rmse_grid <- combined_GPP_Rmse_plot / combined_Reco_Rmse_plot / combined_ET_Rmse_plot
corr_grid <- combined_GPP_Corr_plot / combined_Reco_Corr_plot / combined_ET_Corr_plot

all_grid <- combined_GPP_bias_plot / combined_Reco_bias_plot / combined_ET_bias_plot / 
  combined_GPP_Rmse_plot / combined_Reco_Rmse_plot / combined_ET_Rmse_plot / 
  combined_GPP_Corr_plot / combined_Reco_Corr_plot / combined_ET_Corr_plot

all_grid_2 <- bias_grid / rmse_grid / corr_grid


bias_block <- combined_GPP_bias_plot / combined_Reco_bias_plot / combined_ET_bias_plot
rmse_block <- combined_GPP_Rmse_plot / combined_Reco_Rmse_plot / combined_ET_Rmse_plot
corr_block <- combined_GPP_Corr_plot / combined_Reco_Corr_plot / combined_ET_Corr_plot



bias_block_labeled <- group_y_label("Predicted Bias") | bias_block
bias_block_labeled <- bias_block_labeled + plot_layout(widths = c(0.08, 1))

rmse_block_labeled <- group_y_label("Predicted RMSE") | rmse_block
rmse_block_labeled <- rmse_block_labeled + plot_layout(widths = c(0.08, 1))

corr_block_labeled <- group_y_label("Predicted Correlation") | corr_block
corr_block_labeled <- corr_block_labeled + plot_layout(widths = c(0.08, 1))



all_grid_3 <- bias_block_labeled / rmse_block_labeled / corr_block_labeled






print(bias_grid)
print(rmse_grid)
print(corr_grid)
print(all_grid)
print(all_grid_2)
print(all_grid_3)






# Save the combined plot as a high-res PNG
ggsave(
  filename = "all_grid.png",
  plot = all_grid,
  width = 18,       # in inches
  height = 25,      # adjust as needed
  dpi = 600         # high resolution
)



# Save the combined plot as a high-res PNG
ggsave(
  filename = "all_grid_2.png",
  plot = all_grid_2,
  width = 18,       # in inches
  height = 25,      # adjust as needed
  dpi = 600         # high resolution
)



# Save the combined plot as a high-res PNG USE THIS!!!
ggsave(
  filename = "all_grid_33b.png",
  plot = all_grid_3,
  width = 18,       # in inches
  height = 25,      # adjust as needed
  dpi = 600         # high resolution
)



# Save the combined plot as a high-res PNG
ggsave(
  filename = "bias_grid.png",
  plot = bias_grid,
  width = 18,       # in inches
  height = 16,      # adjust as needed
  dpi = 600         # high resolution
)

# Save the combined plot as a high-res PNG
ggsave(
  filename = "rmse_grid.png",
  plot = rmse_grid,
  width = 18,       # in inches
  height = 16,      # adjust as needed
  dpi = 600         # high resolution
)

# Save the combined plot as a high-res PNG
ggsave(
  filename = "corr_grid.png",
  plot = corr_grid,
  width = 18,       # in inches
  height = 16,      # adjust as needed
  dpi = 600         # high resolution
)







# Perf_GLMM ----
## perf_bias ----

GPP_perf_Bias <-lmer(performance_bias ~ MAT + AI + Precip_Anomaly_Percent + (1 | SiteID), data = glmm_GPP)
summary(GPP_perf_Bias)

ET_perf_Bias <-lmer(performance_bias ~ MAT + AI + Precip_Anomaly_Percent + (1 | SiteID), data = glmm_ET)
summary(ET_perf_Bias)

Reco_perf_Bias <-lmer(performance_bias ~ MAT + AI + Precip_Anomaly_Percent + (1 | SiteID), data = glmm_Reco)
summary(Reco_perf_Bias)


### performance and diagnostic check ----


cm_model_perf_GPP_bias <- performance::check_model(GPP_perf_Bias)

cm_model_perf_ET_bias <- performance::check_model(ET_perf_Bias)

cm_model_perf_Reco_bias <- performance::check_model(Reco_perf_Bias)



plot_cm_model_perf_GPP_bias <- plot(cm_model_perf_GPP_bias, n_columns = 3)

plot_cm_model_perf_ET_bias <- plot(cm_model_perf_ET_bias, n_columns = 3)

plot_cm_model_perf_Reco_bias <- plot(cm_model_perf_Reco_bias, n_columns = 3)



# save
ggplot2::ggsave("model_check_model_perf_GPP_bias_3col.png", plot_cm_model_perf_GPP_bias,
                width = 18, height = 12, dpi = 600, bg = "white")


# save
ggplot2::ggsave("model_check_model_perf_ET_bias_3col.png", plot_cm_model_perf_ET_bias,
                width = 18, height = 12, dpi = 600, bg = "white")


# save
ggplot2::ggsave("model_check_model_perf_Reco_bias_3col.png", plot_cm_model_perf_Reco_bias,
                width = 18, height = 12, dpi = 600, bg = "white")



# 4
tab_model(GPP_perf_Bias, file = "GLMM_model_GPP_perf_Bias_summary.html")


tidy(GPP_perf_Bias, effects = "fixed", conf.int = TRUE) |>
  gt() |>
  tab_header(title = "GLMM Summary: JULES GPP_perf_Bias across Environmental Gradients") |>
  fmt_number(
    columns = c(estimate, std.error, statistic, df, `p.value`, conf.low, conf.high),
    decimals = 4
  ) |>
  gtsave("GLMM_model_GPP_perf_Bias_summary.rtf")  # or use .pdf


# 5 
tab_model(ET_perf_Bias, file = "GLMM_model_ET_perf_Bias_summary.html")


tidy(ET_perf_Bias, effects = "fixed", conf.int = TRUE) |>
  gt() |>
  tab_header(title = "GLMM Summary: JULES ET_perf_Bias across Environmental Gradients") |>
  fmt_number(
    columns = c(estimate, std.error, statistic, df, `p.value`, conf.low, conf.high),
    decimals = 4
  ) |>
  gtsave("GLMM_model_ET_perf_Bias_summary.rtf")  # or use .pdf


# 6 
tab_model(Reco_perf_Bias, file = "GLMM_model_Reco_perf_Bias_summary.html")


tidy(Reco_perf_Bias, effects = "fixed", conf.int = TRUE) |>
  gt() |>
  tab_header(title = "GLMM Summary: JULES Reco_perf_Bias across Environmental Gradients") |>
  fmt_number(
    columns = c(estimate, std.error, statistic, df, `p.value`, conf.low, conf.high),
    decimals = 4
  ) |>
  gtsave("GLMM_model_Reco_perf_Bias_summary.rtf")  # or use .pdf






### plot_perf_bias ----
#### GPP_perf_Bias ----

# Plot for MAT
plot_GPP_perf_Bias_mat <- plot(ggpredict(GPP_perf_Bias, terms = c("MAT"))) +
  geom_line(linewidth = 1.3) +
  labs(x = "Mean Annual Temperature (°C)", y = "GPP") + ggtitle(NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y   = element_text(size = 19, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"))



# Repeat for AI
plot_GPP_perf_Bias_ai <- plot(ggpredict(GPP_perf_Bias, terms = c("AI"))) +
  geom_line(linewidth = 1.3) +
  labs(x = "Aridity Index", y = "GPP") + ggtitle(NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())




# Repeat for Precipitation Anomaly
plot_GPP_perf_Bias_Precip_Anomaly <- plot(ggpredict(GPP_perf_Bias, terms = c("Precip_Anomaly_Percent"))) +
  geom_line(linewidth = 1.3) +
  labs(x = "Precipitation Anomaly (%)", y = "GPP") + ggtitle(NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



# GPP — Bias
plot_GPP_perf_Bias_mat            <- add_pval(plot_GPP_perf_Bias_mat,            GPP_perf_Bias, "MAT",                    show = "stars")
plot_GPP_perf_Bias_ai             <- add_pval(plot_GPP_perf_Bias_ai,             GPP_perf_Bias, "AI",                     show = "stars")
plot_GPP_perf_Bias_Precip_Anomaly <- add_pval(plot_GPP_perf_Bias_Precip_Anomaly, GPP_perf_Bias, "Precip_Anomaly_Percent", show = "stars")





# Combine
combined_GPP_perf_Bias_plot <- combine_row(
  plot_GPP_perf_Bias_mat, plot_GPP_perf_Bias_ai, plot_GPP_perf_Bias_Precip_Anomaly,
  model = GPP_perf_Bias, x_on_top = TRUE, ylim_floor = -0.5
) & theme(legend.position = "right",
          plot.title        = element_text(size = 19, face = "bold"),
          axis.title.x.top  = element_text(size = 19, face = "bold", margin = margin(b = 6)),
          axis.text.x.top   = element_text(size = 19, face = "bold"),
          axis.text.x       = element_text(size = 19, face = "bold")
)


# Show
combined_GPP_perf_Bias_plot


# Save the combined plot as a high-res PNG
ggsave(
  filename = "combined_GPP_perf_Bias_plot_2.png",
  plot = combined_GPP_perf_Bias_plot,
  width = 14,       # in inches
  height = 10,      # adjust as needed
  dpi = 600         # high resolution
)






#### ET_perf_Bias ----

# Plot for MAT
plot_ET_perf_Bias_mat <- plot(ggpredict(ET_perf_Bias, terms = c("MAT"))) +
  geom_line(linewidth = 1.3) +  # <- if needed manually
  labs(x = "Mean Annual Temperature (°C)", y = "ET") + ggtitle(NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y   = element_text(size = 19, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"))



# Repeat for AI
plot_ET_perf_Bias_ai <- plot(ggpredict(ET_perf_Bias, terms = c("AI"))) +
  geom_line(linewidth = 1.3) +
  labs(x = "Aridity Index", y = "ET") + ggtitle(NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



# Repeat for Precipitation Anomaly
plot_ET_perf_Bias_Precip_Anomaly <- plot(ggpredict(ET_perf_Bias, terms = c("Precip_Anomaly_Percent"))) +
  geom_line(linewidth = 1.3) +
  labs(x = "Precipitation Anomaly (%)", y = "ET") + ggtitle(NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



# ET — Bias
plot_ET_perf_Bias_mat            <- add_pval(plot_ET_perf_Bias_mat,            ET_perf_Bias, "MAT",                    show = "stars")
plot_ET_perf_Bias_ai             <- add_pval(plot_ET_perf_Bias_ai,             ET_perf_Bias, "AI",                     show = "stars")
plot_ET_perf_Bias_Precip_Anomaly <- add_pval(plot_ET_perf_Bias_Precip_Anomaly, ET_perf_Bias, "Precip_Anomaly_Percent", show = "stars")





# Combine
combined_ET_perf_Bias_plot <- combine_row(
  plot_ET_perf_Bias_mat, plot_ET_perf_Bias_ai, plot_ET_perf_Bias_Precip_Anomaly,
  model = ET_perf_Bias, x_on_top = TRUE, ylim_floor = -0.5
) & theme(legend.position = "right",
          plot.title        = element_text(size = 19, face = "bold"),
          axis.title.x.top  = element_text(size = 19, margin = margin(b = 6)),
          axis.text.x.top   = element_text(size = 19)
) & no_xaxis_all


# Show
combined_ET_perf_Bias_plot




# Save the combined plot as a high-res PNG
ggsave(
  filename = "combined_ET_perf_Bias_plot_2.png",
  plot = combined_ET_perf_Bias_plot,
  width = 14,       # in inches
  height = 10,      # adjust as needed
  dpi = 600         # high resolution
)







#### Reco_perf_Bias ----

# Plot for MAT
plot_Reco_perf_Bias_mat <- plot(ggpredict(Reco_perf_Bias, terms = c("MAT"))) +
  geom_line(linewidth = 1.3) +  # <- if needed manually
  labs(x = "Mean Annual Temperature (°C)", y = "Reco") + ggtitle(NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y   = element_text(size = 19, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"))





# Repeat for AI
plot_Reco_perf_Bias_ai <- plot(ggpredict(Reco_perf_Bias, terms = c("AI"))) +
  geom_line(linewidth = 1.3) +
  labs(x = "Aridity Index", y = "Reco") + ggtitle(NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())




# Repeat for Precipitation Anomaly
plot_Reco_perf_Bias_Precip_Anomaly <- plot(ggpredict(Reco_perf_Bias, terms = c("Precip_Anomaly_Percent"))) +
  geom_line(linewidth = 1.3) +
  labs(x = "Precipitation Anomaly (%)", y = "Reco") + ggtitle(NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())




# RECO — Bias
plot_Reco_perf_Bias_mat            <- add_pval(plot_Reco_perf_Bias_mat,            Reco_perf_Bias, "MAT",                    show = "stars")
plot_Reco_perf_Bias_ai             <- add_pval(plot_Reco_perf_Bias_ai,             Reco_perf_Bias, "AI",                     show = "stars")
plot_Reco_perf_Bias_Precip_Anomaly <- add_pval(plot_Reco_perf_Bias_Precip_Anomaly, Reco_perf_Bias, "Precip_Anomaly_Percent", show = "stars")




# Combine
combined_Reco_perf_Bias_plot <- combine_row(
  plot_Reco_perf_Bias_mat, plot_Reco_perf_Bias_ai, plot_Reco_perf_Bias_Precip_Anomaly,
  model = Reco_perf_Bias, x_on_top = TRUE, ylim_floor = -0.5
) & theme(legend.position = "right",
          plot.title        = element_text(size = 19, face = "bold"),
          axis.title.x.top  = element_text(size = 19, margin = margin(b = 6)),
          axis.text.x.top   = element_text(size = 19)
) & no_xaxis_all


# Show
combined_Reco_perf_Bias_plot




# Save the combined plot as a high-res PNG
ggsave(
  filename = "combined_Reco_perf_Bias_plot_2.png",
  plot = combined_Reco_perf_Bias_plot,
  width = 14,       # in inches
  height = 10,      # adjust as needed
  dpi = 600         # high resolution
)















## perf_rmse ----

GPP_perf_Rmse <-lmer(performance_rmse ~ MAT + AI + Precip_Anomaly_Percent + (1 | SiteID), data = glmm_GPP)
summary(GPP_perf_Rmse)

ET_perf_Rmse <-lmer(performance_rmse ~ MAT + AI + Precip_Anomaly_Percent + (1 | SiteID), data = glmm_ET)
summary(ET_perf_Rmse)

Reco_perf_Rmse <-lmer(performance_rmse ~ MAT + AI + Precip_Anomaly_Percent + (1 | SiteID), data = glmm_Reco)
summary(Reco_perf_Rmse)



### performance and diagnostic check ----

cm_model_perf_GPP_Rmse <- performance::check_model(GPP_perf_Rmse)

cm_model_perf_ET_Rmse <- performance::check_model(ET_perf_Rmse)

cm_model_perf_Reco_Rmse <- performance::check_model(Reco_perf_Rmse)


plot_cm_model_perf_GPP_Rmse <- plot(cm_model_perf_GPP_Rmse, n_columns = 3)

plot_cm_model_perf_ET_Rmse <- plot(cm_model_perf_ET_Rmse, n_columns = 3)

plot_cm_model_perf_Reco_Rmse <- plot(cm_model_perf_Reco_Rmse, n_columns = 3)



# save
ggplot2::ggsave("model_check_model_perf_GPP_Rmse_3col.png", plot_cm_model_perf_GPP_Rmse,
                width = 18, height = 12, dpi = 600, bg = "white")


# save
ggplot2::ggsave("model_check_model_perf_ET_Rmse_3col.png", plot_cm_model_perf_ET_Rmse,
                width = 18, height = 12, dpi = 600, bg = "white")


# save
ggplot2::ggsave("model_check_model_perf_Reco_Rmse_3col.png", plot_cm_model_perf_Reco_Rmse,
                width = 18, height = 12, dpi = 600, bg = "white")



# 4
tab_model(GPP_perf_Rmse, file = "GLMM_model_GPP_perf_Rmse_summary.html")


tidy(GPP_perf_Rmse, effects = "fixed", conf.int = TRUE) |>
  gt() |>
  tab_header(title = "GLMM Summary: JULES GPP_perf_Rmse across Environmental Gradients") |>
  fmt_number(
    columns = c(estimate, std.error, statistic, df, `p.value`, conf.low, conf.high),
    decimals = 4
  ) |>
  gtsave("GLMM_model_GPP_perf_Rmse_summary.rtf")  # or use .pdf


# 5 
tab_model(ET_perf_Rmse, file = "GLMM_model_ET_perf_Rmse_summary.html")


tidy(ET_perf_Rmse, effects = "fixed", conf.int = TRUE) |>
  gt() |>
  tab_header(title = "GLMM Summary: JULES ET_perf_Rmse across Environmental Gradients") |>
  fmt_number(
    columns = c(estimate, std.error, statistic, df, `p.value`, conf.low, conf.high),
    decimals = 4
  ) |>
  gtsave("GLMM_model_ET_perf_Rmse_summary.rtf")  # or use .pdf


# 6 
tab_model(Reco_perf_Rmse, file = "GLMM_model_Reco_perf_Rmse_summary.html")


tidy(Reco_perf_Rmse, effects = "fixed", conf.int = TRUE) |>
  gt() |>
  tab_header(title = "GLMM Summary: JULES Reco_perf_Rmse across Environmental Gradients") |>
  fmt_number(
    columns = c(estimate, std.error, statistic, df, `p.value`, conf.low, conf.high),
    decimals = 4
  ) |>
  gtsave("GLMM_model_Reco_perf_Rmse_summary.rtf")  # or use .pdf



### plots_perf_rmse ----

#### GPP_perf_Rmse ----

# Plot for MAT
plot_GPP_perf_Rmse_mat <- plot(ggpredict(GPP_perf_Rmse, terms = c("MAT"))) +
  geom_line(linewidth = 1.3) +
  labs(x = "Mean Annual Temperature (°C)", y = "GPP") + ggtitle(NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y   = element_text(size = 19, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"))



# Repeat for AI
plot_GPP_perf_Rmse_ai <- plot(ggpredict(GPP_perf_Rmse, terms = c("AI"))) +
  geom_line(linewidth = 1.3) +
  labs(x = "Aridity Index", y = "GPP") + ggtitle(NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())




# Repeat for Precipitation Anomaly
plot_GPP_perf_Rmse_Precip_Anomaly <- plot(ggpredict(GPP_perf_Rmse, terms = c("Precip_Anomaly_Percent"))) +
  geom_line(linewidth = 1.3) +
  labs(x = "Precipitation Anomaly (%)", y = "GPP") + ggtitle(NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



# GPP — Rmse
plot_GPP_perf_Rmse_mat            <- add_pval(plot_GPP_perf_Rmse_mat,            GPP_perf_Rmse, "MAT",                    show = "stars")
plot_GPP_perf_Rmse_ai             <- add_pval(plot_GPP_perf_Rmse_ai,             GPP_perf_Rmse, "AI",                     show = "stars")
plot_GPP_perf_Rmse_Precip_Anomaly <- add_pval(plot_GPP_perf_Rmse_Precip_Anomaly, GPP_perf_Rmse, "Precip_Anomaly_Percent", show = "stars")





# Combine
combined_GPP_perf_Rmse_plot <- combine_row(
  plot_GPP_perf_Rmse_mat, plot_GPP_perf_Rmse_ai, plot_GPP_perf_Rmse_Precip_Anomaly,
  model = GPP_perf_Rmse, x_on_top = TRUE, ylim_floor = -0.5
) & theme(legend.position = "right",
          plot.title        = element_text(size = 19, face = "bold"),
          axis.title.x.top  = element_text(size = 19, face = "bold", margin = margin(b = 6)),
          axis.text.x.top   = element_text(size = 19, face = "bold"),
          axis.text.x       = element_text(size = 19, face = "bold")
) & no_xaxis_all


# Show
combined_GPP_perf_Rmse_plot


# Save the combined plot as a high-res PNG
ggsave(
  filename = "combined_GPP_perf_Rmse_plot_2.png",
  plot = combined_GPP_perf_Rmse_plot,
  width = 14,       # in inches
  height = 10,      # adjust as needed
  dpi = 600         # high resolution
)






#### ET_perf_Rmse ----

# Plot for MAT
plot_ET_perf_Rmse_mat <- plot(ggpredict(ET_perf_Rmse, terms = c("MAT"))) +
  geom_line(linewidth = 1.3) +  # <- if needed manually
  labs(x = "Mean Annual Temperature (°C)", y = "ET") + ggtitle(NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y   = element_text(size = 19, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"))



# Repeat for AI
plot_ET_perf_Rmse_ai <- plot(ggpredict(ET_perf_Rmse, terms = c("AI"))) +
  geom_line(linewidth = 1.3) +
  labs(x = "Aridity Index", y = "ET") + ggtitle(NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



# Repeat for Precipitation Anomaly
plot_ET_perf_Rmse_Precip_Anomaly <- plot(ggpredict(ET_perf_Rmse, terms = c("Precip_Anomaly_Percent"))) +
  geom_line(linewidth = 1.3) +
  labs(x = "Precipitation Anomaly (%)", y = "ET") + ggtitle(NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



# ET — Rmse
plot_ET_perf_Rmse_mat            <- add_pval(plot_ET_perf_Rmse_mat,            ET_perf_Rmse, "MAT",                    show = "stars")
plot_ET_perf_Rmse_ai             <- add_pval(plot_ET_perf_Rmse_ai,             ET_perf_Rmse, "AI",                     show = "stars")
plot_ET_perf_Rmse_Precip_Anomaly <- add_pval(plot_ET_perf_Rmse_Precip_Anomaly, ET_perf_Rmse, "Precip_Anomaly_Percent", show = "stars")





# Combine
combined_ET_perf_Rmse_plot <- combine_row(
  plot_ET_perf_Rmse_mat, plot_ET_perf_Rmse_ai, plot_ET_perf_Rmse_Precip_Anomaly,
  model = ET_perf_Rmse, x_on_top = TRUE, ylim_floor = -0.5
) & theme(legend.position = "right",
          plot.title        = element_text(size = 19, face = "bold"),
          axis.title.x.top  = element_text(size = 19, margin = margin(b = 6)),
          axis.text.x.top   = element_text(size = 19)
) & no_xaxis_all

# Show
combined_ET_perf_Rmse_plot




# Save the combined plot as a high-res PNG
ggsave(
  filename = "combined_ET_perf_Rmse_plot_2.png",
  plot = combined_ET_perf_Rmse_plot,
  width = 14,       # in inches
  height = 10,      # adjust as needed
  dpi = 600         # high resolution
)







#### Reco_perf_Rmse ----

# Plot for MAT
plot_Reco_perf_Rmse_mat <- plot(ggpredict(Reco_perf_Rmse, terms = c("MAT"))) +
  geom_line(linewidth = 1.3) +  # <- if needed manually
  labs(x = "Mean Annual Temperature (°C)", y = "Reco") + ggtitle(NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y   = element_text(size = 19, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"))





# Repeat for AI
plot_Reco_perf_Rmse_ai <- plot(ggpredict(Reco_perf_Rmse, terms = c("AI"))) +
  geom_line(linewidth = 1.3) +
  labs(x = "Aridity Index", y = "Reco") + ggtitle(NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())




# Repeat for Precipitation Anomaly
plot_Reco_perf_Rmse_Precip_Anomaly <- plot(ggpredict(Reco_perf_Rmse, terms = c("Precip_Anomaly_Percent"))) +
  geom_line(linewidth = 1.3) +
  labs(x = "Precipitation Anomaly (%)", y = "Reco") + ggtitle(NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())




# RECO — Rmse
plot_Reco_perf_Rmse_mat            <- add_pval(plot_Reco_perf_Rmse_mat,            Reco_perf_Rmse, "MAT",                    show = "stars")
plot_Reco_perf_Rmse_ai             <- add_pval(plot_Reco_perf_Rmse_ai,             Reco_perf_Rmse, "AI",                     show = "stars")
plot_Reco_perf_Rmse_Precip_Anomaly <- add_pval(plot_Reco_perf_Rmse_Precip_Anomaly, Reco_perf_Rmse, "Precip_Anomaly_Percent", show = "stars")




# Combine
combined_Reco_perf_Rmse_plot <- combine_row(
  plot_Reco_perf_Rmse_mat, plot_Reco_perf_Rmse_ai, plot_Reco_perf_Rmse_Precip_Anomaly,
  model = Reco_perf_Rmse, x_on_top = TRUE, ylim_floor = -0.5
) & theme(legend.position = "right",
          plot.title        = element_text(size = 19, face = "bold"),
          axis.title.x.top  = element_text(size = 19, margin = margin(b = 6)),
          axis.text.x.top   = element_text(size = 19)
) & no_xaxis_all


# Show
combined_Reco_perf_Rmse_plot




# Save the combined plot as a high-res PNG
ggsave(
  filename = "combined_Reco_perf_Rmse_plot_2.png",
  plot = combined_Reco_perf_Rmse_plot,
  width = 14,       # in inches
  height = 10,      # adjust as needed
  dpi = 600         # high resolution
)











## perf_corr ----
GPP_perf_Corr <-lmer(performance_cor ~ MAT + AI + Precip_Anomaly_Percent + (1 | SiteID), data = glmm_GPP)
summary(GPP_perf_Corr)

ET_perf_Corr <-lmer(performance_cor ~ MAT + AI + Precip_Anomaly_Percent + (1 | SiteID), data = glmm_ET)
summary(ET_perf_Corr)

Reco_perf_Corr <-lmer(performance_cor ~ MAT + AI + Precip_Anomaly_Percent + (1 | SiteID), data = glmm_Reco)
summary(Reco_perf_Corr)


### performance and diagnostic check ----


cm_model_perf_GPP_Corr <- performance::check_model(GPP_perf_Corr)

cm_model_perf_ET_Corr <- performance::check_model(ET_perf_Corr)

cm_model_perf_Reco_Corr <- performance::check_model(Reco_perf_Corr)


plot_cm_model_perf_GPP_Corr <- plot(cm_model_perf_GPP_Corr, n_columns = 3)

plot_cm_model_perf_ET_Corr <- plot(cm_model_perf_ET_Corr, n_columns = 3)

plot_cm_model_perf_Reco_Corr <- plot(cm_model_perf_Reco_Corr, n_columns = 3)




# save
ggplot2::ggsave("model_check_model_perf_GPP_Corr_3col.png", plot_cm_model_perf_GPP_Corr,
                width = 18, height = 12, dpi = 600, bg = "white")


# save
ggplot2::ggsave("model_check_model_perf_ET_Corr_3col.png", plot_cm_model_perf_ET_Corr,
                width = 18, height = 12, dpi = 600, bg = "white")


# save
ggplot2::ggsave("model_check_model_perf_Reco_Corr_3col.png", plot_cm_model_perf_Reco_Corr,
                width = 18, height = 12, dpi = 600, bg = "white")



# 4
tab_model(GPP_perf_Corr, file = "GLMM_model_GPP_perf_Corr_summary.html")


tidy(GPP_perf_Corr, effects = "fixed", conf.int = TRUE) |>
  gt() |>
  tab_header(title = "GLMM Summary: JULES GPP_perf_Corr across Environmental Gradients") |>
  fmt_number(
    columns = c(estimate, std.error, statistic, df, `p.value`, conf.low, conf.high),
    decimals = 4
  ) |>
  gtsave("GLMM_model_GPP_perf_Corr_summary.rtf")  # or use .pdf


# 5 
tab_model(ET_perf_Corr, file = "GLMM_model_ET_perf_Corr_summary.html")


tidy(ET_perf_Corr, effects = "fixed", conf.int = TRUE) |>
  gt() |>
  tab_header(title = "GLMM Summary: JULES ET_perf_Corr across Environmental Gradients") |>
  fmt_number(
    columns = c(estimate, std.error, statistic, df, `p.value`, conf.low, conf.high),
    decimals = 4
  ) |>
  gtsave("GLMM_model_ET_perf_Corr_summary.rtf")  # or use .pdf


# 6 
tab_model(Reco_perf_Corr, file = "GLMM_model_Reco_perf_Corr_summary.html")


tidy(Reco_perf_Corr, effects = "fixed", conf.int = TRUE) |>
  gt() |>
  tab_header(title = "GLMM Summary: JULES Reco_perf_Corr across Environmental Gradients") |>
  fmt_number(
    columns = c(estimate, std.error, statistic, df, `p.value`, conf.low, conf.high),
    decimals = 4
  ) |>
  gtsave("GLMM_model_Reco_perf_Corr_summary.rtf")  # or use .pdf


### plots_perf_corr ----
#### GPP_perf_Corr ----

# Plot for MAT
plot_GPP_perf_Corr_mat <- plot(ggpredict(GPP_perf_Corr, terms = c("MAT"))) +
  geom_line(linewidth = 1.3) +
  labs(x = "Mean Annual Temperature (°C)", y = "GPP") + ggtitle(NULL) + 
  geom_hline(yintercept = 1, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y   = element_text(size = 19, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"))



# Repeat for AI
plot_GPP_perf_Corr_ai <- plot(ggpredict(GPP_perf_Corr, terms = c("AI"))) +
  geom_line(linewidth = 1.3) +
  labs(x = "Aridity Index", y = "GPP") + ggtitle(NULL) + 
  geom_hline(yintercept = 1, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())




# Repeat for Precipitation Anomaly
plot_GPP_perf_Corr_Precip_Anomaly <- plot(ggpredict(GPP_perf_Corr, terms = c("Precip_Anomaly_Percent"))) +
  geom_line(linewidth = 1.3) +
  labs(x = "Precipitation Anomaly (%)", y = "GPP") + ggtitle(NULL) + 
  geom_hline(yintercept = 1, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



# GPP — Corr
plot_GPP_perf_Corr_mat            <- add_pval(plot_GPP_perf_Corr_mat,            GPP_perf_Corr, "MAT",                    show = "stars")
plot_GPP_perf_Corr_ai             <- add_pval(plot_GPP_perf_Corr_ai,             GPP_perf_Corr, "AI",                     show = "stars")
plot_GPP_perf_Corr_Precip_Anomaly <- add_pval(plot_GPP_perf_Corr_Precip_Anomaly, GPP_perf_Corr, "Precip_Anomaly_Percent", show = "stars")





# Combine
combined_GPP_perf_Corr_plot <- combine_row(
  plot_GPP_perf_Corr_mat, plot_GPP_perf_Corr_ai, plot_GPP_perf_Corr_Precip_Anomaly,
  model = GPP_perf_Corr, x_on_top = TRUE
) & theme(legend.position = "right",
          plot.title        = element_text(size = 19, face = "bold"),
          axis.title.x.top  = element_text(size = 19, face = "bold", margin = margin(b = 6)),
          axis.text.x.top   = element_text(size = 19, face = "bold"),
          axis.text.x       = element_text(size = 19, face = "bold")
) & no_xaxis_all


# Show
combined_GPP_perf_Corr_plot


# Save the combined plot as a high-res PNG
ggsave(
  filename = "combined_GPP_perf_Corr_plot_2.png",
  plot = combined_GPP_perf_Corr_plot,
  width = 14,       # in inches
  height = 10,      # adjust as needed
  dpi = 600         # high resolution
)






#### ET_perf_Corr ----

# Plot for MAT
plot_ET_perf_Corr_mat <- plot(ggpredict(ET_perf_Corr, terms = c("MAT"))) +
  geom_line(linewidth = 1.3) +  # <- if needed manually
  labs(x = "Mean Annual Temperature (°C)", y = "ET") + ggtitle(NULL) + 
  geom_hline(yintercept = 1, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y   = element_text(size = 19, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"))



# Repeat for AI
plot_ET_perf_Corr_ai <- plot(ggpredict(ET_perf_Corr, terms = c("AI"))) +
  geom_line(linewidth = 1.3) +
  labs(x = "Aridity Index", y = "ET") + ggtitle(NULL) + 
  geom_hline(yintercept = 1, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



# Repeat for Precipitation Anomaly
plot_ET_perf_Corr_Precip_Anomaly <- plot(ggpredict(ET_perf_Corr, terms = c("Precip_Anomaly_Percent"))) +
  geom_line(linewidth = 1.3) +
  labs(x = "Precipitation Anomaly (%)", y = "ET") + ggtitle(NULL) + 
  geom_hline(yintercept = 1, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



# ET — Corr
plot_ET_perf_Corr_mat            <- add_pval(plot_ET_perf_Corr_mat,            ET_perf_Corr, "MAT",                    show = "stars")
plot_ET_perf_Corr_ai             <- add_pval(plot_ET_perf_Corr_ai,             ET_perf_Corr, "AI",                     show = "stars")
plot_ET_perf_Corr_Precip_Anomaly <- add_pval(plot_ET_perf_Corr_Precip_Anomaly, ET_perf_Corr, "Precip_Anomaly_Percent", show = "stars")





# Combine
combined_ET_perf_Corr_plot <- combine_row(
  plot_ET_perf_Corr_mat, plot_ET_perf_Corr_ai, plot_ET_perf_Corr_Precip_Anomaly,
  model = ET_perf_Corr, x_on_top = FALSE
) & theme(legend.position = "right",
          plot.title        = element_text(size = 19, face = "bold"),
          axis.title.x.top  = element_text(size = 19, margin = margin(b = 6)),
          axis.text.x.top   = element_text(size = 19),
          axis.text.x       = element_text(size = 19, face = "bold"),
          axis.title.x      =element_text(size = 19, face = "bold") 
)


# Show
combined_ET_perf_Corr_plot




# Save the combined plot as a high-res PNG
ggsave(
  filename = "combined_ET_perf_Corr_plot_2.png",
  plot = combined_ET_perf_Corr_plot,
  width = 14,       # in inches
  height = 10,      # adjust as needed
  dpi = 600         # high resolution
)







#### Reco_perf_Corr ----

# Plot for MAT
plot_Reco_perf_Corr_mat <- plot(ggpredict(Reco_perf_Corr, terms = c("MAT"))) +
  geom_line(linewidth = 1.3) +  # <- if needed manually
  labs(x = "Mean Annual Temperature (°C)", y = "Reco") + ggtitle(NULL) + 
  geom_hline(yintercept = 1, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y   = element_text(size = 19, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"))





# Repeat for AI
plot_Reco_perf_Corr_ai <- plot(ggpredict(Reco_perf_Corr, terms = c("AI"))) +
  geom_line(linewidth = 1.3) +
  labs(x = "Aridity Index", y = "Reco") + ggtitle(NULL) + 
  geom_hline(yintercept = 1, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())




# Repeat for Precipitation Anomaly
plot_Reco_perf_Corr_Precip_Anomaly <- plot(ggpredict(Reco_perf_Corr, terms = c("Precip_Anomaly_Percent"))) +
  geom_line(linewidth = 1.3) +
  labs(x = "Precipitation Anomaly (%)", y = "Reco") + ggtitle(NULL) + 
  geom_hline(yintercept = 1, linetype = "dashed", colour = "blue", linewidth = 0.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())




# RECO — Corr
plot_Reco_perf_Corr_mat            <- add_pval(plot_Reco_perf_Corr_mat,            Reco_perf_Corr, "MAT",                    show = "stars")
plot_Reco_perf_Corr_ai             <- add_pval(plot_Reco_perf_Corr_ai,             Reco_perf_Corr, "AI",                     show = "stars")
plot_Reco_perf_Corr_Precip_Anomaly <- add_pval(plot_Reco_perf_Corr_Precip_Anomaly, Reco_perf_Corr, "Precip_Anomaly_Percent", show = "stars")




# Combine
combined_Reco_perf_Corr_plot <- combine_row(
  plot_Reco_perf_Corr_mat, plot_Reco_perf_Corr_ai, plot_Reco_perf_Corr_Precip_Anomaly,
  model = Reco_perf_Corr, x_on_top = TRUE, ylim_floor = -0.5
) & theme(legend.position = "right",
          plot.title        = element_text(size = 19, face = "bold"),
          axis.title.x.top  = element_text(size = 19, margin = margin(b = 6)),
          axis.text.x.top   = element_text(size = 19)
) & no_xaxis_all


# Show
combined_Reco_perf_Corr_plot




# Save the combined plot as a high-res PNG
ggsave(
  filename = "combined_Reco_perf_Corr_plot_2.png",
  plot = combined_Reco_perf_Corr_plot,
  width = 14,       # in inches
  height = 10,      # adjust as needed
  dpi = 600         # high resolution
)










# Combining and saving plots ----

perf_bias_grid <- combined_GPP_perf_Bias_plot / combined_Reco_perf_Bias_plot / combined_ET_perf_Bias_plot
perf_rmse_grid <- combined_GPP_perf_Rmse_plot / combined_Reco_perf_Rmse_plot / combined_ET_perf_Rmse_plot
perf_corr_grid <- combined_GPP_perf_Corr_plot / combined_Reco_perf_Corr_plot / combined_ET_perf_Corr_plot

perf_all_grid <- combined_GPP_perf_Bias_plot / combined_Reco_perf_Bias_plot / combined_ET_perf_Bias_plot / 
  combined_GPP_perf_Rmse_plot / combined_Reco_perf_Rmse_plot / combined_ET_perf_Rmse_plot / 
  combined_GPP_perf_Corr_plot / combined_Reco_perf_Corr_plot / combined_ET_perf_Corr_plot

perf_all_grid_2 <- perf_bias_grid / perf_rmse_grid / perf_corr_grid


perf_bias_block <- combined_GPP_perf_Bias_plot / combined_Reco_perf_Bias_plot / combined_ET_perf_Bias_plot
perf_rmse_block <- combined_GPP_perf_Rmse_plot / combined_Reco_perf_Rmse_plot / combined_ET_perf_Rmse_plot
perf_corr_block <- combined_GPP_perf_Corr_plot / combined_Reco_perf_Corr_plot / combined_ET_perf_Corr_plot



perf_bias_block_labeled <- group_y_label("Predicted Bias") | perf_bias_block
perf_bias_block_labeled <- perf_bias_block_labeled + plot_layout(widths = c(0.08, 1))

perf_rmse_block_labeled <- group_y_label("Predicted RMSE") | perf_rmse_block
perf_rmse_block_labeled <- perf_rmse_block_labeled + plot_layout(widths = c(0.08, 1))

perf_corr_block_labeled <- group_y_label("Predicted Correlation") | perf_corr_block
perf_corr_block_labeled <- perf_corr_block_labeled + plot_layout(widths = c(0.08, 1))



perf_all_grid_3 <- perf_bias_block_labeled / perf_rmse_block_labeled / perf_corr_block_labeled






print(perf_bias_grid)
print(perf_rmse_grid)
print(perf_corr_grid)
print(perf_all_grid)
print(perf_all_grid_2)
print(perf_all_grid_3)






# Save the combined plot as a high-res PNG
ggsave(
  filename = "perf_all_grid.png",
  plot = perf_all_grid,
  width = 18,       # in inches
  height = 25,      # adjust as needed
  dpi = 600         # high resolution
)



# Save the combined plot as a high-res PNG
ggsave(
  filename = "perf_all_grid_2.png",
  plot = perf_all_grid_2,
  width = 18,       # in inches
  height = 25,      # adjust as needed
  dpi = 600         # high resolution
)



# Save the combined plot as a high-res PNG USE THIS!!!
ggsave(
  filename = "perf_all_grid_33.png",
  plot = perf_all_grid_3,
  width = 18,       # in inches
  height = 25,      # adjust as needed
  dpi = 600         # high resolution
)



# Save the combined plot as a high-res PNG
ggsave(
  filename = "perf_bias_grid.png",
  plot = perf_bias_grid,
  width = 18,       # in inches
  height = 16,      # adjust as needed
  dpi = 600         # high resolution
)

# Save the combined plot as a high-res PNG
ggsave(
  filename = "perf_rmse_grid.png",
  plot = perf_rmse_grid,
  width = 18,       # in inches
  height = 16,      # adjust as needed
  dpi = 600         # high resolution
)

# Save the combined plot as a high-res PNG
ggsave(
  filename = "perf_corr_grid.png",
  plot = perf_corr_grid,
  width = 18,       # in inches
  height = 16,      # adjust as needed
  dpi = 600         # high resolution
)



