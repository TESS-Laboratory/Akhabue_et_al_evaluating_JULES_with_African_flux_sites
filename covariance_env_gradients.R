# Load packages ----
library(dplyr)
library(ggplot2)
library(GGally)   
library(knitr)
library(patchwork)
library(tidyverse)


# Summary of covariance between MAP vs. MAT, MAP vs. AI, and MAT vs. AI ----

# read environmental gradient data
env_data <- read_csv("MAP_MAT_AI_1991_2020.csv", show_col_types = FALSE)


## Correlation table ---- 
pairs_list <- list(
  c("MAT", "AI")
)

get_cor <- function(vars) {
  x <- env_data[[vars[1]]]
  y <- env_data[[vars[2]]]
  test <- cor.test(x, y, method = "pearson")
  data.frame(
    Var1 = vars[1],
    Var2 = vars[2],
    r = round(test$estimate, 2),
    p_value = signif(test$p.value, 3)
  )
}

cor_table <- do.call(rbind, lapply(pairs_list, get_cor))
print(cor_table)



## Scatter plot matrix ----
p <- ggpairs(
  env_data[, c("MAT", "AI")],
  lower = list(continuous = "smooth"),   # adds loess smoothing lines
  upper = list(continuous = "cor"),      # shows correlation in upper triangle
  diag = list(continuous = "barDiag")    # histograms on diagonal
) +
  theme_bw(base_size = 12)


ggsave("site_env_correlations2.png", plot = p, width = 8, height = 6, dpi = 300, bg = "white")


# Save to CSV for Supplementary Information
write.csv(cor_table, "site_level_env_correlations2.csv", row.names = FALSE)




### Gather data into long format for plotting ----
pairs_data <- env_data %>%
  select(MAT, AI) %>%
  mutate(Site = row.names(env_data)) %>%
  tidyr::pivot_longer(cols = c(MAT, AI), names_to = "VarX", values_to = "ValueX")


p3 <- ggplot(env_data, aes(x = AI, y = MAT)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(x = "Aridity Index", y = "Mean Annual Temperature (°C)") +
  theme_bw()

# Combine
final_plot <- p1 + p2 + p3 +
  plot_layout(ncol = 2, nrow = 2, guides = "collect", heights = c(1, 1)) +
  plot_annotation(
    tag_levels = 'A',
    theme = theme(
      plot.tag = element_text(size = 16, face = "bold")
    )
  )

# View plot
final_plot

# Save as PNG and PDF
ggsave("env_gradients_pairs_corrected2.png", p3, width = 8, height = 6, dpi = 300, bg = "white")
