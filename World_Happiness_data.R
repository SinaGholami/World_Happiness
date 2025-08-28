library(tidyverse)
library(readr)
library(countrycode)
library(broom)
library(ggcorrplot)

happiness_data <- read_csv("world-happiness-report-updated_2024.csv", show_col_types = FALSE)

df_latest <- happiness_data |>
  group_by('Country name') |>
  slice_max(order_by = year, n = 138, with_ties = FALSE) |>
  ungroup()

# Map continents
df_latest <- df_latest |>
  mutate(continent = countrycode(`Country name`, "country.name", "continent"))

df_clean <- df_latest |>
  drop_na(`Log GDP per capita`, `Life Ladder`)

stats <- df_clean |>
  summarise(
    n      = n(),
    mean   = mean(`Life Ladder`),
    median = median(`Life Ladder`),
    p25    = quantile(`Life Ladder`, 0.25),
    p75    = quantile(`Life Ladder`, 0.75)
  )

mean_x   <- stats$mean
median_x <- stats$median
p25_x    <- stats$p25
p75_x    <- stats$p75
n_c      <- stats$n
# histogram with density + annotations
p_dist <- ggplot(df_clean, aes(x = `Life Ladder`)) +
  # histogram in density scale for nicer overlay
  geom_histogram(aes(y = after_stat(density)),
                 bins = 30, fill = "#4C78A8", color = "white", alpha = 0.9) +
  # density curve
  geom_density(linewidth = 1, alpha = 0.4) +
  annotate("rect", xmin = p25_x, xmax = p75_x, ymin = -Inf, ymax = Inf,
           fill = "#4C78A8", alpha = 0.15) +
  geom_vline(xintercept = median_x, linetype = "dashed", linewidth = 1.1) +
  geom_vline(xintercept = mean_x,   linetype = "dotdash", linewidth = 1.1) +
  labs(
    title = "Distribution of Happiness 2023",
    subtitle = paste0("n = ", n_c,
                      " countries • median = ", round(median_x, 2),
                      " • mean = ", round(mean_x, 2),
                      " • IQR = [", round(p25_x, 2), ", ", round(p75_x, 2), "]"),
    x = "Happiness Score (Life Ladder)",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    plot.title   = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle= element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
  )

print(p_dist)
ggsave(file.path(outdir, "01_distribution_happiness.png"), p_dist, width = 8, height = 5, dpi = 150)

# PLOT 2 — GDP vs Happiness (scatter + trend)

# Pearson correlation for annotation
cor_val <- cor(df_clean$`Log GDP per capita`, df_clean$`Life Ladder`, use = "complete.obs")

p_scatter <- ggplot(df_clean, aes(x = `Log GDP per capita`, y = `Life Ladder`)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  annotate("text", x = min(df_clean$`Log GDP per capita`, na.rm = TRUE),
           y = max(df_latest$`Life Ladder`, na.rm = TRUE),
           label = paste0("Pearson r = ", round(cor_val, 2)),
           hjust = 0, vjust = 1, fontface = "bold") +
  labs(title = "GDP vs Happiness — 2023",
       x = "Log GDP per Capita", y = "Happiness Score") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
print(p_scatter)
ggsave(file.path(outdir, "02_scatter_gdp_vs_happiness.png"), p_scatter, width = 8, height = 6, dpi = 150)


# PLOT 3 — Happiness by Continent (boxplot)

p_box <- df_clean |>
  filter(!is.na(continent)) |>
  ggplot(aes(x = continent, y = `Life Ladder`, fill = continent)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 23, outlier.fill = "white") +
  labs(
    title = "Happiness by Continent — 2023",
    subtitle = "Boxplot of Life Ladder scores (median, IQR, and outliers)",
    x = "Continent",
    y = "Happiness Score"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    legend.position = "none"
  )

print(p_box)
ggsave(file.path(outdir, "03_boxplot_continent_pretty.png"), p_box, width = 8, height = 5, dpi = 150)


# PLOT 4 — Correlation heatmap (selected variables)

corr_vars <- df_clean |>
  select(`Life Ladder`,
         `Log GDP per capita`,
         `Social support`,
         `Healthy life expectancy at birth`,
         `Freedom to make life choices`,
         Generosity,
         `Perceptions of corruption`) |>
  drop_na()

corr_mat <- cor(corr_vars, use = "complete.obs")
p_heat <- ggcorrplot(corr_mat, method = c("square"), ggtheme = theme_minimal(),
                     digits = 1, tl.cex = 8, lab = TRUE, type = "upper", show.diag = TRUE,
                     title = "Correlation Heatmap 2023") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
print(p_heat)
ggsave(file.path(outdir, "04_correlation_heatmap.png"), p_heat, width = 7.5, height = 6.5, dpi = 150)


# MODEL — Linear regression (standardized coeffs)

# Build a clean modeling frame
model_df <- df_clean |>
  select(`Life Ladder`,
         `Log GDP per capita`,
         `Social support`,
         `Healthy life expectancy at birth`,
         `Freedom to make life choices`,
         Generosity,
         `Perceptions of corruption`) |>
  drop_na()

# Standardize predictors for comparable coefficients
model_df_z <- model_df |>
  mutate(across(-`Life Ladder`, scale))  # standardize X only

lm_fit <- lm(`Life Ladder` ~ ., data = model_df_z)
model_sum <- glance(lm_fit)      # R^2 etc.
model_tidy <- tidy(lm_fit, conf.int = TRUE) |>
  filter(term != "(Intercept)") |>
  arrange(desc(estimate))

print(model_sum)
print(model_tidy)

# PLOT 5 — Coefficients bar chart (with 95% CI)
p_coef <- model_tidy |>
  mutate(term = str_replace_all(term, "_", " ")) |>
  ggplot(aes(x = reorder(term, estimate), y = estimate)) +
  geom_col(alpha = 0.9) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() +
  labs(title = paste0("Standardized coefficients (R² = ", round(model_sum$r.squared, 2), ")"),
       x = "Predictors", y = "Std. Coefficient") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
print(p_coef)
ggsave(file.path(outdir, "05_coefficients_bar.png"), p_coef, width = 8, height = 6, dpi = 150)
