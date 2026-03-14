library(tidyverse)

df <- read.csv('governance_data.csv')
gov_cols <- c('control_corruption', 'govt_effectiveness', 'political_stability',
              'regulatory_quality', 'rule_of_law', 'voice_accountability')
X <- df[, gov_cols]

cat("Observations:", nrow(df), "\n")
cat("Countries:", length(unique(df$country)), "\n")
cat("Year range:", min(df$year), "-", max(df$year), "\n")

# Per country time series analysis #####

country_list <- df %>% split(.$country_code)

plot_country <- function(code) {
  country_list[[code]] %>%
    pivot_longer(cols = all_of(gov_cols), names_to = "group", values_to = "score") %>%
    ggplot(aes(x = year, y = score, color = group)) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.5) +
    labs(title = paste(code, "Governance Indicators Over Time"),
         x = "Year", y = "Score", color = "Indicator") +
    scale_color_brewer(palette = "Dark2") +
    theme_minimal() +
    theme(legend.position = "bottom")
}

# Usage
plot_country("CAN")
plot_country("JPN")

# Count missing countries by year
all_countries <- unique(df$country_code)

missing_by_year <- df %>%
  group_by(year) %>%
  summarise(
    countries_present  = n_distinct(country_code),
    missing_count      = length(all_countries) - n_distinct(country_code),
    missing_countries  = list(setdiff(all_countries, country_code))
  ) %>%
  arrange(year)

missing_by_year

# 1996-2000 window
country_scores_early <- df %>%
  filter(year >= 1996 & year <= 2000) %>%
  group_by(country, country_code) %>%
  summarise(across(all_of(gov_cols), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(composite_score_early = rowSums(across(all_of(gov_cols)), na.rm = TRUE))

# 2019-2023 window
country_scores_recent <- df %>%
  filter(year >= 2019 & year <= 2023) %>%
  group_by(country, country_code) %>%
  summarise(across(all_of(gov_cols), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(composite_score_recent = rowSums(across(all_of(gov_cols)), na.rm = TRUE))

# Join both windows: keep all gov_col means from both periods + composite diff only
country_scores_combined <- country_scores_early %>%
  rename_with(~ paste0(.x, "_early"), all_of(gov_cols)) %>%
  left_join(
    country_scores_recent %>%
      rename_with(~ paste0(.x, "_recent"), all_of(gov_cols)) %>%
      select(country_code, ends_with("_recent"), composite_score_recent),
    by = "country_code"
  ) %>%
  mutate(score_diff = composite_score_recent - composite_score_early) %>%
  arrange(desc(score_diff))

# Plotting #####

# Pivot both windows to long format for plotting
early_long <- country_scores_early %>%
  pivot_longer(cols = all_of(gov_cols), names_to = "indicator", values_to = "score") %>%
  mutate(window = "1996-2000")

recent_long <- country_scores_recent %>%
  pivot_longer(cols = all_of(gov_cols), names_to = "indicator", values_to = "score") %>%
  mutate(window = "2019-2023")

# Combine both windows
both_windows <- bind_rows(early_long, recent_long)

# Overlapping boxplots by indicator
ggplot(both_windows, aes(x = indicator, y = score, fill = window)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.8, position = position_dodge(0.8)) +
  scale_fill_manual(values = c("1996-2000" = "steelblue", "2019-2023" = "tomato")) +
  scale_x_discrete(labels = c(
    "control_corruption"   = "Control of\nCorruption",
    "govt_effectiveness"   = "Govt\nEffectiveness",
    "political_stability"  = "Political\nStability",
    "regulatory_quality"   = "Regulatory\nQuality",
    "rule_of_law"          = "Rule\nof Law",
    "voice_accountability" = "Voice &\nAccountability"
  )) +
  labs(
    title = "Governance Indicators: 1996–2000 vs 2019–2023",
    x     = "Indicator",
    y     = "Score",
    fill  = "Year Window"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
