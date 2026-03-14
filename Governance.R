library(tidyverse)
library(readxl)

df <- read.csv('governance_data.csv')

df <- df %>%
  rename(
    "Control of Corruption"  = control_corruption,
    "Gov't Effectiveness"    = govt_effectiveness,
    "Political Stability"    = political_stability,
    "Regulatory Quality"     = regulatory_quality,
    "Rule of Law"            = rule_of_law,
    "Voice & Accountability" = voice_accountability
  )

# Update gov_cols to match new names
gov_cols <- c("Control of Corruption", "Gov't Effectiveness", "Political Stability",
              "Regulatory Quality", "Rule of Law", "Voice & Accountability")

cat("Observations:", nrow(df), "\n")
cat("Countries:", length(unique(df$country)), "\n")
cat("Year range:", min(df$year), "-", max(df$year), "\n")

# Load region mapping
region_map <- read_excel("region_reference.xlsx", sheet = "Region Reference")

# Join into your main dataframe
df <- df %>% left_join(region_map %>% select(country_code, region), by = "country_code")

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
plot_country("USA")

# Parallel Coordinates Plot

country_means_time <- df %>%
  pivot_longer(cols = all_of(gov_cols), names_to = "indicator", values_to = "score") %>%
  group_by(country_code, region, year, indicator) %>%
  summarise(score = mean(score, na.rm = TRUE), .groups = "drop")

region_means_time <- country_means_time %>%
  group_by(region, year, indicator) %>%
  summarise(score = mean(score, na.rm = TRUE), .groups = "drop")

ggplot() +
  geom_ribbon(data = country_means_time %>%
                group_by(region, year, indicator) %>%
                summarise(lower = quantile(score, 0.25, na.rm = TRUE),
                          upper = quantile(score, 0.75, na.rm = TRUE),
                          .groups = "drop"),
              aes(x = year, ymin = lower, ymax = upper, fill = region),
              alpha = 0.2) +
  geom_line(data = region_means_time,
            aes(x = year, y = score, color = region, group = region),
            linewidth = 1.2) +
  facet_wrap(~ indicator, ncol = 3) +
  scale_color_brewer(palette = "Set3") +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Governance Indicators Over Time by Region",
       x = "Year", y = "Score", color = "Region", fill = "Region") +
  theme_minimal() +
  theme(legend.position = "bottom")

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
  group_by(country, country_code, region) %>%
  summarise(across(all_of(gov_cols), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(composite_score_early = rowSums(across(all_of(gov_cols)), na.rm = TRUE))

# 2019-2023 window
country_scores_recent <- df %>%
  filter(year >= 2019 & year <= 2023) %>%
  group_by(country, country_code, region) %>%
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
  scale_x_discrete() +
  labs(
    title = "Governance Indicators: 1996–2000 vs 2019–2023",
    x     = "Indicator",
    y     = "Score",
    fill  = "Year Window"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Regional means #####

region_means <- df %>%
  group_by(region) %>%
  summarise(across(all_of(gov_cols), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(composite_score = rowSums(across(all_of(gov_cols)), na.rm = TRUE)) %>%
  arrange(desc(composite_score))

# Comparison to motivate regional means

df %>%
  pivot_longer(cols = all_of(gov_cols), names_to = "indicator", values_to = "score") %>%
  ggplot(aes(x = indicator, y = score, fill = indicator)) +
  geom_boxplot(alpha = 0.8, outlier.size = 0.8, show.legend = FALSE) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(title = "Distribution of Governance Indicators",
       x = NULL, y = "Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8))

region_means %>%
  pivot_longer(cols = all_of(gov_cols), names_to = "indicator", values_to = "mean_score") %>%
  ggplot(aes(x = indicator, y = reorder(region, mean_score, FUN = mean), fill = mean_score)) +
  geom_tile() +
  geom_text(aes(label = round(mean_score, 2)), size = 3) +
  scale_fill_gradient2(low = "firebrick2", mid = "white", high = "steelblue", midpoint = 0) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(title = "Mean Governance Score by Region and Indicator",
       x = NULL, y = NULL, fill = "Mean Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8))

# Region mean boxplots

# Plot 1: Side-by-side early vs recent composite by region
country_scores_combined %>%
  select(country, country_code, region, composite_score_early, composite_score_recent) %>%
  pivot_longer(cols = c(composite_score_early, composite_score_recent),
               names_to = "window", values_to = "composite") %>%
  mutate(window = recode(window,
                         "composite_score_early"  = "1996–2000",
                         "composite_score_recent" = "2019–2023"
  )) %>%
  ggplot(aes(x = reorder(region, composite, FUN = median), y = composite, fill = window)) +
  geom_boxplot(alpha = 0.8, outlier.size = 0.8, position = position_dodge(0.8)) +
  geom_jitter(aes(group = window), position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8),
              size = 0.7, alpha = 0.4) +
  coord_flip() +
  scale_fill_manual(values = c("1996–2000" = "steelblue", "2019–2023" = "tomato")) +
  labs(title = "Composite Governance Score by Region: Early vs Recent",
       x = NULL, y = "Composite Score", fill = "Window") +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

# Plot 2: Score difference (recent - early) by region
country_scores_combined %>%
  ggplot(aes(x = reorder(region, score_diff, FUN = median), y = score_diff, fill = region)) +
  geom_boxplot(alpha = 0.8, outlier.size = 0.8, show.legend = FALSE) +
  geom_jitter(width = 0.15, size = 0.8, alpha = 0.4, show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.6) +
  coord_flip() +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Change in Composite Score by Region",
       x = NULL, y = "Score Difference") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

# Plot 3: Overall composite (all years) by region
df %>%
  mutate(composite = rowSums(across(all_of(gov_cols)), na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(region, composite, FUN = median), y = composite, fill = region)) +
  geom_violin(alpha = 0.8, trim = FALSE, show.legend = FALSE) +
  geom_boxplot(width = 0.1, alpha = 0.4, outlier.shape = NA, show.legend = FALSE) +
  coord_flip() +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Overall Composite Governance Score by Region (1996–2023)",
       x = NULL, y = "Composite Score") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

# Correlation plots

library(ggcorrplot)

cor_matrix <- df %>%
  select(all_of(gov_cols)) %>%
  cor(use = "complete.obs")

# All data corr plot
ggcorrplot(cor_matrix,
           method   = "square",
           type     = "full",        
           lab      = TRUE,
           lab_size = 4) +
  scale_x_discrete(limits = rev, guide = guide_axis(n.dodge = 2)) +
  scale_fill_gradient2(name = "Pearson\nCorrelation",
                       low  = "steelblue",
                       mid  = "white",
                       high = "firebrick2",
                       midpoint = 0,
                       limits = c(-1, 1)) +
  theme(axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 10))

# Compute correlation matrix per region
region_cor_plots <- df %>%
  group_by(region) %>%
  group_map(~ {
    cor_m <- .x %>%
      select(all_of(gov_cols)) %>%
      cor(use = "complete.obs")
    
    ggcorrplot(cor_m,
               method   = "square",
               type     = "full",
               lab      = TRUE,
               lab_size = 2) +
      scale_x_discrete(limits = rev, guide = guide_axis(n.dodge = 2)) +
      scale_fill_gradient2(name = "Pearson\nCorrelation",
                           low  = "steelblue",
                           mid  = "white",
                           high = "firebrick2",
                           midpoint = 0,
                           limits = c(-1, 1)) +
      labs(title = .y$region) +
      theme(axis.text.x = element_text(size = 5, angle = 0, hjust = 0.5),
            axis.text.y = element_text(size = 5),
            plot.title  = element_text(size = 8, face = "bold"),
            legend.position = "none")
  }, .keep = TRUE)

# Arrange all 9 regions in a grid
library(patchwork)
wrap_plots(region_cor_plots, ncol = 3) +
  plot_annotation(title = "Correlation Between Governance Indicators by Region",
                  theme = theme(plot.title = element_text(face = "bold", size = 13)))
  
# Normality Testing #####

# library(MVN)
# mvn(df %>% select(all_of(gov_cols)), mvnTest = "mardia")
# mvn(df %>% select(all_of(gov_cols)), mvnTest = "hz")
# 
# mvn(df %>% select(all_of(gov_cols)), mvnTest = "mardia", multivariatePlot = "qq")

library(DescTools)

# Negentropy per indicator (approximation via kurtosis)
neg_entropy <- df %>%
  select(all_of(gov_cols)) %>%
  summarise(across(everything(), ~ (1/12) * mean((scale(.x))^3, na.rm = TRUE)^2 +
                     (1/48) * (mean((scale(.x))^4, na.rm = TRUE) - 3)^2)) %>%
  pivot_longer(everything(), names_to = "indicator", values_to = "negentropy") %>%
  arrange(desc(negentropy))

neg_entropy

# Sampling Adequecy #####

library(psych)

# Kaiser-Meyer-Olkin (KMO) test
KMO(df %>% select(all_of(gov_cols)) %>% na.omit())

# Bartlett's test of sphericity
cortest.bartlett(cor_matrix, n = nrow(df %>% na.omit()))

# FA #####

