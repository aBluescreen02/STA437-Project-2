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
  theme(legend.position = "right")

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

# Step 1: Determine number of factors via scree plot + parallel analysis
fa_data <- df %>% select(all_of(gov_cols)) %>% na.omit()

# Parallel analysis data
pa <- fa.parallel(fa_data, fm = "ml", fa = "fa", plot = FALSE)

# Parallel analysis
pa <- fa.parallel(fa_data, fm = "ml", fa = "fa", plot = FALSE)

pa_df <- data.frame(
  factor         = seq_along(pa$fa.values),
  observed       = pa$fa.values,
  simulated_95   = pa$fa.sim,
  simulated_mean = pa$fa.simr
)

n_factors <- sum(pa$fa.values > pa$fa.sim)

# Add cumulative variance to pa_df
pa_df <- pa_df %>%
  mutate(cum_var = cumsum(pmax(observed, 0)) / sum(pmax(observed, 0)) * 100)

# Scale factor: map 0-100% onto eigenvalue axis range
y_max   <- max(pa_df$observed) * 1.1
scale_f <- y_max / 100

ggplot(pa_df, aes(x = factor)) +
  annotate("rect", xmin = 0.5, xmax = n_factors + 0.5,
           ymin = -Inf, ymax = Inf, fill = "steelblue", alpha = 0.1) +
  geom_line(aes(y = observed, color = "Observed eigenvalues"), linewidth = 1) +
  geom_point(aes(y = observed, color = "Observed eigenvalues"), size = 2.5) +
  geom_line(aes(y = simulated_95, color = "95th pctl (null)"),
            linetype = "dashed", linewidth = 0.8) +
  geom_point(aes(y = simulated_95, color = "95th pctl (null)"),
             shape = 17, size = 2) +
  geom_line(aes(y = simulated_mean, color = "Mean (null)"),
            linetype = "dotted", linewidth = 0.8) +
  geom_hline(yintercept = 1, color = "gold3", linetype = "dotted", linewidth = 0.8) +
  # Cumulative variance line on secondary axis
  geom_line(aes(y = cum_var * scale_f, color = "Cumulative Variance"),
            linewidth = 1, linetype = "longdash") +
  geom_point(aes(y = cum_var * scale_f, color = "Cumulative Variance"), size = 2.5) +
  annotate("text", x = max(pa_df$factor), y = 1,
           label = "Kaiser criterion", color = "gold3",
           hjust = 1.1, vjust = -0.5, size = 3.5) +
  annotate("text", x = n_factors + 0.3, y = max(pa_df$observed) * 0.85,
           label = paste(n_factors, "factors retained"),
           color = "steelblue", hjust = -0.1, size = 3.5) +
  scale_color_manual(values = c(
    "Observed eigenvalues" = "steelblue",
    "95th pctl (null)"     = "firebrick2",
    "Mean (null)"          = "grey40",
    "Cumulative Variance"  = "darkorchid"
  )) +
  scale_x_continuous(breaks = seq_along(pa$fa.values)) +
  scale_y_continuous(
    name     = "Eigenvalue",
    sec.axis = sec_axis(~ . / scale_f, name = "Cumulative Variance (%)",
                        breaks = seq(0, 100, 20),
                        labels = paste0(seq(0, 100, 20), "%"))
  ) +
  labs(title = paste0("Parallel Analysis (n = ", nrow(fa_data), ", p = ", length(gov_cols), ")"),
       x = "Component", color = NULL) +
  theme_minimal() +
  theme(plot.title       = element_text(face = "bold", hjust = 0.5),
        legend.position  = "inside",
        legend.position.inside = c(0.72, 0.55),
        axis.title.y.right = element_text(color = "darkorchid"),
        axis.text.y.right  = element_text(color = "darkorchid"))

# Step 2: Fit FA with varimax (orthogonal) and promax (oblique) — compare both
fa_varimax <- fa(fa_data, nfactors = 2, rotate = "varimax", fm = "ml")
fa_promax  <- fa(fa_data, nfactors = 2, rotate = "promax",  fm = "ml")
fa_none    <- fa(fa_data, nfactors = 2, rotate = "none",    fm = "ml")

# Function to convert loadings to long format
loadings_long <- function(fa_model, rotation_name) {
  fa_model$loadings[] %>%
    as.data.frame() %>%
    rownames_to_column("indicator") %>%
    pivot_longer(-indicator, names_to = "factor", values_to = "loading") %>%
    mutate(rotation = rotation_name)
}

# Combine all three
all_loadings <- bind_rows(
  loadings_long(fa_none,    "No Rotation"),
  loadings_long(fa_varimax, "Varimax"),
  loadings_long(fa_promax,  "Promax")
) %>%
  mutate(rotation = factor(rotation, levels = c("No Rotation", "Varimax", "Promax")))

# Side-by-side heatmaps
ggplot(all_loadings, aes(x = factor, y = indicator, fill = loading)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(loading, 2)), size = 3.5, fontface = "bold") +
  scale_fill_gradient2(low  = "steelblue", mid = "white", high = "firebrick2",
                       midpoint = 0, limits = c(-1, 1), name = "Loading") +
  facet_wrap(~ rotation, ncol = 3) +
  labs(title = "Factor Loadings by Rotation Method",
       x = "Factor", y = NULL) +
  theme_minimal() +
  theme(plot.title      = element_text(face = "bold", hjust = 0.5),
        strip.text      = element_text(face = "bold", size = 11),
        axis.text.y     = element_text(size = 10),
        legend.position = "right")

# Extract promax factor scores and join region
fa_scores <- as.data.frame(fa_promax$scores) %>%
  rename(Factor1 = ML1, Factor2 = ML2)

df_fa <- df %>%
  na.omit() %>%
  bind_cols(fa_scores)

# Compute regional centroids
region_centroids <- df_fa %>%
  group_by(region) %>%
  summarise(Factor1 = mean(Factor1, na.rm = TRUE),
            Factor2 = mean(Factor2, na.rm = TRUE))

# Biplot be region
ggplot() +
  geom_point(data = df_fa,
             aes(x = Factor1, y = Factor2, color = region),
             alpha = 0.3, size = 1) +
  geom_point(data = region_centroids,
             aes(x = Factor1, y = Factor2, color = region),
             size = 5, shape = 18) +
  geom_label(data = region_centroids,
             aes(x = Factor1, y = Factor2, label = region, color = region),
             size = 2.5, fontface = "bold", nudge_y = 0.15,
             label.padding = unit(0.15, "lines"), show.legend = FALSE) +
  scale_color_brewer(palette = "Set1") +
  labs(title    = "Factor Score Biplot by Region (Promax Rotation)",
       x        = "Factor 1 — State Capacity",
       y        = "Factor 2 — Political Legitimacy & Civil Freedom",
       color    = "Region") +
  theme_minimal() +
  theme(plot.title      = element_text(face = "bold", hjust = 0.5),
        legend.position = "right")

# Individual biplots by region over time #####

# Compute early and recent centroids per region in factor space
early_scores <- as.data.frame(fa(df %>%
                                   filter(year >= 1996 & year <= 2000) %>%
                                   na.omit() %>%
                                   select(all_of(gov_cols)),
                                 nfactors = 2, rotate = "promax", fm = "ml")$scores) %>%
  rename(Factor1 = ML1, Factor2 = ML2)

recent_scores <- as.data.frame(fa(df %>%
                                    filter(year >= 2019 & year <= 2023) %>%
                                    na.omit() %>%
                                    select(all_of(gov_cols)),
                                  nfactors = 2, rotate = "promax", fm = "ml")$scores) %>%
  rename(Factor1 = ML1, Factor2 = ML2)

early_centroids <- df %>%
  filter(year >= 1996 & year <= 2000) %>%
  na.omit() %>%
  bind_cols(early_scores) %>%
  group_by(region) %>%
  summarise(Factor1 = mean(Factor1), Factor2 = mean(Factor2)) %>%
  mutate(window = "1996–2000")

recent_centroids <- df %>%
  filter(year >= 2019 & year <= 2023) %>%
  na.omit() %>%
  bind_cols(recent_scores) %>%
  group_by(region) %>%
  summarise(Factor1 = mean(Factor1), Factor2 = mean(Factor2)) %>%
  mutate(window = "2019–2023")

window_centroids <- bind_rows(early_centroids, recent_centroids)

# Biplot faceted by region with trajectories + window centroids
ggplot(df_fa, aes(x = Factor1, y = Factor2, color = year)) +
  geom_point(alpha = 0.6, size = 1) +
  geom_path(aes(group = country_code), alpha = 0.3, linewidth = 0.3) +
  # Window centroids
  geom_point(data = window_centroids,
             aes(x = Factor1, y = Factor2, shape = window),
             color = "black", size = 4) +
  geom_line(data = window_centroids,
            aes(x = Factor1, y = Factor2, group = region),
            color = "black", linewidth = 0.8, linetype = "dashed",
            arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
  facet_wrap(~ region, ncol = 3) +
  scale_color_gradient(low = "firebrick2", high = "steelblue") +
  scale_shape_manual(values = c("1996–2000" = 15, "2019–2023" = 17)) +
  labs(title  = "Governance Trajectory by Region (Promax Factor Space)",
       x      = "Factor 1 — State Capacity",
       y      = "Factor 2 — Political Legitimacy & Civil Freedom",
       color  = "Year", shape = "Window") +
  theme_minimal() +
  theme(plot.title      = element_text(face = "bold", hjust = 0.5),
        legend.position = "right",
        strip.text      = element_text(face = "bold", size = 9))
