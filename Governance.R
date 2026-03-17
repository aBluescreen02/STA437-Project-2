library(tidyverse)
library(readxl)
library(ggcorrplot)
library(patchwork)
library(psych)
library(DescTools)
library(maps)
library(MVN)
library(cowplot)

# ── 1. DATA LOADING & PREPARATION ── #####

region_map <- read_excel("region_reference.xlsx", sheet = "Region Reference")

df <- read.csv('governance_data.csv') %>%
  rename(
    "Control of Corruption"  = control_corruption,
    "Gov't Effectiveness"    = govt_effectiveness,
    "Political Stability"    = political_stability,
    "Regulatory Quality"     = regulatory_quality,
    "Rule of Law"            = rule_of_law,
    "Voice & Accountability" = voice_accountability
  ) %>%
  left_join(
    read_excel("region_reference.xlsx", sheet = "Region Reference") %>%
      select(country_code, region),
    by = "country_code"
  )

df %>%
  distinct(country_code, region) %>%
  count(region, name = "n_countries") %>%
  arrange(desc(n_countries))

# Get world map data
world_map <- map_data("world") %>%
  # Standardize country names to match region_map
  mutate(region = recode(region,
                         "USA"                          = "United States",
                         "UK"                           = "United Kingdom",
                         "South Korea"                  = "Korea, Rep.",
                         "North Korea"                  = "Korea, Dem. People's Rep.",
                         "Russia"                       = "Russian Federation",
                         "Iran"                         = "Iran, Islamic Rep.",
                         "Syria"                        = "Syrian Arab Republic",
                         "Venezuela"                    = "Venezuela, RB",
                         "Yemen"                        = "Yemen, Rep.",
                         "Egypt"                        = "Egypt, Arab Rep.",
                         "Democratic Republic of the Congo" = "Congo, Dem. Rep.",
                         "Republic of Congo"            = "Congo, Rep.",
                         "Ivory Coast"                  = "Cote d'Ivoire",
                         "Tanzania"                     = "Tanzania",
                         "Laos"                         = "Lao PDR",
                         "Vietnam"                      = "Viet Nam",
                         "Taiwan"                       = "Taiwan, China",
                         "Kyrgyzstan"                   = "Kyrgyz Republic",
                         "Slovakia"                     = "Slovak Republic",
                         "Czechia"                      = "Czechia",
                         "Turkey"                       = "Turkiye",
                         "Gambia"                       = "Gambia, The",
                         "Bahamas"                      = "Bahamas, The",
                         "Micronesia"                   = "Micronesia, Fed. Sts.",
                         "Bolivia"                      = "Bolivia",
                         "Trinidad"                     = "Trinidad and Tobago",
                         "Tobago"                       = "Trinidad and Tobago"
  ))

# Join region classifications
world_regions <- world_map %>%
  left_join(
    region_map %>% select(country = country, region_group = region),
    by = c("region" = "country")
  )

# Region color palette
region_colors <- c(
  "North America"                 = "#4E79A7",
  "Central America & Caribbean"   = "#76B7B2",
  "South America"                 = "#59A14F",
  "Western Europe"                = "#F28E2B",
  "Eastern Europe & Central Asia" = "#E15759",
  "Middle East & North Africa"    = "#B07AA1",
  "Sub-Saharan Africa"            = "#FF9DA7",
  "South Asia"                    = "#9C755F",
  "East Asia & Pacific"           = "#BAB0AC"
)

ggplot(world_regions, aes(x = long, y = lat, group = group, fill = region_group)) +
  geom_polygon(color = "white", linewidth = 0.1) +
  scale_fill_manual(values  = region_colors,
                    na.value = "grey85",
                    name    = "Region") +
  coord_fixed(1) +
  labs() +
  theme_void() +
  theme(
    plot.title      = element_text(face = "bold", hjust = 0.5, size = 13),
    legend.position = "bottom",
    legend.title    = element_text(face = "bold", size = 14),
    legend.text     = element_text(size = 14)
  ) +
  guides(fill = guide_legend(nrow = 3))

# ggsave("region_map.png", width = 16, height = 9, dpi = 300)

gov_cols <- c("Control of Corruption", "Gov't Effectiveness", "Political Stability",
              "Regulatory Quality", "Rule of Law", "Voice & Accountability")

cat("Observations:", nrow(df), "\n")
cat("Countries:",    length(unique(df$country)), "\n")
cat("Year range:",   min(df$year), "-", max(df$year), "\n")

# ── 2. TIME WINDOW AGGREGATION ── #####

country_scores_early <- df %>%
  filter(year >= 1996 & year <= 2000) %>%
  group_by(country, country_code, region) %>%
  summarise(across(all_of(gov_cols), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(composite_score_early = rowSums(across(all_of(gov_cols)), na.rm = TRUE))

country_scores_recent <- df %>%
  filter(year >= 2019 & year <= 2023) %>%
  group_by(country, country_code, region) %>%
  summarise(across(all_of(gov_cols), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(composite_score_recent = rowSums(across(all_of(gov_cols)), na.rm = TRUE))

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

# ── 3. DATA INSPECTION ── #####

all_countries <- unique(df$country_code)

missing_by_year <- df %>%
  group_by(year) %>%
  summarise(
    countries_present = n_distinct(country_code),
    missing_count     = length(all_countries) - n_distinct(country_code),
    missing_countries = list(setdiff(all_countries, country_code))
  ) %>%
  arrange(year)

missing_by_year

# ── 4. EXPLORATORY DATA ANALYSIS ── #####

# 4a. Regional composite boxplots
region_order <- df %>%
  mutate(composite = rowSums(across(all_of(gov_cols)), na.rm = TRUE)) %>%
  group_by(region) %>%
  summarise(median_composite = median(composite, na.rm = TRUE)) %>%
  arrange(median_composite) %>%
  pull(region)

p_early_vs_recent_box <- country_scores_combined %>%
  select(country, country_code, region, composite_score_early, composite_score_recent) %>%
  pivot_longer(cols = c(composite_score_early, composite_score_recent),
               names_to = "window", values_to = "composite") %>%
  mutate(region = factor(region, levels = region_order),
         window = recode(window,
                         "composite_score_early"  = "1996–2000",
                         "composite_score_recent" = "2019–2023")) %>%
  ggplot(aes(x = region, y = composite, fill = window)) +
  geom_boxplot(alpha = 0.8, outlier.size = 0.8, position = position_dodge(0.8)) +
  geom_jitter(aes(group = window),
              position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8),
              size = 0.7, alpha = 0.4) +
  scale_fill_manual(values = c("1996–2000" = "steelblue", "2019–2023" = "tomato")) +
  labs(title = "Early vs Recent",
       x = NULL, y = "Composite Score", fill = "Window") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5))

p_violin <- df %>%
  mutate(composite = rowSums(across(all_of(gov_cols)), na.rm = TRUE),
         region = factor(region, levels = region_order)) %>%
  ggplot(aes(x = region, y = composite, fill = region)) +
  geom_violin(alpha = 0.8, trim = FALSE, show.legend = FALSE) +
  geom_boxplot(width = 0.1, alpha = 0.4, outlier.shape = NA, show.legend = FALSE) +
  scale_fill_brewer(palette = "Set3") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(title = "1996–2023",
       x = NULL, y = "Composite Score") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# 4b. Regional mean heatmap
region_means <- df %>%
  group_by(region) %>%
  summarise(across(all_of(gov_cols), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(composite_score = rowSums(across(all_of(gov_cols)), na.rm = TRUE)) %>%
  arrange(desc(composite_score))

p_region_means <- region_means %>%
  pivot_longer(cols = all_of(gov_cols), names_to = "indicator", values_to = "mean_score") %>%
  ggplot(aes(x = indicator, y = reorder(region, mean_score, FUN = mean), fill = mean_score)) +
  geom_tile() +
  geom_text(aes(label = round(mean_score, 2)), size = 3) +
  scale_fill_gradient2(low = "firebrick2", mid = "white", high = "steelblue", midpoint = 0) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(title = "Mean Governance Score by Region and Indicator",
       x = NULL, y = NULL, fill = "Mean Score") +
  theme_minimal()

(p_region_means | ((p_violin + ggtitle("Composite Governance Scores by Region\n(1996-2023)")) / p_early_vs_recent_box)) +
  plot_layout(widths = c(1, 2.2))

# ── 5. ASSUMPTION TESTING ── #####

# 5a. Correlation heatmap
cor_matrix <- df %>%
  select(all_of(gov_cols)) %>%
  cor(use = "complete.obs")

p_corr <- ggcorrplot(cor_matrix, method = "square", type = "full", lab = TRUE, lab_size = 4) +
  scale_x_discrete(limits = rev, guide = guide_axis(n.dodge = 2)) +
  scale_fill_gradient2(name = "Pearson\nCorrelation",
                       low = "steelblue", mid = "white", high = "firebrick2",
                       midpoint = 0, limits = c(-1, 1)) +
  theme(axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 10, angle = 0, hjust = 0.5),
        legend.position = "left",
        legend.key.height = unit(1.5, "cm"))

# 5b. Correlation by region
region_cor_plots <- df %>%
  group_by(region) %>%
  group_map(~ {
    cor_m <- .x %>% select(all_of(gov_cols)) %>% cor(use = "complete.obs")
    ggcorrplot(cor_m, method = "square", type = "full", lab = TRUE, lab_size = 2) +
      scale_x_discrete(limits = rev, guide = guide_axis(n.dodge = 2)) +
      scale_fill_gradient2(name = "Pearson\nCorrelation",
                           low = "steelblue", mid = "white", high = "firebrick2",
                           midpoint = 0, limits = c(-1, 1)) +
      labs(title = .y$region) +
      theme(axis.text.x     = element_text(size = 5, angle = 0, hjust = 0.5),
            axis.text.y     = element_text(size = 5),
            plot.title      = element_text(size = 8, face = "bold"),
            legend.position = "none")
  }, .keep = TRUE)

wrap_plots(region_cor_plots, ncol = 3) +
  plot_annotation(title = "Correlation Between Governance Indicators by Region",
                  theme = theme(plot.title = element_text(face = "bold", size = 13)))

# 5c. Normality — Mardia's test + QQ plot + negentropy

fa_data <- df %>% select(all_of(gov_cols)) %>% na.omit()

# Mardia's test
mardia_result <- mvn(fa_data, mvnTest = "mardia", multivariatePlot = "qq")

neg_entropy <- df %>%
  select(all_of(gov_cols)) %>%
  summarise(across(everything(), ~ (1/12) * mean((scale(.x))^3, na.rm = TRUE)^2 +
                     (1/48) * (mean((scale(.x))^4, na.rm = TRUE) - 3)^2)) %>%
  pivot_longer(everything(), names_to = "indicator", values_to = "negentropy") %>%
  arrange(desc(negentropy))

neg_entropy

# 5d. Sampling adequacy

KMO(fa_data)
cortest.bartlett(cor_matrix, n = nrow(fa_data))

# ── 6. FACTOR ANALYSIS ── #####

# 6a. Parallel analysis
pa <- fa.parallel(fa_data, fm = "ml", fa = "fa", plot = FALSE)

pa_df <- data.frame(
  factor         = seq_along(pa$fa.values),
  observed       = pa$fa.values,
  simulated_95   = pa$fa.sim,
  simulated_mean = pa$fa.simr
) %>%
  mutate(cum_var = cumsum(pmax(observed, 0)) / sum(pmax(observed, 0)) * 100)

n_factors <- 2
y_max     <- max(pa_df$observed) * 1.1
scale_f   <- y_max / 100

p_scree <- ggplot(pa_df, aes(x = factor)) +
  annotate("rect", xmin = 0.5, xmax = n_factors + 0.5,
           ymin = -Inf, ymax = Inf, fill = "steelblue", alpha = 0.1) +
  geom_line(aes(y = observed,          color = "Observed eigenvalues"), linewidth = 1) +
  geom_point(aes(y = observed,         color = "Observed eigenvalues"), size = 2.5) +
  geom_line(aes(y = simulated_95,      color = "95th pctl (null)"),  linetype = "dashed",  linewidth = 0.8) +
  geom_point(aes(y = simulated_95,     color = "95th pctl (null)"),  shape = 17, size = 2) +
  geom_line(aes(y = simulated_mean,    color = "Mean (null)"),        linetype = "dotted",  linewidth = 0.8) +
  geom_line(aes(y = cum_var * scale_f, color = "Cumulative Variance"), linewidth = 1, linetype = "longdash") +
  geom_point(aes(y = cum_var * scale_f, color = "Cumulative Variance"), size = 2.5) +
  geom_hline(yintercept = 1, color = "gold3", linetype = "dotted", linewidth = 0.8) +
  annotate("text", x = max(pa_df$factor), y = 1,
           label = "Kaiser criterion", color = "gold3", hjust = 1.1, vjust = -0.5, size = 3.5) +
  annotate("text", x = n_factors + 0.3, y = max(pa_df$observed) * 0.85,
           label = paste(n_factors, "factors retained"),
           color = "steelblue", hjust = -0.1, size = 3.5) +
  scale_color_manual(values = c("Observed eigenvalues" = "steelblue",
                                "95th pctl (null)"     = "firebrick2",
                                "Mean (null)"          = "grey40",
                                "Cumulative Variance"  = "darkorchid")) +
  scale_x_continuous(breaks = seq_along(pa$fa.values)) +
  scale_y_continuous(
    name     = "Eigenvalue",
    sec.axis = sec_axis(~ . / scale_f, name = "Cumulative Variance (%)",
                        breaks = seq(0, 100, 20),
                        labels = paste0(seq(0, 100, 20), "%"))) +
  labs(title = paste0("Parallel Analysis (n = ", nrow(fa_data), ", p = ", length(gov_cols), ")"),
       x = "Component", color = NULL) +
  theme_minimal() +
  theme(plot.title             = element_text(face = "bold", hjust = 0.5),
        legend.position        = "inside",
        legend.position.inside = c(0.72, 0.55),
        axis.title.y.right     = element_text(color = "darkorchid"),
        axis.text.y.right      = element_text(color = "darkorchid"))

# 6b. Fit FA models
fa_none    <- fa(fa_data, nfactors = 2, rotate = "none",    fm = "ml")
fa_varimax <- fa(fa_data, nfactors = 2, rotate = "varimax", fm = "ml")
fa_promax  <- fa(fa_data, nfactors = 2, rotate = "promax",  fm = "ml")

# 6c. Loading heatmaps
loadings_long <- function(fa_model, rotation_name) {
  fa_model$loadings[] %>%
    as.data.frame() %>%
    rownames_to_column("indicator") %>%
    pivot_longer(-indicator, names_to = "factor", values_to = "loading") %>%
    mutate(rotation = rotation_name)
}

p_loadings <- bind_rows(
  loadings_long(fa_none,    "No Rotation"),
  loadings_long(fa_varimax, "Varimax"),
  loadings_long(fa_promax,  "Promax")
) %>%
  mutate(rotation = factor(rotation, levels = c("No Rotation", "Varimax", "Promax"))) %>%
  ggplot(aes(x = factor, y = indicator, fill = loading)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(loading, 2)), size = 3.5, fontface = "bold") +
  scale_fill_gradient2(low = "steelblue", mid = "white", high = "firebrick2",
                       midpoint = 0, limits = c(-1, 1), name = "Loading") +
  facet_wrap(~ rotation, ncol = 3) +
  labs(title = NULL, x = NULL, y = NULL) +
  theme_minimal() +
  theme(plot.title      = element_text(face = "bold", hjust = 0.5),
        strip.text      = element_text(face = "bold", size = 11),
        axis.text.y     = element_text(size = 10),
        legend.position = "none")

p_corr | (p_loadings / p_scree + plot_layout(heights = c(1, 1.2))) +
  plot_layout(widths = c(1, 2))

# ── 7. INTERPRETATION — FACTOR SCORE BIPLOTS ── #####

# 7a. Extract promax and varimax scores and join to df
fa_pro_scores <- as.data.frame(fa_promax$scores) %>%
  rename(Factor1 = ML1, Factor2 = ML2)

df_pro_fa <- df %>%
  na.omit() %>%
  bind_cols(fa_pro_scores)

fa_vari_scores <- as.data.frame(fa_varimax$scores) %>%
  rename(Factor1 = ML1, Factor2 = ML2)

df_vari_fa <- df %>%
  na.omit() %>%
  bind_cols(fa_vari_scores)

# 7b. Global biplot with regional centroids for promax and varimax
region_centroids <- df_pro_fa %>%
  group_by(region) %>%
  summarise(Factor1 = mean(Factor1, na.rm = TRUE),
            Factor2 = mean(Factor2, na.rm = TRUE))

region_vari_centroids <- df_vari_fa %>%
  group_by(region) %>%
  summarise(Factor1 = mean(Factor1, na.rm = TRUE),
            Factor2 = mean(Factor2, na.rm = TRUE))

# Compute shared axis limits
x_lim <- range(c(df_pro_fa$Factor1, df_vari_fa$Factor1), na.rm = TRUE)
y_lim <- range(c(df_pro_fa$Factor2, df_vari_fa$Factor2), na.rm = TRUE)

# Promax biplot
p_promax <- ggplot() +
  geom_point(data = df_pro_fa,
             aes(x = Factor1, y = Factor2, color = region),
             alpha = 0.3, size = 1) +
  geom_point(data = region_centroids,
             aes(x = Factor1, y = Factor2, color = region),
             size = 5, shape = 21, fill = "white", stroke = 2) +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(limits = x_lim) +
  scale_y_continuous(limits = y_lim) +
  labs(title = "Promax Rotation",
       x     = "Factor 1 - State Effectiveness",
       y     = "Factor 2 - Civil Order & Accountability",
       color = "Region") +
  theme_minimal() +
  theme(plot.title      = element_text(face = "bold", hjust = 0.5),
        legend.position = "none")

# Varimax biplot
p_varimax <- ggplot() +
  geom_point(data = df_vari_fa,
             aes(x = Factor1, y = Factor2, color = region),
             alpha = 0.3, size = 1) +
  geom_point(data = region_vari_centroids,
             aes(x = Factor1, y = Factor2, color = region),
             size = 5, shape = 21, fill = "white", stroke = 2) +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(limits = x_lim) +
  scale_y_continuous(limits = y_lim) +
  labs(title = "Varimax Rotation",
       x     = "Factor 1 - State Effectiveness",
       y     = NULL,
       color = "Region") +
  theme_minimal() +
  theme(plot.title      = element_text(face = "bold", hjust = 0.5),
        axis.text.y     = element_blank(),
        axis.ticks.y    = element_blank(),
        legend.position = "bottom")

region_levels <- sort(unique(df_pro_fa$region))  # or define manually

df_pro_fa$region        <- factor(df_pro_fa$region,        levels = region_levels)
df_vari_fa$region       <- factor(df_vari_fa$region,       levels = region_levels)
region_centroids$region <- factor(region_centroids$region, levels = region_levels)
region_vari_centroids$region <- factor(region_vari_centroids$region, levels = region_levels)

p_promax + p_varimax

# 7c. Window centroids from df_pro_fa (same factor space as global biplot)
window_centroids <- df_pro_fa %>%
  mutate(window = case_when(
    year >= 1996 & year <= 2000 ~ "1996–2000",
    year >= 2019 & year <= 2023 ~ "2019–2023",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(window)) %>%
  group_by(region, window) %>%
  summarise(Factor1 = mean(Factor1, na.rm = TRUE),
            Factor2 = mean(Factor2, na.rm = TRUE),
            .groups = "drop")

# 7d. Country trajectory distances from df_pro_fa
country_trajectory <- df_pro_fa %>%
  mutate(window = case_when(
    year >= 1996 & year <= 2000 ~ "early",
    year >= 2019 & year <= 2023 ~ "recent",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(window)) %>%
  group_by(country, country_code, region, window) %>%
  summarise(Factor1 = mean(Factor1, na.rm = TRUE),
            Factor2 = mean(Factor2, na.rm = TRUE),
            .groups = "drop") %>%
  pivot_wider(names_from = window, values_from = c(Factor1, Factor2)) %>%
  filter(!is.na(Factor1_early) & !is.na(Factor1_recent)) %>%
  mutate(trajectory_dist = sqrt((Factor1_recent - Factor1_early)^2 +
                                  (Factor2_recent - Factor2_early)^2)) %>%
  arrange(desc(trajectory_dist))

# 7e. Region order by max trajectory distance
region_order <- country_trajectory %>%
  group_by(region) %>%
  summarise(max_dist = max(trajectory_dist, na.rm = TRUE)) %>%
  arrange(desc(max_dist)) %>%
  pull(region)

df_pro_fa <- df_pro_fa %>% mutate(region = factor(region, levels = region_order))
window_centroids <- window_centroids %>% mutate(region = factor(region, levels = region_order))

# 7f. Bootstrap test: did region centroids shift significantly between windows?
set.seed(437)
B <- 1000

# Observed centroid difference per region
observed_diff <- df_pro_fa %>%
  mutate(window = case_when(
    year >= 1996 & year <= 2000 ~ "early",
    year >= 2019 & year <= 2023 ~ "recent",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(window)) %>%
  group_by(region, window) %>%
  summarise(Factor1 = mean(Factor1, na.rm = TRUE),
            Factor2 = mean(Factor2, na.rm = TRUE),
            .groups = "drop") %>%
  pivot_wider(names_from = window, values_from = c(Factor1, Factor2)) %>%
  mutate(obs_dist = sqrt((Factor1_recent - Factor1_early)^2 +
                           (Factor2_recent - Factor2_early)^2))

# Bootstrap: permute window labels within each region and recompute centroid distance
bootstrap_results <- df_pro_fa %>%
  mutate(window = case_when(
    year >= 1996 & year <= 2000 ~ "early",
    year >= 2019 & year <= 2023 ~ "recent",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(window)) %>%
  group_by(region) %>%
  group_map(~ {
    data    <- .x
    reg     <- .y$region
    obs     <- observed_diff %>% filter(region == reg) %>% pull(obs_dist)
    
    boot_dists <- replicate(B, {
      permuted <- data %>% mutate(window = sample(window))
      cents    <- permuted %>%
        group_by(window) %>%
        summarise(F1 = mean(Factor1, na.rm = TRUE),
                  F2 = mean(Factor2, na.rm = TRUE),
                  .groups = "drop")
      if (nrow(cents) < 2) return(NA)
      sqrt((cents$F1[cents$window == "recent"] - cents$F1[cents$window == "early"])^2 +
             (cents$F2[cents$window == "recent"] - cents$F2[cents$window == "early"])^2)
    })
    
    data.frame(
      region   = reg,
      obs_dist = obs,
      p_value  = mean(boot_dists >= obs, na.rm = TRUE),
      ci_95    = quantile(boot_dists, 0.95, na.rm = TRUE)
    )
  }, .keep = TRUE) %>%
  bind_rows() %>%
  mutate(significant = p_value < 0.05) %>%
  arrange(p_value)

bootstrap_results

ggplot(df_pro_fa, aes(x = Factor1, y = Factor2, color = year)) +
  geom_point(alpha = 0.6, size = 1) +
  geom_path(aes(group = country_code), alpha = 0.3, linewidth = 0.3) +
  geom_point(data = window_centroids,
             aes(x = Factor1, y = Factor2, shape = window),
             color = "black", size = 4) +
  # p-value label
  geom_label(data = bootstrap_results %>%
               mutate(region     = factor(region, levels = region_order),
                      label_text = paste0("p = ", round(p_value, 3))),
             aes(x = Inf, y = -Inf, label = label_text),
             inherit.aes   = FALSE,
             hjust = 1.05, vjust = -0.3,
             size = 3, fill = "white", color = "grey30",
             label.padding = unit(0.3, "lines"),
             label.r       = unit(0.15, "lines")) +
  scale_color_gradient(low = "firebrick2", high = "steelblue") +
  scale_shape_manual(values = c("1996–2000" = 15, "2019–2023" = 17)) +
  facet_wrap(~ region, ncol = 3) +
  labs(x      = "Factor 1 - State Effectiveness",
       y      = "Factor 2 - Civil Order & Accountability",
       color  = "Year", shape = "Window\nMidpoint") +
  theme_minimal() +
  theme(plot.title      = element_text(face = "bold", hjust = 0.5),
        legend.position = "right",
        strip.text      = element_text(face = "bold", size = 9))

# 7g. Top 9 countries by trajectory distance
top9 <- country_trajectory %>%
  filter(!is.na(trajectory_dist)) %>%
  slice_head(n = 9) %>%
  mutate(rank  = row_number(),
         label = paste0(scales::ordinal(rank), " - ", country))

top9_codes  <- top9$country_code
top9_labels <- setNames(top9$label, top9$country_code)

p_traj <- df_pro_fa %>%
  filter(country_code %in% top9_codes) %>%
  mutate(country_code = factor(country_code, levels = top9_codes)) %>%
  ggplot(aes(x = Factor1, y = Factor2, color = year)) +
  geom_point(alpha = 0.6, size = 1.5) +
  geom_path(aes(group = country_code), linewidth = 0.6, alpha = 0.8) +
  geom_label(data = df_pro_fa %>%
               filter(country_code %in% top9_codes) %>%
               mutate(country_code = factor(country_code, levels = top9_codes)) %>%
               group_by(country_code) %>%
               filter(year == min(year) | year == max(year)) %>%
               ungroup(),
             aes(label = year), size = 2.5, alpha = 0.8) +
  # Trajectory distance text box
  geom_label(data = top9 %>%
               mutate(country_code = factor(country_code, levels = top9_codes),
                      label_text   = paste0("Traj dist: ", round(trajectory_dist, 2))),
             aes(x = 2.3, y = -2.5, label = label_text),
             inherit.aes = FALSE,
             hjust = 1, vjust = 0,
             size = 3, fontface = "bold",
             fill = "white", color = "grey30",
             label.padding = unit(0.3, "lines"),
             label.r       = unit(0.15, "lines")) +
  facet_wrap(~ country_code, ncol = 3,
             labeller = labeller(country_code = top9_labels)) +
  scale_color_gradient(low = "firebrick2", high = "steelblue") +
  scale_x_continuous(limits = c(-2.5, 2.5)) +
  scale_y_continuous(limits = c(-2.5, 2.5)) +
  labs(title = "Governance Trajectory Distance",
       x     = "Factor 1 - State Effectiveness",
       y     = "Factor 2 - Civil Order & Accountability",
       color = "Year") +
  theme_minimal() +
  theme(plot.title      = element_text(face = "bold", hjust = 0.5),
        legend.position = "right",
        strip.text      = element_text(face = "bold", size = 9))

# 7h. Countries furthest from the Factor1 = Factor2 diagonal
country_diagonal <- df_pro_fa %>%
  group_by(country, country_code, region) %>%
  summarise(diagonal_dist = mean(abs(Factor2 - Factor1) / sqrt(2), na.rm = TRUE),
            avg_Factor1   = mean(Factor1, na.rm = TRUE),
            avg_Factor2   = mean(Factor2, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(desc(diagonal_dist))

top9_diag <- country_diagonal %>%
  slice_head(n = 9) %>%
  mutate(rank  = row_number(),
         label = paste0(scales::ordinal(rank), " - ", country))

top9_diag_codes  <- top9_diag$country_code
top9_diag_labels <- setNames(top9_diag$label, top9_diag$country_code)

p_diag_dist <- df_pro_fa %>%
  filter(country_code %in% top9_diag_codes) %>%
  mutate(country_code = factor(country_code, levels = top9_diag_codes)) %>%
  ggplot(aes(x = Factor1, y = Factor2, color = year)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "grey40", linewidth = 0.6) +
  geom_point(alpha = 0.6, size = 1.5) +
  geom_path(aes(group = country_code), linewidth = 0.6, alpha = 0.8) +
  geom_label(data = df_pro_fa %>%
               filter(country_code %in% top9_diag_codes) %>%
               mutate(country_code = factor(country_code, levels = top9_diag_codes)) %>%
               group_by(country_code) %>%
               filter(year == min(year) | year == max(year)) %>%
               ungroup(),
             aes(label = year), size = 2.5, alpha = 0.8) +
  # Average diagonal distance text box
  geom_label(data = top9_diag %>%
               mutate(country_code = factor(country_code, levels = top9_diag_codes),
                      label_text = paste0("Avg dist: ", round(diagonal_dist, 2))),
             aes(x = 2.5, y = -2.5, label = label_text),
             inherit.aes = FALSE,
             hjust = 1, vjust = 0,
             size = 3, fontface = "bold",
             fill = "white", color = "grey30",
             label.padding = unit(0.3, "lines"),
             label.r = unit(0.15, "lines")) +
  facet_wrap(~ country_code, ncol = 3,
             labeller = labeller(country_code = top9_diag_labels)) +
  scale_color_gradient(low = "firebrick2", high = "steelblue") +
  scale_x_continuous(limits = c(-2.5, 2.5)) +
  scale_y_continuous(limits = c(-2.5, 2.5)) +
  labs(title = "Furthest from Factor 1 = Factor 2 Diagonal",
       x     = "Factor 1 - State Effectiveness",
       y     = NULL,
       color = "Year") +
  theme_minimal() +
  theme(plot.title      = element_text(face = "bold", hjust = 0.5),
        legend.position = "right",
        strip.text      = element_text(face = "bold", size = 9))

p_traj_nl    <- p_traj      + theme(legend.position = "none")
p_diag_nl    <- p_diag_dist + theme(legend.position = "none")

shared_legend <- get_legend(
  p_traj + theme(legend.position = "right")
)

plot_grid(
  p_traj_nl, p_diag_nl, shared_legend,
  nrow       = 1,
  rel_widths = c(1, 1, 0.1)
)
  
