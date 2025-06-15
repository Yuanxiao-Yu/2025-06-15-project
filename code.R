library(tidyverse)
library(ez)
library(ggplot2)

read.csv("data_preprocessed.csv")
data <- as.data.frame(data_preprocessed)


# Reshape data to long format for amplitude (μV)
# Extracting ERP amplitude values
data_long_uv <- data %>%
  pivot_longer(
    cols = ends_with("_μV"),
    names_to = "word_class",
    values_to = "amplitude"
  ) %>%
  mutate(
    word_class = case_when(
      word_class == "common_noun_μV" ~ "Common Noun",
      word_class == "action_verb_μV" ~ "Action Verb",
      word_class == "typical_event_noun_μV" ~ "Typical Event Noun",
      word_class == "gerund_event_noun_μV" ~ "Gerund Event Noun"
    )
  )
summary(data_long_uv)
view(data_long_uv)


# Reshape data to long format for latency (ms)
# Extracting latency levels
data_long_ms <- data %>%
  pivot_longer(
    cols = ends_with("_ms"),
    names_to = "word_class",
    values_to = "latency"
  ) %>%
  mutate(
    word_class = case_when(
      word_class == "common_noun_ms" ~ "Common Noun",
      word_class == "action_verb_ms" ~ "Action Verb",
      word_class == "typical_event_noun_ms" ~ "Typical Event Noun",
      word_class == "gerund_event_noun_ms" ~ "Gerund Event Noun"
    )
  )
summary(data_long_ms)


# Assigning the Positions to their Groups
p200_positions <- c("C3", "C4", "O1", "O2")
n400_positions <- c("Pz", "CP3", "CP4", "CPz")
p600_positions <- c("Pz", "T7", "T8")

# Run ANOVA for a given ERP component and measure (amplitude | latency)
run_anova <- function(data, measure, erp_name, positions) {
  data_filtered <- data %>%
    filter(position %in% positions)
  
  if (measure == "amplitude") {
    anova_result <- ezANOVA(
      data = data_filtered,
      dv = amplitude,
      wid = sample_name,
      within = .(word_class, position),
      detailed = TRUE,
      type = 3
    )
  } else {
    anova_result <- ezANOVA(
      data = data_filtered,
      dv = latency,
      wid = sample_name,
      within = .(word_class, position),
      detailed = TRUE,
      type = 3
    )
  }
  
  # Print ANOVA results with Greenhouse-Geisser correction
  cat("\nANOVA Results for", erp_name, "-", measure, "\n")
  print(anova_result$ANOVA)
  print(anova_result$`Mauchly's Test for Sphericity`)
  print(anova_result$`Sphericity Corrections`)
  
  # Post-hoc pairwise comparisons
  if (measure == "amplitude") {
    posthoc <- pairwise.t.test(
      data_filtered$amplitude,
      interaction(data_filtered$word_class, data_filtered$position),
      paired = TRUE,
      p.adjust.method = "bonferroni"
    )
  } else {
    posthoc <- pairwise.t.test(
      data_filtered$latency,
      interaction(data_filtered$word_class, data_filtered$position),
      paired = TRUE,
      p.adjust.method = "bonferroni"
    )
  }
  
  cat("\nPost-hoc Pairwise Comparisons for", erp_name, "-", measure, "\n")
  print(posthoc)
}

# Run ANOVA for each ERP component and measure
run_anova(data_long_uv, "amplitude", "P200", p200_positions)
run_anova(data_long_ms, "latency", "P200", p200_positions)
run_anova(data_long_uv, "amplitude", "N400", n400_positions)
run_anova(data_long_ms, "latency", "N400", n400_positions)
run_anova(data_long_uv, "amplitude", "P600", p600_positions)
run_anova(data_long_ms, "latency", "P600", p600_positions)

# Visualization: Mean amplitude by word class and position
plot_amplitude <- function(data, erp_name, positions) {
  data_filtered <- data %>%
    filter(position %in% positions) %>%
    group_by(word_class, position) %>%
    summarise(mean_amplitude = mean(amplitude, na.rm = TRUE), .groups = "drop")
  
  ggplot(data_filtered, aes(x = position, y = mean_amplitude, fill = word_class)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    labs(title = paste("Mean Amplitude for", erp_name),
         x = "Electrode Position", y = "Mean Amplitude (μV)") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set2")
}

# Visualization: Mean latency by word class and position
plot_latency <- function(data, erp_name, positions) {
  data_filtered <- data %>%
    filter(position %in% positions) %>%
    group_by(word_class, position) %>%
    summarise(mean_latency = mean(latency, na.rm = TRUE), .groups = "drop")
  
  ggplot(data_filtered, aes(x = position, y = mean_latency, fill = word_class)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    labs(title = paste("Mean Latency for", erp_name),
         x = "Electrode Position", y = "Mean Latency (ms)") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set2")
}

# Generate plots
ggsave("p200_amplitude.png", plot_amplitude(data_long_uv, "P200", p200_positions))
ggsave("p200_latency.png", plot_latency(data_long_ms, "P200", p200_positions))
ggsave("n400_amplitude.png", plot_amplitude(data_long_uv, "N400", n400_positions))
ggsave("n400_latency.png", plot_latency(data_long_ms, "N400", n400_positions))
ggsave("p600_amplitude.png", plot_amplitude(data_long_uv, "P600", p600_positions))
ggsave("p600_latency.png", plot_latency(data_long_ms, "P600", p600_positions))
