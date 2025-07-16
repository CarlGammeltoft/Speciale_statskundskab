# Indlæser data -----------------------------------------------------------
library(tidyverse)
library(patchwork)

setwd("/Users/carlgammeltoft/Lokale dokumenter/R_speciale/4_modelling/")
data <- read.csv("1_input/data_finalized.csv")
data <- data %>% 
  filter(date >= "2023-09-01")


# PRO-PALÆSTINA-PLOTS -----------------------------------------------------

# Define size categories
size_categories <- list(
  "Lille (<40 demonstranter)" = function(x) x < 40 & x > 0,
  "Mellem (40-99 demonstranter)" = function(x) x >= 40 & x <= 99,
  "Stor (>99 demonstranter)" = function(x) x > 99,
  "alle" = function(x) x > 0
)

# Prepare data for plotting
barplot_data <- list()
for (size_label in names(size_categories)) {
  size_filter <- size_categories[[size_label]]
  
  # Calculate mean communication with and without demonstrations for different ideology groups
  communication_data <- data %>%
    filter(size_filter(size_palestine_protests) | size_palestine_protests == 0) %>%
    mutate(
      Group = ifelse(size_filter(size_palestine_protests), "Med demonstration", "Uden demonstration"),
      IdeologyGroup = case_when(
        ideology_percentile_rep <= 0.15 ~ "Mest liberale",
        ideology_percentile_rep >= 0.85 ~ "Mest konservative",
        main_ideology == 0 ~ "Liberale",
        main_ideology == 1 ~ "Konservative",
        TRUE ~ "Alle politikere"
      )
    ) %>%
    filter(!is.na(IdeologyGroup)) %>%
    group_by(Group, IdeologyGroup) %>%
    summarise(
      MeanCommunication = mean(communication_window_7, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(SizeCategory = size_label)
  
  barplot_data[[size_label]] <- communication_data
}

# Add "Alle politikere" as a group for each size category
for (size_label in names(size_categories)) {
  size_filter <- size_categories[[size_label]]
  
  all_politicians <- data %>%
    filter(size_filter(size_palestine_protests) | size_palestine_protests == 0) %>%
    mutate(Group = ifelse(size_filter(size_palestine_protests), "Med demonstration", "Uden demonstration")) %>%
    group_by(Group) %>%
    summarise(
      IdeologyGroup = "Alle politikere",
      MeanCommunication = mean(communication_window_7, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(SizeCategory = size_label)
  
  barplot_data[[size_label]] <- bind_rows(barplot_data[[size_label]], all_politicians)
}

# Combine all results
barplot_df <- bind_rows(barplot_data)

# Correct the order of ideology groups
ideology_levels <- c("Mest liberale", "Liberale", "Konservative", "Mest konservative", "Alle politikere")
barplot_df$IdeologyGroup <- factor(barplot_df$IdeologyGroup, levels = ideology_levels)

# Add highlighting for "Alle politikere" in plots
plots <- list()
for (size_label in unique(barplot_df$SizeCategory)) {
  plot_data <- barplot_df %>% filter(SizeCategory == size_label)
  
  plots[[size_label]] <- ggplot(plot_data, aes(x = IdeologyGroup, y = MeanCommunication, fill = Group)) +
    annotate("rect", xmin = which(levels(factor(plot_data$IdeologyGroup)) == "Alle politikere") - 0.5, 
             xmax = which(levels(factor(plot_data$IdeologyGroup)) == "Alle politikere") + 0.5, 
             ymin = -Inf, ymax = Inf, fill = "lightgrey", alpha = 0.8) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    geom_hline(yintercept = 0) + 
    labs(
      title = paste("Kommunikation for", size_label),
      x = "Ideologisk gruppe",
      y = "Gennemsnitlig kommunikation",
      fill = "Demonstrationsstatus"
    ) +
    theme_minimal() +
    scale_fill_manual(values = c("Med demonstration" = "black", "Uden demonstration" = "grey")) +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      plot.title = element_text(hjust = 0.5),
      panel.grid = element_blank()
    )
}

# To view plots, use:
print(plots[["alle"]])
print(plots[["Lille (<40 demonstranter)"]])
print(plots[["Mellem (40-99 demonstranter)"]])
print(plots[["Stor (>99 demonstranter)"]])

plots_palestine <- plots

# PRO-ISRAEL-PLOTS -----------------------------------------------------

# Define size categories
size_categories <- list(
  "Lille (<40 demonstranter)" = function(x) x < 40 & x > 0,
  "Mellem (40-99 demonstranter)" = function(x) x >= 40 & x <= 99,
  "Stor (>99 demonstranter)" = function(x) x > 99,
  "alle" = function(x) x > 0
)

# Prepare data for plotting
barplot_data <- list()
for (size_label in names(size_categories)) {
  size_filter <- size_categories[[size_label]]
  
  # Calculate mean communication with and without demonstrations for different ideology groups
  communication_data <- data %>%
    filter(size_filter(size_israel_protests) | size_israel_protests == 0) %>%
    mutate(
      Group = ifelse(size_filter(size_israel_protests), "Med demonstration", "Uden demonstration"),
      IdeologyGroup = case_when(
        ideology_percentile_rep <= 0.15 ~ "Mest liberale",
        ideology_percentile_rep >= 0.85 ~ "Mest konservative",
        main_ideology == 0 ~ "Liberale",
        main_ideology == 1 ~ "Konservative",
        TRUE ~ "Alle politikere"
      )
    ) %>%
    filter(!is.na(IdeologyGroup)) %>%
    group_by(Group, IdeologyGroup) %>%
    summarise(
      MeanCommunication = mean(communication_window_7, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(SizeCategory = size_label)
  
  barplot_data[[size_label]] <- communication_data
}

# Add "Alle politikere" as a group for each size category
for (size_label in names(size_categories)) {
  size_filter <- size_categories[[size_label]]
  
  all_politicians <- data %>%
    filter(size_filter(size_israel_protests) | size_israel_protests == 0) %>%
    mutate(Group = ifelse(size_filter(size_israel_protests), "Med demonstration", "Uden demonstration")) %>%
    group_by(Group) %>%
    summarise(
      IdeologyGroup = "Alle politikere",
      MeanCommunication = mean(communication_window_7, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(SizeCategory = size_label)
  
  barplot_data[[size_label]] <- bind_rows(barplot_data[[size_label]], all_politicians)
}

# Combine all results
barplot_df <- bind_rows(barplot_data)

# Correct the order of ideology groups
ideology_levels <- c("Mest liberale", "Liberale", "Konservative", "Mest konservative", "Alle politikere")
barplot_df$IdeologyGroup <- factor(barplot_df$IdeologyGroup, levels = ideology_levels)

# Add highlighting for "Alle politikere" in plots
plots <- list()
for (size_label in unique(barplot_df$SizeCategory)) {
  plot_data <- barplot_df %>% filter(SizeCategory == size_label)
  
  plots[[size_label]] <- ggplot(plot_data, aes(x = IdeologyGroup, y = MeanCommunication, fill = Group)) +
    annotate("rect", xmin = which(levels(factor(plot_data$IdeologyGroup)) == "Alle politikere") - 0.5, 
             xmax = which(levels(factor(plot_data$IdeologyGroup)) == "Alle politikere") + 0.5, 
             ymin = -Inf, ymax = Inf, fill = "lightgrey", alpha = 0.8) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    geom_hline(yintercept = 0) +
    labs(
      title = paste("Kommunikation for", size_label),
      x = "Ideologisk gruppe",
      y = "Gennemsnitlig kommunikation",
      fill = "Demonstrationsstatus"
    ) +
    theme_minimal() +
    scale_fill_manual(values = c("Med demonstration" = "black", "Uden demonstration" = "grey")) +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      plot.title = element_text(hjust = 0.5),
      panel.grid = element_blank()
    )
}

# To view plots, use:
print(plots[["alle"]])
print(plots[["Lille (<40 demonstranter)"]])
print(plots[["Mellem (40-99 demonstranter)"]])
print(plots[["Stor (>99 demonstranter)"]])

plots_israel <- plots


# Samler plotsne ----------------------------------------------------------

# Gemmer plotsne ----------------------------------------------------------

# Install and load necessary libraries
library(ggplot2)
library(cowplot)

# Definer et fælles tema for alle plots
common_theme <- theme(
  axis.text = element_text(size = 10),
  axis.title = element_text(size = 12),
  plot.title = element_text(size = 14, hjust = 0.5),
  legend.position = "none"
)

# Juster plot-titler og tilføj det fælles tema til plots_palestine
plots_palestine[["alle"]] <- plots_palestine[["alle"]] +
  labs(title = "Alle demonstrationer", y = "Gennemsnitlig kommunikation") +
  common_theme

plots_palestine[["Lille (<40 demonstranter)"]] <- plots_palestine[["Lille (<40 demonstranter)"]] +
  labs(title = "Lille (<40 demonstranter)", y = "Gennemsnitlig kommunikation") +
  common_theme

plots_palestine[["Mellem (40-99 demonstranter)"]] <- plots_palestine[["Mellem (40-99 demonstranter)"]] +
  labs(title = "Mellem (40-99 demonstranter)", y = "Gennemsnitlig kommunikation") +
  common_theme

plots_palestine[["Stor (>99 demonstranter)"]] <- plots_palestine[["Stor (>99 demonstranter)"]] +
  labs(title = "Stor (>99 demonstranter)", y = "Gennemsnitlig kommunikation") +
  common_theme

# Juster plot-titler og tilføj det fælles tema til plots_israel
plots_israel[["alle"]] <- plots_israel[["alle"]] +
  labs(title = "Alle demonstrationer", y = "Gennemsnitlig kommunikation") +
  common_theme

plots_israel[["Lille (<40 demonstranter)"]] <- plots_israel[["Lille (<40 demonstranter)"]] +
  labs(title = "Lille (<40 demonstranter)", y = "Gennemsnitlig kommunikation") +
  common_theme

plots_israel[["Mellem (40-99 demonstranter)"]] <- plots_israel[["Mellem (40-99 demonstranter)"]] +
  labs(title = "Mellem (40-99 demonstranter)", y = "Gennemsnitlig kommunikation") +
  common_theme

plots_israel[["Stor (>99 demonstranter)"]] <- plots_israel[["Stor (>99 demonstranter)"]] +
  labs(title = "Stor (>99 demonstranter)", y = "Gennemsnitlig kommunikation") +
  common_theme

# Ekstraher fælles legende fra ét plot
grob <- ggplotGrob(
  plots_palestine[["alle"]] +
    guides(fill = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom", legend.box = "horizontal", legend.spacing.y = unit(-0.05, "cm"))
)
common_legend <- gtable::gtable_filter(grob, "guide-box")



# Kombiner alle plots i et grid
palestine_combined <- plot_grid(
  plots_palestine[["alle"]],
  plots_palestine[["Lille (<40 demonstranter)"]],
  plots_palestine[["Mellem (40-99 demonstranter)"]],
  plots_palestine[["Stor (>99 demonstranter)"]],
  ncol = 2, align = "hv"
)

israel_combined <- plot_grid(
  plots_israel[["alle"]],
  plots_israel[["Lille (<40 demonstranter)"]],
  plots_israel[["Mellem (40-99 demonstranter)"]],
  plots_israel[["Stor (>99 demonstranter)"]],
  ncol = 2, align = "hv"
)

# Saml den endelige figur
#final_combined_plot <- plot_grid(
#  ggdraw() + 
#    draw_label("Forskel i kommunikation (Med demonstration vs. Uden demonstration)", 
#               fontface = "bold", size = 16, hjust = 0.5),
#  ggdraw() + draw_label("Pro-palæstinensiske demonstrationer", fontface = "bold", size = 14, hjust = 0.5),
#  palestine_combined,
#  ggdraw() + draw_label("Pro-israelske demonstrationer", fontface = "bold", size = 14, hjust = 0.5),
#  israel_combined,
#  common_legend,
#  ncol = 1, rel_heights = c(0.05, 0.03, 0.42, 0.03, 0.42, 0.05)
#)

# Saml den endelige figur
#final_combined_plot <- plot_grid(
#  ggdraw() + draw_label("Pro-palæstinensiske demonstrationer", fontface = "bold", size = 14, hjust = 0.5),
#  palestine_combined,
#  ggdraw() + draw_label("Pro-israelske demonstrationer", fontface = "bold", size = 14, hjust = 0.5),
#  israel_combined,
#  common_legend,
#  ncol = 1, 
#  rel_heights = c(0.01, 0.4, 0.01, 0.4, 0.1) # Adjusted heights
#)


# Kombiner alt i ét samlet plot
final_plot <- plot_grid(
  ggdraw() + 
    draw_label("Pro-palæstinensiske demonstrationer", fontface = "bold", size = 14, hjust = 0.5),
  palestine_combined,
  ggdraw() + 
    draw_label("Pro-israelske demonstrationer", fontface = "bold", size = 14, hjust = 0.5),
  israel_combined,
  ncol = 1, 
  rel_heights = c(0.01, 0.4, 0.01, 0.4) # Forbedret spacing
)

# Tilføj den fælles legende tættere på plottene
final_plot <- plot_grid(
  final_plot,
  common_legend,
  ncol = 1,
  rel_heights = c(0.95, 0.1) # Gør plads til legenden tættere på
)

# Vis det endelige plot
print(final_plot)

# Gem det samlede plot som en enkelt fil
ggsave(
  filename = "/Users/carlgammeltoft/Lokale dokumenter/R_speciale/4_modelling/3_output/Plots_deskriptiv_sammenligning_forskellige_størrelser/Plots_combined_palestine_israel.pdf",
  plot = final_plot,
  width = 14, height = 18
)

