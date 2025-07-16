### Kombinerer alle tidslige plots i ét samlet plot

library(cowplot)

# Pro-palæstinensiske demonstrationer -------------------------------------
# Ændr y-aksens tekst og overskriften for hvert plot
gnm_2_pal_plot <- gnm_2_pal_plot +
  labs(title = "2 dage", y = "Gennemsnitlig kommunikation") +
  theme(legend.position = "none") +
  theme(plot.subtitle = element_blank())

gnm_7_pal_plot <- gnm_7_pal_plot +
  labs(title = "7 dage", y = "Gennemsnitlig kommunikation") +
  theme(legend.position = "none") +
  theme(plot.subtitle = element_blank())

gnm_14_pal_plot <- gnm_14_pal_plot +
  labs(title = "14 dage", y = "Gennemsnitlig kommunikation") +
  theme(legend.position = "none") + 
  theme(plot.subtitle = element_blank())

gnm_21_pal_plot <- gnm_21_pal_plot +
  labs(title = "21 dage", y = "Gennemsnitlig kommunikation") +
  theme(legend.position = "none") + 
  theme(plot.subtitle = element_blank())

# Pro-israelske demonstrationer -------------------------------------------
# Ændr y-aksens tekst og overskriften for hvert plot
gnm_2_isr_plot <- gnm_2_isr_plot +
  labs(title = "2 dage", y = "Gennemsnitlig kommunikation") +
  theme(legend.position = "none") + 
  theme(plot.subtitle = element_blank())

gnm_7_isr_plot <- gnm_7_isr_plot +
  labs(title = "7 dage", y = "Gennemsnitlig kommunikation") +
  theme(legend.position = "none") + 
  theme(plot.subtitle = element_blank())

gnm_14_isr_plot <- gnm_14_isr_plot +
  labs(title = "14 dage", y = "Gennemsnitlig kommunikation") +
  theme(legend.position = "none") + 
  theme(plot.subtitle = element_blank())

gnm_21_isr_plot <- gnm_21_isr_plot +
  labs(title = "21 dage", y = "Gennemsnitlig kommunikation") +
  theme(legend.position = "none") + 
  theme(plot.subtitle = element_blank())

# Ekstraher den fælles legende fra ét plot
grob <- ggplotGrob(
  gnm_2_pal_plot +
    guides(fill = guide_legend(nrow = 1)) +  # Én række for legenden
    theme(legend.position = "bottom", legend.box = "horizontal")
)
common_legend <- gtable::gtable_filter(grob, "guide-box")

# Kombiner de pro-palæstinensiske plots i et grid
barplots_pale_combined <- plot_grid(
  gnm_2_pal_plot, gnm_7_pal_plot, 
  gnm_14_pal_plot, gnm_21_pal_plot, 
  ncol = 2, align = "hv"
)

# Kombiner de pro-israelske plots i et grid
barplots_isr_combined <- plot_grid(
  gnm_2_isr_plot, gnm_7_isr_plot, 
  gnm_14_isr_plot, gnm_21_isr_plot, 
  ncol = 2, align = "hv"
)

# Kombiner alt i ét samlet plot
# final_plot <- plot_grid(
#  ggdraw() + 
#    draw_label("Gennemsnitlig kommunikation efter demonstrationsstatus", 
#               fontface = "bold", size = 16, hjust = 0.5),
#  plot_grid(
#    ggdraw() + draw_label("Pro-palæstinensiske demonstrationer", fontface = "bold", size = 14, hjust = 0.5),
#    barplots_pale_combined,
#    ggdraw() + draw_label("Pro-israelske demonstrationer", fontface = "bold", size = 14, hjust = 0.5),
#    barplots_isr_combined,
#    ncol = 1, rel_heights = c(0.05, 0.45, 0.05, 0.45)
#  ),
#  common_legend,
#  ncol = 1, rel_heights = c(0.08, 0.85, 0.07)
#)

# Kombiner alt i ét samlet plot
# Kombiner alt i ét samlet plot
final_plot <- plot_grid(
  ggdraw() + 
    draw_label("Pro-palæstinensiske demonstrationer", fontface = "bold", size = 14, hjust = 0.5),
  barplots_pale_combined,
  ggdraw() + 
    draw_label("Pro-israelske demonstrationer", fontface = "bold", size = 14, hjust = 0.5),
  barplots_isr_combined,
  ncol = 1, 
  rel_heights = c(0.01, 0.4, 0.01, 0.4) # Forbedret spacing
)

# Tilføj den fælles legende nederst
final_plot <- plot_grid(
  final_plot,
  common_legend,
  ncol = 1,
  rel_heights = c(0.9, 0.05) # Tilpasning for at gøre plads til legenden
)

# Vis det endelige plot
print(final_plot)

# Gem det endelige plot som en enkelt fil
ggsave(
  filename = "/Users/carlgammeltoft/Lokale dokumenter/R_speciale/4_modelling/3_output/Plots_deskriptiv_sammenligning_tidslig_2_7_14_21_dage/combined_plots_all.pdf",
  plot = final_plot,
  width = 14, height = 18
)


