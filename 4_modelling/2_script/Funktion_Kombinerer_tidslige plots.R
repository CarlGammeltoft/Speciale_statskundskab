### Kombinerer tidslige plots

# Start med at køre de forskellige scripts, der laver plotsne. Kør derefter denne fil

library(cowplot)

# Pro-palæstinensiske demonstrationer -------------------------------------
# Kombinerer barplots for pro-palæstinensiske demoer
# Starter med at fjerne legend og overskrifter.

# Ændr y-aksens tekst og overskriften for hvert plot
gnm_2_pal_plot <- gnm_2_pal_plot +
  labs(title = "2 dage", y = "Gennemsnitlig kommunikation") +
  theme(legend.position = "none")

gnm_7_pal_plot <- gnm_7_pal_plot +
  labs(title = "7 dage", y = "Gennemsnitlig kommunikation") +
  theme(legend.position = "none")

gnm_14_pal_plot <- gnm_14_pal_plot +
  labs(title = "14 dage", y = "Gennemsnitlig kommunikation") +
  theme(legend.position = "none")

gnm_21_pal_plot <- gnm_21_pal_plot +
  labs(title = "21 dage", y = "Gennemsnitlig kommunikation") +
  theme(legend.position = "none")

# Ekstraher den fælles legende fra ét plot
common_legend <- get_legend(gnm_2_pal_plot + theme(legend.position = "bottom"))

# Ekstraher korrekt legende ved hjælp af ggplotGrob
grob <- ggplotGrob(
  gnm_2_pal_plot +
    guides(fill = guide_legend(nrow = 1)) +  # Én række for legenden
    theme(legend.position = "bottom", legend.box = "horizontal")
)

# Find og isoler legenden i grob-objektet
common_legend <- gtable::gtable_filter(grob, "guide-box")

# Kombiner plots i et grid
barplots_pale_combined <- plot_grid(
  gnm_2_pal_plot, gnm_7_pal_plot, 
  gnm_14_pal_plot, gnm_21_pal_plot, 
  ncol = 2, align = "hv"
)

# Tilføj den samlede overskrift og fælles legende
final_plot <- plot_grid(
  ggdraw() + 
    draw_label("Gennemsnitlig kommunikation efter demonstrationer/ikke-demonstration (pro-palæstinensisk)", 
               fontface = "bold", size = 14, hjust = 0.5),
  barplots_pale_combined,
  common_legend,
  ncol = 1, rel_heights = c(0.08, 0.82, 0.1)
)

# Vis det endelige plot
print(final_plot)

# Gemmer plottet som en enkelt fil
ggsave(
  filename = "/Users/carlgammeltoft/Lokale dokumenter/R_speciale/4_modelling/3_output/Plots_deskriptiv_sammenligning_tidslig_2_7_14_21_dage/pro_pale_plots.png",
  plot = final_plot,
  width = 12, height = 10
)


# Pro-Israel --------------------------------------------------------------

# Pro-israelske demonstrationer -------------------------------------
# Kombinerer barplots for pro-israelske demoer
# Starter med at fjerne legend og overskrifter.

# Ændr y-aksens tekst og overskriften for hvert plot
gnm_2_isr_plot <- gnm_2_isr_plot +
  labs(title = "2 dage", y = "Gennemsnitlig kommunikation") +
  theme(legend.position = "none")

gnm_7_isr_plot <- gnm_7_isr_plot +
  labs(title = "7 dage", y = "Gennemsnitlig kommunikation") +
  theme(legend.position = "none")

gnm_14_isr_plot <- gnm_14_isr_plot +
  labs(title = "14 dage", y = "Gennemsnitlig kommunikation") +
  theme(legend.position = "none")

gnm_21_isr_plot <- gnm_21_isr_plot +
  labs(title = "21 dage", y = "Gennemsnitlig kommunikation") +
  theme(legend.position = "none")

# Ekstraher den fælles legende fra ét plot
grob <- ggplotGrob(
  gnm_2_isr_plot +
    guides(fill = guide_legend(nrow = 1)) +  # Én række for legenden
    theme(legend.position = "bottom", legend.box = "horizontal")
)

# Find og isoler legenden i grob-objektet
common_legend <- gtable::gtable_filter(grob, "guide-box")

# Kombiner plots i et grid
barplots_isr_combined <- plot_grid(
  gnm_2_isr_plot, gnm_7_isr_plot, 
  gnm_14_isr_plot, gnm_21_isr_plot, 
  ncol = 2, align = "hv"
)

# Tilføj den samlede overskrift og fælles legende
final_plot_isr <- plot_grid(
  ggdraw() + 
    draw_label("Gennemsnitlig kommunikation efter demonstrationer/ikke-demonstration (pro-israelsk)", 
               fontface = "bold", size = 14, hjust = 0.5),
  barplots_isr_combined,
  common_legend,
  ncol = 1, rel_heights = c(0.08, 0.82, 0.1)
)

# Vis det endelige plot
print(final_plot_isr)

# Gemmer plottet som en enkelt fil
ggsave(
  filename = "/Users/carlgammeltoft/Lokale dokumenter/R_speciale/4_modelling/3_output/Plots_deskriptiv_sammenligning_tidslig_2_7_14_21_dage/pro_israel_plots.png",
  plot = final_plot_isr,
  width = 12, height = 10
)




