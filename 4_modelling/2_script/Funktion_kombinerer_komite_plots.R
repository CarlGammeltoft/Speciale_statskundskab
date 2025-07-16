### Funktion - sammensætter KAMP-figurer


### NOTE: KØR FØRST SCRIPTS FOR KAMPVALG ISRAEL OG PALÆSTINA OG KØR DEREFTER DETTE

# Tjek at begge plots er kørt
israel_plot
palestine_plot

# Sammensætter plots - Kombiner plots med fælles legend
# Sørg for ensartede y-aksegrænser
palestine_plot <- palestine_plot + 
  scale_y_continuous(limits = c(min(results_df$conf_low), max(results_df$conf_high)))

israel_plot <- israel_plot + 
  scale_y_continuous(limits = c(min(results_df$conf_low), max(results_df$conf_high)))

# Fjern y-akse label fra højre plot (palestine_plot)
palestine_plot <- palestine_plot + 
  labs(y = NULL, title = "Pro-palæstinensiske demonstrationer")  # Opdater titel

# Opdater titel for venstre plot (israel_plot)
israel_plot <- israel_plot + 
  labs(title = "Pro-israelske demonstrationer")  # Opdater titel

# Kombinér plots side om side med fælles overskrift og legend i bunden
combined_plot <- israel_plot + palestine_plot +
  plot_layout(ncol = 2, guides = "collect") +  # Layout med 2 kolonner og fælles legend
  plot_annotation(
    title = "",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 16))  # Center og justér titel
  ) &
  theme(legend.position = "bottom")  # Placér legend i bunden

# Vis det kombinerede plot
print(combined_plot)

# Gemmer figuren
ggsave(
  filename = "/Users/carlgammeltoft/Lokale dokumenter/R_speciale/4_modelling/3_output/plots_komiteer/kombineret_komite.pdf",
  plot = combined_plot,
  width = 12, height = 6
)
