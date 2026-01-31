source(file.path(here(), "code", "submission 2", "SupplementaryFigure 4", "batchA.R"))
source(file.path(here(), "code", "submission 2", "SupplementaryFigure 4", "batchB.R"))

df_plot <- rbind(df_plotA |> add_column(case = "(A) Kidney"), 
                 df_plotB |> add_column(case = "(B) Liver"))

smooth_lines_df <- rbind(smooth_lines_dfA |> add_column(case = "(A) Kidney"), 
      smooth_lines_dfB |> add_column(case = "(B) Liver"))


p <- ggplot(df_plot, aes(x = Date)) +
  geom_point(aes(y = Observed), color = "black", size = 1.5) +
  geom_line(
    data = smooth_lines_df,
    aes(x = Date, y = Fitted_smooth, group = segment_id, color = Panel),
    linewidth = 1
  ) +
  scale_color_manual(
    values = c(
      "Drug Overdose" = "blue",
      "Non-drug Overdose" = "red",
      "Overall" = "darkgreen"
    )
  ) +
  scale_x_date(
    breaks = breaks,
    labels = labels,
    limits = c(start_date, as.Date("2024-12-31") + 30),
    expand = expansion(mult = c(0.01, 0.0))
  ) +
  labs(
    x = "Month",
    y = "Organ transplants\n(per 1,000 actively waitlisted patients)"
  ) +
  facet_grid(Panel ~ case, scales = "free_y") +
  theme_bw(base_size = 16) +
  theme(
    legend.position = "none",
    #strip.text = element_text(face = "bold", size = 14),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.text = element_text(angle = 0),
    panel.spacing = unit(1, "lines"), 
    strip.background = element_rect(fill = "black"), 
    strip.text = element_text(face = "bold", color = "white"),
    panel.spacing.x = unit(2, "lines")
  ) 


print(p)
if(FALSE){
  ggsave(file.path(here(), "code", "submission 2", "SupplementaryFigure 4", "SupplementaryFigure 4.pdf"), 
         p, 
         device = "pdf", 
         height = 11, 
         width = 8.5, 
         units = "in")
}
