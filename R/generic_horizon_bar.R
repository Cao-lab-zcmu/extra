generic_horizon_bar <- 
  function(
           df,
           title,
           palette = ggsci::pal_npg()(9),
           ylab = "stat",
           xlab = "type",
           fill_lab = "",
           save = "tmp.svg"
           ){
    p <- ggplot(data = df,
                aes(x = class,
                    y = value,
                    fill = .id)) +
      geom_col(width = 0.7,
               position = "identity") +
      scale_fill_manual(values = palette) +
      labs(title = Hmisc::capitalize(title),
           y = Hmisc::capitalize(ylab),
           x = Hmisc::capitalize(xlab),
           fill = Hmisc::capitalize(fill_lab)) +
      coord_flip() +
      facet_wrap(~.id, scales = "free") +
      theme(legend.position = "bottom",
            text = element_text(size = 16, family = "Times", face = "bold"),
            axis.text = element_text(size = 4),
            plot.title = element_text(hjust = 0.3))
    ggsave(p, file = save, width = 11, height = 9)
  }
