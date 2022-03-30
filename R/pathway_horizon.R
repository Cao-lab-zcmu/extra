pathway_horizon <- 
  function(
           df,
           title,
           ylab = "-log10(Gamma p)",
           xlab = "pathway",
           fill_lab = "",
           save = "tmp.svg"
           ){
    df <- dplyr::mutate(df, Gamma = -log10(Gamma),
                        pathway = stringr::str_wrap(pathway, width = 30)) %>% 
      dplyr::arrange(desc(Gamma)) %>% 
      dplyr::slice(1:20)
    p <- ggplot(data = df) +
      geom_point(position = "identity",
                 aes(x = reorder(pathway, Gamma),
                     y = Gamma,
                     size = Hits.sig,
                     color = Hits.sig)) +
      # geom_segment(aes(x = pathway, xend = pathway, y = 0, yend = Gamma)) +
      scale_color_gradient2(low = "white", mid = "#FED439FF", high = "#BB0021FF") +
      # geom_hline(yintercept = -log10(0.05), linetype = 3) +
      guides(size = "none") +
      labs(title = Hmisc::capitalize(title),
           y = Hmisc::capitalize(ylab),
           x = Hmisc::capitalize(xlab),
           fill = Hmisc::capitalize(fill_lab)) +
      coord_flip() +
      theme(legend.position = "right",
            plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "cm"),
            text = element_text(family = "Times", face = "bold")
            # axis.text = element_text(size = 6),
            # plot.title = element_text(hjust = 0.3)
      )
    ggsave(p, file = save, width = 7, height = 8)
  }
