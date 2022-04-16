show_palette <- 
  function(
           palette,
           width = 2,
           height = 10,
           font_size = 5,
           ylab = "Re-ID"
           ){
    df <- data.table::data.table(name = names(palette), color = unname(palette))
    ## ---------------------------------------------------------------------- 
    p <- ggplot(df) +
      geom_tile(aes(x = "color", y = stringr::str_wrap(name, width = 25),
                    fill = name),
                width = 1, height = 1, size = 1, color = "black") +
      labs(x = "Color", y = ylab) +
      guides(fill = "none") +
      scale_fill_manual(values = palette) +
      theme_minimal() +
      theme(text = element_text(size = font_size, face = "bold", family = "Times"),
            plot.background = element_rect(fill = 'white', colour = "white"),
            axis.text.x = element_blank(),
            panel.grid = element_blank())
    ggsave(p, file = "tmp.svg", width = width, height = height)
    return()
  }
