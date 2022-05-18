show_palette <- 
  function(
           palette,
           width = 2,
           height = 10,
           font_size = 5,
           ylab = "Re-ID",
           xlab = "Color",
           title = "",
           re_order = T
           ){
    df <- data.table::data.table(name = names(palette), color = unname(palette)) %>% 
      dplyr::mutate(name = stringr::str_wrap(name, width = 25))
    if(!re_order){
      df <- dplyr::mutate(df, name = factor(name, levels = name))
    }
    ## ---------------------------------------------------------------------- 
    p <- ggplot(df) +
      geom_tile(aes(x = "color", y = name,
                    fill = name),
                width = 1, height = 1, size = 1, color = "black") +
      labs(x = xlab, y = ylab) +
      ggtitle(title) +
      guides(fill = "none") +
      scale_fill_manual(values = palette) +
      theme_minimal() +
      theme(text = element_text(size = font_size, face = "bold", family = "Times"),
            title = element_text(hjust = -2),
            axis.text.x = element_blank(),
            panel.grid = element_blank())
    ggsave(p, file = "tmp.svg", width = width, height = height)
    return()
  }
mutate_show_palette <- 
  function(
           palette,
           width = 5,
           height = 10,
           font_size = 5,
           ylab = "Re-ID",
           xlab = "Color",
           legend.position = "right",
           legend.key.height = unit(5, "cm"),
           legend.key.width = unit(0.5, "cm"),
           fill_lab = "",
           title = "",
           re_order = T
           ){
    df <- data.table::data.table(name = names(palette), color = unname(palette)) %>% 
      dplyr::mutate(name = stringr::str_wrap(name, width = 25))
    if(!re_order){
      df <- dplyr::mutate(df, name = factor(name, levels = name))
    }
    ## ---------------------------------------------------------------------- 
    p <- ggplot(df) +
      geom_tile(aes(x = "color", y = name,
                    fill = name),
                width = 1, height = 1, size = 1, color = "black") +
      labs(x = xlab, y = ylab, fill = fill_lab) +
      ggtitle(title) +
      scale_fill_manual(values = palette) +
      theme_minimal() +
      theme(text = element_text(size = font_size, face = "bold", family = "Times"),
            title = element_text(hjust = -2),
            axis.text.x = element_blank(),
            legend.position = legend.position,
            legend.key.height = legend.key.height,
            legend.key.width = legend.key.width,
            panel.grid = element_blank())
    p <- ggpubr::get_legend(p)
    p <- ggpubr::as_ggplot(p)
    ggsave(p, file = "tmp.svg", width = width, height = height)
    return()
  }
