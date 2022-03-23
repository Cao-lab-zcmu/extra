merge_horizon_accuracy <- 
  function(
           list,
           title,
           savename,
           palette = ggsci::pal_simpsons()(9),
           ylab = "stat ratio",
           xlab = "classification",
           fill_lab = "type",
           return_p = T
           ){
    list <- lapply(list, reshape2::melt,
                   id.vars = "classification",
                   variable.name = "type",
                   value.name = "value")
    list <- lapply(list, dplyr::mutate,
                   classification = stringr::str_wrap(classification, width = 25),
                   type = as.character(type),
                   type = Hmisc::capitalize(type))
    df <- data.table::rbindlist(list, idcol = T) %>% 
      dplyr::filter(type == "True")
    line_df <- reshape2::dcast(df, classification + type ~ .id)
    ## ---------------------------------------------------------------------- 
    p <- ggplot(data = df,
                aes(x = classification,
                    y = value,
                    color = .id)) +
      geom_segment(data = line_df, aes(x = classification, xend = classification, y = top1, yend = top50),
                   color = "black") +
      geom_point(size = 5,
                 position = "identity") +
      scale_color_manual(values = palette) +
      labs(title = Hmisc::capitalize(title),
           y = Hmisc::capitalize(ylab),
           x = Hmisc::capitalize(xlab),
           fill = Hmisc::capitalize(fill_lab)) +
      coord_flip() +
      theme(legend.position = "bottom",
            text = element_text(family = "Times", size = 20, face = "bold"),
            plot.title = element_text(hjust = 0.3))
      ## ---------------------------------------------------------------------- 
      if(return_p == T)
        return(p)
      ggsave(p, file = savename, width = 9, height = 15)
  }
