mutate_horizon_bar_accuracy <- 
  function(
           df,
           title,
           savename,
           palette = ggsci::pal_npg()(9),
           ylab = "stat ratio",
           xlab = "classification",
           fill_lab = "type",
           extra_sides_df = NULL,
           return_p = T
           ){
    ## ------------------------------------- 
    ## get parent class
    parent_class <- mutate_get_parent_class(df$classification) %>%
      lapply(., end_of_vector) %>%
      unlist() %>%
      unname()
    df <- dplyr::mutate(df, parent_class = ifelse(is.na(parent_class), classification, parent_class))
    ## ---------------------------------------------------------------------- 
    annotation <- df %>%
      dplyr::mutate(combine = paste0(classification, " ---- ", parent_class))
    df <- reshape2::melt(df, id.vars = c("classification", "parent_class"),
                         variable.name = "type",
                         value.name = "value")
    df <- dplyr::mutate(df,
                        classification = stringr::str_wrap(classification, width = 25),
                        parent_class = stringr::str_wrap(parent_class, width = 25),
                        type = as.character(type),
                        type = Hmisc::capitalize(type))
    ## ---------------------------------------------------------------------- 
    p <- ggplot(data = df,
                aes(x = classification,
                    y = value,
                    fill = type)) +
      geom_col(width = 0.7,
               position = "stack") +
      scale_fill_manual(values = palette) +
      labs(title = Hmisc::capitalize(title),
           y = Hmisc::capitalize(ylab),
           x = Hmisc::capitalize(xlab),
           fill = Hmisc::capitalize(fill_lab)) +
      coord_flip() +
      theme(legend.position = "bottom",
            axis.text.y = element_blank(),
            text = element_text(family = "Times", size = 20, face = "bold"),
            plot.title = element_text(hjust = 0.3))
    ## ---------------------------------------------------------------------- 
    if(is.null(extra_sides_df) == F){
      max = 500
      ps <- ggplot(data = extra_sides_df) +
        geom_col(width = 0.7,
                 fill = "#709AE1FF",
                 alpha = 0.7,
                 aes(x = classification, y = ifelse(sum >= max, max, sum))) +
        coord_flip() +
        ylim(0, max) +
        labs(x = "", y = "Compounds number") +
        theme(axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              text = element_text(family = "Times", size = 20, face = "bold"))
      ## ------------------------------------- 
      pa1 <- ggplot(annotation) +
        geom_tile(aes(x = "classification", y = stringr::str_wrap(classification, width = 25),
                      fill = stringr::str_wrap(parent_class, width = 25)),
                  width = 1, height = 1, alpha = 0.5, size = 1, color = "black") +
        labs(fill = "", x = "", y = "") +
        theme_minimal() +
        scale_fill_lancet() +
        theme(text = element_text(size = 14, face = "bold", family = "Times"),
              axis.text.x = element_blank(),
              legend.key.height = unit(1.5, "cm"),
              legend.position = "left",
              panel.grid = element_blank())
      ## ------------------------------------- 
      svg(savename, width = 16, height = 15)
      grid.newpage()
      pushViewport( viewport(layout = grid.layout(100, 200) ))
      ## ------------------ 
      ## classification
      print( pa1, vp = viewport(layout.pos.row = 5:94, layout.pos.col = 1:63))
      ## cluster accuracy
      print( p, vp = viewport(layout.pos.row = 2:100, layout.pos.col = 65:138))
      ## compounds number
      print( ps, vp = viewport(layout.pos.row = 5:96, layout.pos.col = 142:195))
      ## ------------------ 
      dev.off()
      ## ------------------------------------- 
      return()
    }
    ## ---------------------------------------------------------------------- 
    if(return_p == T)
      return(p)
    ggsave(p, file = savename, width = 9, height = 15)
  }
end_of_vector <- 
  function(
           vector
           ){
    if(length(vector) == 0){
      return(NA)
    }
    var <- vector[length(vector)]
    return(var)
  }
