horizon_bar_accuracy <- 
  function(
           df,
           title,
           savename,
           palette = ggsci::pal_npg()(9),
           ylab = "stat ratio",
           xlab = "classification",
           fill_lab = "type",
           extra_sides_df = NA,
           return_p = T
           ){
    df <- reshape2::melt(df, id.vars = "classification",
                         variable.name = "type",
                         value.name = "value")
    df <- dplyr::mutate(df, classification = stringr::str_wrap(classification, width = 25),
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
            text = element_text(family = "Times", size = 20, face = "bold"),
            plot.title = element_text(hjust = 0.3))
    ## ---------------------------------------------------------------------- 
    if(!is.na(extra_sides_df)){
      max = 500
      ps <- ggplot(data = extra_sides_df) +
        geom_col(width = 0.7, aes(x = classification, y = ifelse(sum >= max, max, sum))) +
        coord_flip() +
        ylim(0, max) +
        theme(axis.text.y = element_blank(),
              text = element_text(family = "Times", size = 20, face = "bold"))
      ## ------------------------------------- 
      svg(savename, width = 14, height = 15)
      grid.newpage()
      pushViewport( viewport(layout = grid.layout(100, 20) ))
      ## ------------------ 
      print( p, vp=viewport(layout.pos.row=1:100, layout.pos.col=1:12))
      print( ps, vp=viewport(layout.pos.row=4:95, layout.pos.col=13:19))
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
