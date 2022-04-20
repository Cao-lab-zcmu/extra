generic_horizon_bar <- 
  function(
           df,
           scale_fill_expression = "scale_fill_gradient(high = '#E6550DFF', low = '#FDD0A2FF')",
           x = colnames(df)[1],
           y = colnames(df)[2],
           ylab = "stat",
           xlab = "type",
           position = "identity",
           save = "tmp.svg"
           ){
    df[[x]] <- stringr::str_wrap(df[[x]], width = 35)
    df[[x]] <- factor(df[[x]], levels = df[[x]][order(df[[y]], decreasing = F)])
    p <- ggplot(data = df,
                aes(x = eval(parse(text = x)),
                    y = eval(parse(text = y)),
                    fill = eval(parse(text = y)))) +
      geom_col(width = 0.7, position = position) +
      eval(parse(text = scale_fill_expression)) +
      labs(y = Hmisc::capitalize(ylab),
           x = Hmisc::capitalize(xlab)) +
      coord_flip() +
      theme(legend.position = "none",
            text = element_text(size = 16, family = "Times", face = "bold"),
            axis.text = element_text(size = 10),
            plot.title = element_text(hjust = 0.3))
    ggsave(p, file = save, width = 6, height = 12)
  }
