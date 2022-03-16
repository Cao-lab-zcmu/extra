visualize_facet_pca <- 
  function(
           df,
           palette,
           metadata,
           savename = "pca_facet.svg",
           anno_adjust = 3/4
           ){
    ## ---------------------------------------------------------------------- 
    ## get PC importance df
    df <- df %>%
      dplyr::filter(facet_row != "extra") %>%
      merge(metadata, by = "sample", all.x = T) %>%
      dplyr::as_tibble()
    ## ------------------------------------- 
    annotation <- df %>%
      dplyr::distinct(im_PC1, im_PC2, legend_PC1, legend_PC2,
                      anno_x, anno_y,
                      facet_col, facet_row) %>%
      dplyr::mutate(anno_x = min(anno_x) * anno_adjust,
                    anno_y = max(anno_y) * anno_adjust)
    ## ---------------------------------------------------------------------- 
    ## drawing part
    p <- ggplot(df, aes(x = PC1, y = PC2, fill = group)) +
      geom_point(alpha = 0.8, size = 3, shape = 21, stroke = 0.1) +
      ## draw confidence ellipse
      stat_ellipse(aes(color = group), level = 0.95) +
      scale_color_manual(values = palette) +
      scale_fill_manual(values = palette) +
      ## exlude repeat legend
      guides(color =  "none") +
      ## for PC1
      geom_text(data = annotation, aes(x = anno_x * 1.4, y = anno_y * 1.4, label = legend_PC1),
                hjust = 0, color = "black",
                fontface = "bold", alpha = 0.6,
                size = 2, inherit.aes = FALSE,
                family = "Times") +
      ## for PC2
      geom_text(data = annotation, aes(x = anno_x * 1.4, y = anno_y * (18/22) * 1.4, label = legend_PC2),
                hjust = 0, color = "black", fontface = "bold",
                alpha = 0.6, size = 2, inherit.aes = FALSE,
                family = "Times") +
      labs(y = "PC2", x = "PC1", fill = "Group") +
      ## facet into multiple panel
      facet_grid(Hmisc::capitalize(facet_row) ~ Hmisc::capitalize(facet_col)) +
      theme(legend.position = "right",
            text = element_text(family = "Times"))
    ggsave(p, file = savename, width = 11, height = 6.5)
    return(p)
  }
