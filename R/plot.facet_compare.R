plot.facet_compare <- 
  function(
           list1,
           list2,
           ## ------------------------------------- 
           title,
           savename,
           ylim_min = 50,
           group_levels = c("origin", "noise", "high noise"), 
           from = c("MCnebula", "MolnetEnhancer"),
           by_col = "classification",
           ylab = "In cluster numbers",
           xlab = "Classification",
           fill_lab = "Type",
           ## ------------------------------------- 
           palette = ggsci::pal_npg()(9),
           width = 18,
           height = 12
           ){
    ## select in common classification
    common.class <- merge(list1[[1]], list2[[1]], by = by_col) %>% 
      dplyr::select(1)
    ## filter classification
    list <- list(list1, list2) %>% 
      lapply(function(list){
               list <- lapply(list, merge, y = common.class, by = by_col, all.y = T) %>% 
                 ## reset col name as sum
                 lapply(dplyr::rename, sum = 2) %>% 
                 lapply(dplyr::mutate, sum = ifelse(is.na(sum), 0, sum))
           }) %>% 
      lapply(data.table::rbindlist, idcol = T) %>% 
      lapply(dplyr::rename, group = .id) %>% 
      lapply(function(df){
               dplyr::mutate(df, group = mapply_rename_col("h_noise", "high noise", group))
           }) %>% 
      mapply(function(df, VALUE){
               dplyr::mutate(df, from = VALUE)
           }, ., from, SIMPLIFY = F)
    ## for segment
    df <- merge(list[[1]], list[[2]], by = c("group", by_col))
    ## for point
    df2 <- dplyr::bind_rows(list[[1]], list[[2]])
    ## ---------------------------------------------------------------------- 
    ## plot figure
    p <- ggplot() +
      geom_segment(data = df,
                   aes(x = classification,
                       xend = classification,
                       y = sum.x,
                       yend = sum.y),
                   color = "black") +
      geom_point(data = df2,
                 aes(x = classification, y = sum, color = from),
                 size = 5,
                 position = "identity") +
      scale_color_manual(values = palette) +
      scale_y_continuous(breaks = c(ylim_min, 300, 600, 900, 1200)) +
      labs(title = Hmisc::capitalize(title),
           y = Hmisc::capitalize(ylab),
           x = Hmisc::capitalize(xlab),
           fill = Hmisc::capitalize(fill_lab)) +
      coord_flip(ylim = c(ylim_min, max(df2$sum) * 1.1)) +
      facet_wrap(~ factor(group, levels = group_levels)) +
      theme(legend.position = "bottom",
            text = element_text(family = "Times", size = 20, face = "bold"),
            axis.text.x = element_text(size = 10),
            strip.text = element_text(size = 20),
            plot.title = element_text(hjust = 0.3))
    ggsave(p, file = savename, width = width, height = height)
  }
