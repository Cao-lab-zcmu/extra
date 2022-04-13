mutate2_horizon_bar_accuracy <- 
  function(
           df_list,
           extra_list,
           ## ------------------------------------- 
           title,
           savename,
           ylab = "stat ratio",
           xlab = "classification",
           fill_lab = "type",
           ## ------------------------------------- 
           palette = ggsci::pal_npg()(9),
           mutate_palette = c("true" = palette[3],
                              "latent" = palette[2],
                              "false" = palette[1],
                              "noise" = "#FED439FF",
                              "high_noise" = "#8A4198FF"),
           extra_palette = c("sum" = "#95CC5EFF",
                             "noise" = "#FED439FF",
                             "high_noise" = "#8A4198FF"),
           group_palette = ggsci::pal_rickandmorty()(12),
           ## ------------------------------------- 
           width = 18,
           height = 15,
           l_ratio = 57,
           m_ratio = 130
           # extra_col_max = NA
           ){
    ## ---------------------------------------------------------------------- 
    ## ---------------------------------------------------------------------- 
    ## ---------------------------------------------------------------------- 
    ## get parent class
    df_list <- lapply(df_list, function(df){
                        parent_class <- mutate_get_parent_class(df$classification) %>%
                          lapply(., end_of_vector) %>%
                          unlist(use.names = F)
                        df <- dplyr::mutate(df, parent_class = ifelse(is.na(parent_class),
                                                                      classification,
                                                                      parent_class),
                                            st.true = 0, en.true = true,
                                            st.latent = en.true, en.latent = st.latent + latent,
                                            st.false = en.latent, en.false = st.false + false)
           })
    ## ---------------------------------------------------------------------- 
    ## ---------------------------------------------------------------------- 
    ## ---------------------------------------------------------------------- 
    ## group draw
    annotation <- df_list[["origin"]]
    pa1 <- ggplot(annotation) +
      geom_tile(aes(x = "classification", y = stringr::str_wrap(classification, width = 25),
                    fill = stringr::str_wrap(parent_class, width = 25)),
                width = 1, height = 1, alpha = 0.5, size = 1, color = "black") +
      labs(fill = "", x = "", y = "") +
      theme_minimal() +
      scale_fill_manual(values = colorRampPalette(group_palette)(length(unique(annotation$parent_class)))) +
      theme(text = element_text(size = 14, face = "bold", family = "Times"),
            axis.text.x = element_blank(),
            legend.key.height = unit(1.5, "cm"),
            legend.position = "left",
            panel.grid = element_blank())
    ## ---------------------------------------------------------------------- 
    ## ---------------------------------------------------------------------- 
    ## ---------------------------------------------------------------------- 
    ## initial stat
    mutate_origin <- df_list[["origin"]] %>% 
      reshape2::melt(., id.vars = colnames(.)[!colnames(.) %in% c("true", "false", "latent")],
                     variable.name = "type",
                     value.name = "value") %>% 
    dplyr::mutate(., y = as.numeric(apply(., 1, function(v){
                                            v[[paste0("st.", v[["type"]])]]
                                                                      })),
                  yend = as.numeric(apply(., 1, function(v){
                                 v[[paste0("en.", v[["type"]])]]
                                            })))
    ## ---------------------------------------------------------------------- 
    ## noise dirft
    noise_df <- mutate2.horizon.tmp_merge("origin", "noise", df_list) %>% 
      dplyr::filter(y != yend, exclude == F,
                    classification %in% mutate_origin$classification)
    ## high noise drift
    h_noise_df <- mutate2.horizon.tmp_merge("noise", "h_noise", df_list) %>% 
      dplyr::filter(y != yend, exclude == F,
                    classification %in% mutate_origin$classification)
    ## ---------------------------------------------------------------------- 
    p <- ggplot() +
      ## origin
      geom_segment(data = mutate_origin,
                   aes(x = classification, xend = classification,
                       y = y, yend = yend,
                       color = type),
                   size = 7) +
      ## noise drift
      geom_segment(data = noise_df,
                 aes(x = classification, xend = classification,
                     y = y, yend = yend,
                     color = "noise"),
                   size = 7, 
                   inherit.aes = F) +
      ## high noise drift
      geom_segment(data = h_noise_df,
                 aes(x = classification, xend = classification,
                     y = y, yend = yend,
                     color = "high_noise"),
                   size = 7,
                   inherit.aes = F) +
      ## the point indicate the start of noise drift
      geom_point(data = noise_df,
                 aes(x = classification,
                     y = y, color = "noise"),
                 shape = 4, size = 5) +
      ## the point indicate the start of high noise drift
      geom_point(data = h_noise_df,
                 aes(x = classification,
                     y = y, color = "high_noise"),
                 shape = 4, size = 5) +
      scale_color_manual(values = mutate_palette) +
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
    ## ---------------------------------------------------------------------- 
    ## ---------------------------------------------------------------------- 
    extra.noise_df <- mutate2.extra.horizon.tmp_merge(extra_list = extra_list)
    ## ------------------------------------- 
    ps <- ggplot() +
        ## origin sum
        geom_segment(data = extra_list[["origin"]],
                     aes(x = classification,
                         xend = classification,
                         y = 0,
                         yend = sum,
                         color = "sum"),
                     size = 7
                     ) +
        ## noise drift
        geom_segment(data = dplyr::mutate(extra.noise_df,
                                          sum.x = ifelse(is.na(sum.x), 0, sum.x)),
                     aes(x = classification,
                         xend = classification,
                         y = sum,
                         yend = sum.x,
                         color = "noise"),
                     size = 7
                     ) +
        ## high_noise drift
        geom_segment(data = dplyr::mutate(dplyr::filter(extra.noise_df, is.na(sum.x) == F),
                                          sum.x = ifelse(is.na(sum.y), 0, sum.x),
                                          sum.y = ifelse(is.na(sum.y), sum.x, sum.y)),
                     aes(x = classification,
                         xend = classification,
                         y = sum.x,
                         yend = sum.y,
                         color = "high_noise"),
                     size = 7
                     ) +
        scale_color_manual(values = extra_palette) +
        labs(x = NULL, y = NULL, color = NULL) +
        theme(axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              text = element_text(family = "Times", size = 20, face = "bold"))
    ## ------------------------------------- 
    ## do coord. axis cut off
    ps1 <- ps + 
      coord_flip(ylim = c(50, 500)) +
      geom_hline(yintercept = c(50), linetype = "dashed", size = 0.7,
                 color = "grey") +
      scale_y_continuous(breaks = c(50, 100, 200, 300, 400, 500))
    ps2 <- ps +
      coord_flip(ylim = c(900, 1300)) +
      scale_y_continuous(breaks = c(1000, 1200))
    ps <- ggpubr::ggarrange(ps1, ps2, ncol = 2, nrow = 1,
                            widths = c(2/3, 1/3),
                            common.legend = TRUE, legend = "right", align = "h")
    ## ---------------------------------------------------------------------- 
    ## ---------------------------------------------------------------------- 
    ## ---------------------------------------------------------------------- 
    svg(savename, width = width, height = height)
    grid.newpage()
    pushViewport( viewport(layout = grid.layout(1000, 200) ))
    ## ------------------ 
    ## classification
    ## while 2 line of bottom legend, set to 923
    adjust <- 940
    print( pa1, vp = viewport(layout.pos.row = 30:adjust, layout.pos.col = 1:l_ratio))
    ## cluster accuracy
    print( p, vp = viewport(layout.pos.row = 3:1000, layout.pos.col = (l_ratio + 2):m_ratio))
    ## compounds number
    print( ps, vp = viewport(layout.pos.row = 30:adjust, layout.pos.col = (m_ratio + 4):195))
    ## ------------------ 
    dev.off()
    return()
    ## ---------------------------------------------------------------------- 
    ## ---------------------------------------------------------------------- 
    ## ---------------------------------------------------------------------- 
  }
## difine a function
mutate2.extra.horizon.tmp_merge <- 
  function(
           v1 = "noise",
           v2 = "h_noise",
           extra_list
           ){
    df <- merge(extra_list[[v1]], extra_list[[v2]],
                              by = "classification", all.x = T) %>% 
      merge(extra_list[["origin"]], by = "classification", all.y = T) %>% 
    return(df)
  }
mutate2.horizon.tmp_merge <-
  function(
           v1,
           v2,
           df_list
           ){
  df <- merge(df_list[[v1]], df_list[[v2]],
                    by = "classification", all.x = T) %>% 
  dplyr::mutate(flow1 = "true", flow2 = "latent") %>% 
  ## ------------------------------------- 
  ## both true and false is changing, so duplicated the col
  reshape2::melt(., id.vars = colnames(.)[!colnames(.) %in% c("flow1", "flow2")],
                 variable.name = "type",
                 value.name = "value") %>% 
  ## ------------------------------------- 
  ## calculate segment from y to yend
  dplyr::mutate(., y = as.numeric(apply(., 1, function(v){
                                          v[[paste0("en.", v[["value"]], ".x")]]
                                            })),
                yend = as.numeric(apply(., 1, function(v){
                                          v[[paste0("en.", v[["value"]], ".y")]]
                                            })),
                exclude = ifelse(is.na(yend), T, F),
                y = ifelse(is.na(yend), 0, y),
                yend = ifelse(is.na(yend), 1, yend))
  return(df)
}

