pretty_table <- 
  function(
           df,
           title = "compounds summary",
           subtitle = "LC-MS",
           footnote = "Compounds summary",
           font = "Times",
           filename = "tmp.html",
           path = ".",
           return_gt = T,
           shorter_name = T,
           spanner = F,
           default = F
           ){
    ## ---------------------------------------------------------------------- 
    title = paste0("**", Hmisc::capitalize(title), "**")
    subtitle = paste0("**", Hmisc::capitalize(subtitle), "**")
    colnames(df) <- Hmisc::capitalize(colnames(df))
    ## ---------------------------------------------------------------------- 
    if(default == F){
      t <- base_gt_solid_line(df, title = title, subtitle = subtitle,
                              footnote = footnote, font = font)
    }
    ## ------------------------------------- 
    if(default == T){
      t <- gt(df) %>%
        opt_table_font(font=list(font)) %>%
        tab_header(title = md(title),
                   subtitle = md(subtitle)) %>%
        opt_align_table_header(align = "left") %>%
        tab_footnote(footnote = footnote,
                     locations = cells_title(groups = c("title")))

    }
    ## ---------------------------------------------------------------------- 
    if(shorter_name == T){
      t <- t %>%
        cols_width(Name ~ px(300))
    }
    ## ---------------------------------------------------------------------- 
    if(spanner == T){
      columns <- colnames(df) %>% 
        .[grepl("#", .)]
      t <- t %>%
        tab_spanner_delim(columns = columns,
                          delim = "#")
    }
    ## ---------------------------------------------------------------------- 
    gtsave(t, filename, path)
    if(return_gt == T)
      return(t)
  }
## ---------------------------------------------------------------------- 
mapply_rename_col <- 
  function(
           mutate_set,
           replace_set,
           names
           ){
    envir <- environment()
    mapply(base_mapply_rename_col, mutate_set, replace_set,
           MoreArgs = list(envir = envir))
    return(names)
  }
base_mapply_rename_col <- 
  function(
           mutate,
           replace,
           envir,
           names = get("names", envir = envir)
           ){
    names <- gsub(mutate, replace, names)
    assign("names", names, envir = envir)
  }
## ---------------------------------------------------------------------- 
# add_spanner <- 
#   function(
#            t,
#            names,
#            group,
#            col_spanner
#            ){
#     envir <- environment()
#     mapply(base_add_spanner, group, col_spanner,
#            MoreArgs = list(envir = envir))
#     return(t)
#   }
# base_add_spanner <- 
#   function(
#            the_group,
#            spanner,
#            envir,
#            names = get("names", envir = envir),
#            t = get("t", envir = envir)
#            ){
#     columns <- names %>%
#       .[grepl(the_group, .)]
#     t <- t %>%
#       tab_spanner(label = spanner,
#                   columns = columns)
#     assign("t", t, envir = envir)
#   }
## ---------------------------------------------------------------------- 
base_gt_solid_line <- 
  function(
           df,
           title,
           subtitle,
           footnote,
           font
           ){
    t <- gt(df) %>%
      opt_table_font(font=list(font)) %>%
      tab_header(title = md(title),
                 subtitle = md(subtitle)) %>%
      opt_align_table_header(align = "left") %>%
      tab_footnote(footnote = footnote,
                   locations = cells_title(groups = c("title"))) %>%
      opt_table_lines(extent = c("none")) %>%
      cols_align(align = "left",
                 columns = everything()) %>%
      tab_style(style = cell_borders(sides = c("top", "bottom"),
                                     color = "black",
                                     weight = px(1.5),
                                     style = "solid"),
                locations = cells_column_labels()) %>%
      tab_style(style = cell_text(v_align="top"),
                locations = cells_column_labels(columns = everything())) %>%
      tab_style(style = cell_borders(sides = c("bottom"),
                                     color = "black",
                                     weight = px(1.5),
                                     style = "solid"),
                locations = cells_body(columns=everything(),
                                       rows=nrow(df))) %>%
      tab_style(style = cell_text(v_align="top"),
                locations = cells_body(columns = everything()))
    return(t)
  }
