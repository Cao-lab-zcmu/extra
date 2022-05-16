pretty_table <- 
  function(
           df,
           title = "compounds summary",
           subtitle = "LC-MS",
           footnote = "Compounds summary",
           font = "Times New Roman",
           filename = "tmp.html",
           path = ".",
           return_gt = T,
           shorter_name = T,
           spanner = F,
           group = T,
           default = F
           ){
    ## ---------------------------------------------------------------------- 
    title = paste0("**", Hmisc::capitalize(title), "**")
    subtitle = paste0("**", Hmisc::capitalize(subtitle), "**")
    colnames(df) <- Hmisc::capitalize(colnames(df))
    ## ---------------------------------------------------------------------- 
    if(!default){
      t <- base_gt_solid_line(df, title = title, subtitle = subtitle,
                              footnote = footnote, font = font, group = group)
    }
    ## ------------------------------------- 
    if(default){
      if(group){
        t <- gt(df, groupname_col = "Info")
      }else{
        t <- gt(df)
      }
      t <- t %>%
        opt_table_font(font=list(font)) %>%
        tab_header(title = md(title),
                   subtitle = md(subtitle)) %>%
        opt_align_table_header(align = "left") %>%
        tab_footnote(footnote = footnote,
                     locations = cells_title(groups = c("title")))
    }
    ## ---------------------------------------------------------------------- 
    if(shorter_name){
      t <- t %>%
        cols_width(Name ~ px(300))
    }
    ## ---------------------------------------------------------------------- 
    if(spanner){
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
    names <- gsub(mutate, replace, names, perl = T)
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
           font,
           group
           ){
    ## ------------------------------------- 
    if(group){
      ## final group, the end row names
      tmp <- dplyr::mutate(df, row = 1:nrow(df))
      end_row <- dplyr::filter(tmp, Info == tail(unique(tmp$Info), n = 1))$row %>% 
        tail(n = 1)
      t <- gt(df, groupname_col = "Info")
    }else{
      t <- gt(df)
      end_row <- nrow(df)
    }
    t <- t %>%
      ## ------------------------------------- 
      ## set font
      opt_table_font(font=list(font)) %>%
      ## ------------------------------------- 
      ## set title or footnote etc., annotation
      tab_header(title = md(title),
                 subtitle = md(subtitle)) %>%
      tab_footnote(footnote = footnote,
                   locations = cells_title(groups = c("title"))) %>%
      ## ------------------------------------- 
      ## set alignment
      opt_align_table_header(align = "left") %>%
      cols_align(align = "left",
                 columns = everything()) %>%
      tab_style(style = cell_text(v_align = "top"),
                locations = cells_column_labels(columns = everything())) %>%
      tab_style(style = cell_text(v_align = "top"),
                locations = cells_body(columns = everything())) %>%
      ## ------------------------------------- 
      ## set lines
      opt_table_lines(extent = c("none")) %>%
      ## head lines
      tab_style(style = cell_borders(sides = c("top", "bottom"),
                                     color = "black",
                                     weight = px(1.5),
                                     style = "solid"),
                locations = cells_column_labels()) %>%
      ## end lines
      tab_style(style = cell_borders(sides = c("bottom"),
                                     color = "black",
                                     weight = px(1.5),
                                     style = "solid"),
                locations = cells_body(columns = everything(),
                                       rows = eval(parse(text = end_row)))) %>% 
      ## ------------------------------------- 
      ## set group rows
      tab_style(style = cell_text(align = "center",
                                  weight = "bold"),
                locations = cells_row_groups(groups = everything())) %>% 
      ## set lines
      tab_style(style = cell_borders(sides = c("top", "bottom"),
                                     color = "grey",
                                     weight = px(1),
                                     style = "solid"),
                locations = cells_row_groups(groups = everything()))
    return(t)
  }
