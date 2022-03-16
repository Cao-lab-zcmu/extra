meta_compound_filter <- 
  function(
           list,
           vip,
           dose = "high",
           l_abs_log_fc = 1, ## or 0
           l_q_value = 0.05, ## or 1
           l_vip = 1, ## or 0
           id_fix = T,
           round = T
           ){
    if(id_fix == T)
      vip <- dplyr::mutate(vip, id = gsub("X", "", id))
    ## ---------------------------------------------------------------------- 
    set <- data.table::rbindlist(list)
    ## ------------------------------------- 
    if(round == T){
      set <- dplyr::mutate(set, log2.fc = round(log2.fc, 2))
    }
    ## ---------------------------------------------------------------------- 
    ## control and model
    part1 <- dplyr::filter(set, facet_row == "extra") %>%
      merge(vip, by = "id", all.x = T, sort = F) %>%
      dplyr::filter(vip > l_vip,
                    abs(log2.fc) > l_abs_log_fc,
                    q_value < l_q_value) %>%
      dplyr::as_tibble()
    ## ------------------------------------- 
    if(round == T){
      part1 <- dplyr::mutate(part1, vip = round(vip, 2))
    }
    ## ---------------------------------------------------------------------- 
    ## drug dispose
    part2 <- dplyr::filter(set, facet_row == "high") %>%
      by_group_as_list("facet_col") %>%
      ## ------------------ rename the col
      lapply(., meta_rename_prefix,
             col = c("p_value", "q_value", "log2.fc"),
             prefix_from_col = "facet_col", internal = "#") %>%
      ## ------------------ select the needed col
      lapply(., dplyr::select,
             id, contains("#")) %>%
      ## ------------------ merge into a data.frame
      meta_merge_list(col = "id") %>%
      dplyr::as_tibble()
    ## ---------------------------------------------------------------------- 
    ## gather data
    df <- part1 %>%
      dplyr::select(id, vip) %>%
      merge(., part2, by = "id", all.x = T, sort = F) %>%
      dplyr::filter(abs(`raw_model#log2.fc`) > 1 | abs(`pro_model#log2.fc`) > 1) %>%
      dplyr::as_tibble()
    return(df)
  }
meta_rename_prefix <- 
  function(
           df,
           col,
           prefix = NA,
           prefix_from_col = NA,
           internal = "."
           ){
    if(is.na(prefix_from_col) == F){
      prefix <- paste0(df[1, ][[prefix_from_col]], internal)
    }
    colnames(df) <- base_meta_rename_prefix(colnames(df),
                                            mutate = col,
                                            PREFIX = prefix)
    return(df)
  }
base_meta_rename_prefix <- 
  function(
           names,
           mutate,
           PREFIX
           ){
    df <- data.table::data.table(origin = names) %>%
      dplyr::mutate(change = ifelse(origin %in% mutate,
                                    paste0(PREFIX, origin),
                                    origin))
    return(df$change)
  }
meta_merge_list <- 
  function(
           list,
           col = "id"
           ){
    df <- list[[1]]
    for(i in 2:length(list)){
      df <- merge(df, list[[i]], by = col, all.x = T, sort = F)
    }
    return(df)
  }
