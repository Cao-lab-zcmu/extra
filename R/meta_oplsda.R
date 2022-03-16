meta_oplsda <- 
  function(
           df,
           metadata,
           GROUP
           ){
    metadata <- metadata %>%
      dplyr::filter(group %in% all_of(GROUP)) %>%
      dplyr::select(sample, group)
    ## ---------------------------------------------------------------------- 
    ## collate data
    df <- filter_via_rownames(df, metadata$sample) %>%
      data.frame() %>%
      dplyr::mutate(sample = rownames(.)) %>%
      ## in order to sort sample with group
      merge(metadata, by = "sample", all.x = T)
    ## select the col of var
    gr <- grepl("sample|group", colnames(df))
    ## scale the data
    matrix <- meta_scale(df[, !gr])
    ## ----------------------------------------------------------------------
    ## opls-da
    cat("## calculate OPLS-DA...\n")
    oplsda <- ropls::opls(x = matrix, y = unlist(df[, "group"]),
                          predI = 1, orthoI = NA)
    ## ---------------------------------------------------------------------- 
    ## gather opls-da results
    ## T score
    tscore <- oplsda@modelDF[1, "R2X"] * 100
    ## O score
    oscore <- oplsda@modelDF[2, "R2X"] * 100
    ## ------------------------------------- 
    ## collate as df
    part1 <- data.table(h1 = oplsda@scoreMN[, 1], o1 = oplsda@orthoScoreMN[, 1]) %>%
      dplyr::bind_cols(df[, gr]) %>%
      dplyr::mutate(x_lab = paste0("T score[1] (", tscore, "%)"),
                    y_lab = paste0("Orthogonal T score[1] (", oscore, "%)"))
      dplyr::as_tibble()
    ## ---------------------------------------------------------------------- 
    ## gather VIP value
    part2 <- data.frame(oplsda@vipVn) %>%
      ## get id
      dplyr::mutate(id = rownames(.)) %>%
      ## rename
      dplyr::rename(vip = oplsda.vipVn) %>%
      dplyr::as_tibble()
    ## ---------------------------------------------------------------------- 
    list <- list(oplsda_coord = part1, vip = part2)
    ## -------------------------------------
    return(list)
  }
