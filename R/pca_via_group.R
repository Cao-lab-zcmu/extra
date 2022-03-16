pca_via_group <- 
  function(
           df,
           compare,
           extra_compare,
           db_get_sample,
           ...
           ){
    ## ------------------------------------- 
    if(is.data.frame(compare)){
      cat("## compute dominant compare group\n")
      part1 <- pbapply::pbapply(compare, MARGIN = 1, base_pca_via_group,
                                df = df, meta = db_get_sample,
                                ...)
      part1 <- data.table::rbindlist(part1)
    }
    ## ------------------------------------- 
    if(is.list(extra_compare)){
      cat("## compute extra compare group\n")
      part2 <- pbapply::pblapply(extra_compare, internal_base_pca_via_group,
                                 df = df, meta = db_get_sample,
                                 ...)
      part2 <- data.table::rbindlist(part2)
    }
    ## ------------------------------------- 
    df <- dplyr::bind_rows(part1, part2) %>%
      dplyr::as_tibble()
    return(df)
  }
internal_base_pca_via_group <- 
  function(
           extra_compare_part,
           df,
           meta,
           ...
           ){
    if(is.data.frame(extra_compare_part)){
      part <- pbapply::pbapply(extra_compare_part, MARGIN = 1, base_pca_via_group,
                               df = df, meta = meta, ...)
      part <- data.table::rbindlist(part)
      return(part)
    }
    if(is.list(extra_compare_part)){
      part <- lapply(extra_compare_part, base_pca_via_group,
                                df = df, meta = meta, ...)
      part <- data.table::rbindlist(part)
      return(part)
    }
  }
base_pca_via_group <- 
  function(
           group,
           df,
           meta,
           dose = c("high", "medium", "low"),
           ...
           ){
    ## according to group name to get sample file name
    group <- unique(group)
    list <- meta[names(meta) %in% group]
    sample <- unlist(list)
    ## ---------------------------------------------------------------------- 
    ## facet col (different dispose)
    facet_col <- meta_get_facet_col(group, ...)
    ## facet row (different dosage)
    facet_row <- meta_get_facet_row(group, dose)
    ## ---------------------------------------------------------------------- 
    df <- filter_via_rownames(df, sample)
    ## scale the data
    df <- meta_scale(df)
    ## ------------------ 
    pca <- prcomp(df, scale. = F)
    ## ------------------------------------- 
    ## PC annotation
    summary <- summary(pca)
    ## following, return a vector project
    #     PC1    PC2    PC3
    #  0.2595 0.2190 0.1774
    summary = c(round(summary$importance[2,],4))[1:3]
    ## ---------------------------------------------------------------------- 
    pca_coord <- dplyr::as_tibble(pca$x) %>%
      dplyr::select(PC1, PC2, PC3) %>%
      dplyr::mutate(sample = rownames(df),
                    ## facet annotation
                    facet_col = facet_col, facet_row = facet_row,
                    ## annotation of PC importance
                    im_PC1 = summary[["PC1"]], im_PC2 = summary[["PC2"]],
                    ## importance lengend in figure
                    legend_PC1 = paste0("PC1 (", im_PC1 * 100, "%)"),
                    ## the same as PC1
                    legend_PC2 = paste0("PC2 (", im_PC2 * 100, "%)"),
                    ## calculate the PC importance annotation coord in figure
                    ## x coord
                    anno_x = min(PC1) * (20 / 20),
                    ## y coord
                    anno_y = max(PC2) * (22 / 20),
                    ## pc3 is not considered ruster into figure
                    im_PC3 = summary[["PC3"]])
    ## ---------------------------------------------------------------------- 
    return(pca_coord)
  }
meta_sort <- 
  function(
           vector,
           levels = c("pro", "raw", "model", "control"),
           ...
           ){
    vector <- gsub("_.*$", "", vector) %>%
      vector_delete_var("control") %>%
      unique()
    if(length(vector) == 1)
      vector <- c("control", "model")
    levels = c(levels, vector) %>%
      unique()
    vector <- sort(factor(vector, levels = levels))
    return(vector)
  }
meta_scale <- 
  function(
           df
           ){
    df <- scale(df, center = T, scale = T)
    exclude <- df[1,] %>%
      is.nan()
    df <- df[, !exclude]
    return(df)
  }
multi_extract <- 
  function(
           vector,
           pattern_set
           ){
    vector <- lapply(pattern_set, base_muti_extract,
                     vector = vector)
    vector <- unlist(vector)
    return(vector)
  }
base_muti_extract <-
  function(
           pattern,
           vector
           ){
    character <- stringr::str_extract(vector, pattern)
    return(character)
  }
meta_get_facet_col <- 
  function(
           group,
           ...
           ){
    facet_col <- meta_sort(group, ...) %>%
      paste(collapse = "_")
    ## if multiple group
    if(length(group) >= 3){
      check <- c("control", "model") %in% group
      if(F %in% check == F)
        facet_col <- paste0("multiple_", facet_col)
    }
    return(facet_col)
  }
meta_get_facet_row <- 
  function(
           group,
           dose = c("high", "medium", "low")
           ){
    facet_row <- multi_extract(group, dose) %>%
      sort() %>%
      unique()
    ## if multiple dose
    if(length(facet_row) > 1){
      facet_row <- "multiple"
    }
    ## only control and model
    if(length(facet_row) == 0)
      facet_row <- "extra"
    return(facet_row)
  }
