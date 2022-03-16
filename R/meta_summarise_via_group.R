# |V1         |V2         |
# |:----------|:----------|
# |control    |model      |
# |model      |pro_high   |
# |model      |pro_low    |
# |model      |pro_medium |
# |model      |raw_high   |
# |model      |raw_low    |
# |model      |raw_medium |
# |pro_high   |raw_high   |
# |pro_low    |raw_low    |
# |pro_medium |raw_medium |
## -------------------------------------
## load into a metadata
meta_summarise_via_group <- 
  function(
           df,
           compare
           ){
    list <- apply(compare, 1, base_meta_summarise_via_group,
                  df = df, simplify = F)
    return(list)
  }
## ---------------------------------------------------------------------- 
# $fc
# [1] "test"

# $p_value
# [1] "test"

# $q_value
# [1] "test"
## ------------------------------------- 
base_meta_summarise_via_group <- 
  function(
           group,
           df
           ){
    GROUP <- mutate_meta_sort(group)
    cat("## computaton of", paste(GROUP, collapse = "/"), "\n")
    ## ------------------ 
    df <- dplyr::filter(df, group %in% all_of(GROUP))
    ## ------------------ 
    x_row <- grep(GROUP[1], df$group)
    ## x versus y
    y_row <- grep(GROUP[2], df$group)
    ## ------------------------------------- 
    cat("## ------------------ log2(FC)\n")
    fc_set <- meta_calculate_couple(df, x_row, y_row, group,
                                    "base_meta_calculate_fc") %>%
      dplyr::rename(log2.fc = expr)
    ## ------------------------------------- 
    cat("## ------------------ t.test\n")
    p_set <- meta_calculate_couple(df, x_row, y_row, group,
                                    "base_meta_calculate_p")
    ## ------------------------------------- 
    cat("## ------------------ FDR\n")
    q_set <- p_set %>%
      dplyr::filter(is.na(expr) == F) %>%
      dplyr::mutate(q_value = fdrtool::fdrtool(expr, statistic = 'pvalue', plot = F)$qval) %>%
      dplyr::rename(p_value = expr)
    ## ------------------------------------- 
    ## gather all data
    df <- q_set %>%
      dplyr::select(id, p_value, q_value) %>%
      merge(fc_set, by = "id", all.x = T, sort = F) %>%
      dplyr::as_tibble()
    ## ------------------------------------- 
    return(df)
  }
## ---------------------------------------------------------------------- 
meta_calculate_couple <- 
  function(
           df,
           x_row,
           y_row,
           group,
           fun
           ){
    fun <- match.fun(fun)
    gr <- grepl("sample|group", colnames(df))
    df <- df[, !gr]
    ## use in multiple function
    data_set <- pbapply::pbapply(df, 2, fun, x_row, y_row)
    ## reformat and then return
    data_set <- data.table::data.table(id = names(data_set), expr = unname(data_set),
                                       facet_col = meta_get_facet_col(group),
                                       facet_row = meta_get_facet_row(group))
    return(data_set)
  }
## ------------------------------------- 
base_meta_calculate_fc <- 
  function(
           vector,
           x_row,
           y_row
           ){
    vector <- vector + 1
    fc <- log2(mean(vector[x_row]) / mean(vector[y_row]))
    return(fc)
  }
## ------------------------------------- 
base_meta_calculate_p <- 
  function(
           vector,
           x_row,
           y_row
           ){
    vector <- vector + 1
    x <- vector[x_row]
    y <- vector[y_row]
    check <- try(stat <- t.test(x, y, var.equal = T, paired = F), silent = T)
    if(class(check) == "try-error"){
      return(NA)
    }
    stat <- stat$p.value
    return(stat)
  }
## ---------------------------------------------------------------------- 
## trans df format
meta_array_to_df <- 
  function(
           compute_df,
           metadata
           ){
    df <- compute_df %>%
      data.frame(check.names = F) %>%
      dplyr::mutate(sample = rownames(.)) %>%
      merge(metadata[, c("sample", "group"), with = F], by = "sample", all.x = T) %>%
      dplyr::select(sample, group, colnames(.)) %>%
      dplyr::as_tibble()
    return(df)
  }
mutate_meta_sort <- 
  function(
           vector,
           levels = c("pro", "raw", "model", "control")
           ){
    df <- data.table::data.table(origin = vector)
    df <- dplyr::mutate(df, mutate = gsub("_.*$", "", origin),
                        mutate = factor(mutate, levels = levels))
    df <- dplyr::arrange(df, mutate)
    return(df$origin)
  }
