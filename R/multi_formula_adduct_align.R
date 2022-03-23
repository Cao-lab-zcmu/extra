multi_formula_adduct_align <- 
  function(
           list,
           db,
           ...
           ){
    list <- pbapply::pblapply(list, mz_df_align, db = db, ...)
    return(list)
  }
mz_df_align <- 
  function(
           df,
           db,
           col = "mass",
           ...
           ){
    list <- lapply(df[[col]], mz_align, df = db, ...)
    adduct <- by_group_as_list(df, col)
    list <- mapply(mutate_bind_cols, adduct, list, SIMPLIFY = F)
    df <- data.table::rbindlist(list, fill = T)
    return(df)
  }
mutate_bind_cols <- 
  function(
           row,
           df
           ){
    df <- dplyr::bind_cols(row[rep(1, nrow(df)),], df)
    return(df)
  }
