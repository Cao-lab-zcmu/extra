table_app <- 
  function(
           df,
           col = "evaluate",
           prop = T
           ){
    if(is.data.frame(df) == F)
      return()
    stat <- table(df[[col]])
    if(prop == T)
      stat <- prop.table(stat)
    stat <- dplyr::bind_rows(stat)
    return(stat)
  }
