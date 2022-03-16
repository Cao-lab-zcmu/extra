filter_via_rownames <- 
  function(
           df,
           row
           ){
    df <- df[rownames(df) %in% row, ]
    return(df)
  }
