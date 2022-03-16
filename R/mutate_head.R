mutate_head <- 
  function(
           df,
           row = 10,
           col = 10
           ){
    df <- df[1:row, 1:col]
    return(df)
  }
