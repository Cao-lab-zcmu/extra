as_pca_df <- 
  function(
           df
           ){
    rownames(df) <- df[[1]]
    df <- df %>%
      t()
    df <- df[-1,]
    rownames(df) <- rownames(df) %>%
      gsub(" Peak area", "", .)
    return(df)
  }
adjust_extremity <- 
  function(
           df
           ){
    df <- dplyr::summarise_all(df, add_1)
    return(df)
  }
add_1 <-
  function(
           x
           ){
    x <- x + 1
    return(x)
  }
