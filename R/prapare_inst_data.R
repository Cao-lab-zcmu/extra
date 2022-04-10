prapare_inst_data <- 
  function(
           df,
           raw_path = ".",
           to_path = "~/MCnebula/inst/extdata"
           ){
    df <- df %>% 
      dplyr::filter(tanimotoSimilarity >= 0.8) %>% 
      dplyr::select(.id, rank, score, tanimotoSimilarity, file_name) %>% 
      dplyr::slice(1:5)
    return(df)
  }

