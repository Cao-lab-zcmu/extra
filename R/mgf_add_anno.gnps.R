mgf_add_anno.gnps <- 
  function(
           df
           ){
    slice_line <- list("1:3", "4:6", "7:nrow(df)")
    list <- lapply(slice_line, function(lines){
                   list <- slice(df, eval(parse(text = lines)))
                   return(list)
           })
    ## ------------------------------------- 
    ## scans
    scans <- str_extract(list[[1]][2, 1], "[0-9]{1,}$")
    scans <- c(V1 = paste0("SCANS=", scans))
    ## ------------------------------------- 
    ## merge
    merge <- c(V1 = "MERGED_STATS=1 / 1 (0 removed due to low quality, 0 removed due to low cosine)")
    ## ------------------------------------- 
    df <- bind_rows(list[[1]], scans, list[[2]], merge, list[[3]])
    return(df)
  }
