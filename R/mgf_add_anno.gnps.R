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
mgf_add_anno.mistree <- 
  function(
           df
           ){
    mass_level <- df$V1[grepl("MSLEVEL", df$V1)]
    ## ---------------------------------------------------------------------- 
    ## process level 1
    if(mass_level == "MSLEVEL=1"){
      slice_line <- list("1:4", "5", "6:nrow(df)")
      list <- lapply(slice_line, function(lines){
                       list <- slice(df, eval(parse(text = lines)))
                       return(list)
           })
      ## ------------------------------------- 
      ## rt
      rt <- c(V1 = "RTINSECONDS=1000")
      ## spectype
      sp <- c(V1 = "SPECTYPE=CORRELATED MS")
      ## ------------------------------------- 
      ## filename
      filename <- c(V1 = "FILENAME=sample.mzML")
      ## scans
      scans <- c(V1 = "SCANS=-1")
      ## ------------------------------------- 
      ## bind rows
      df <- bind_rows(list[[1]], rt, sp, list[[2]], filename, scans, list[[3]])
      return(df)
    }else{
      ## ---------------------------------------------------------------------- 
      ## process level 2
      slice_line <- list("1:6", "7:nrow(df)")
      list <- lapply(slice_line, function(lines){
                       list <- slice(df, eval(parse(text = lines)))
                       return(list)
           })
      ## ------------------------------------- 
      filename <- c(V1 = "FILENAME=sample.mzML")
      df <- bind_rows(list[[1]], filename, list[[2]])
      return(df)
    }
  }
