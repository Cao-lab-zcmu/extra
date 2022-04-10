## for .mgf data
spectrum_add_noise <- 
  function(
           list,
           cl = 8,
           filter_empty = T,
           ...
           ){
    list <- pbapply::pblapply(list, base_spectrum_add_noise, cl = cl, ...)
    ## ------------------------------------- 
    ## filter the empty spectrum
    if(filter_empty){
      unempty <- lapply(list, is.data.frame) %>% 
        unlist(use.names = F)
      empty <- !unempty
      empty <- list[empty] %>% 
        names() %>% 
        unique()
      list <- list[!names(list) %in% empty]
    }
    ## ------------------------------------- 
    return(list)
  }
base_spectrum_add_noise <- 
  function(
           df,
           discard_level1 = F,
           mass_process_level_1 = F,
           mass_process_level_2 = T,
           ...
           ){
    mass_level <- df$V1[grepl("MSLEVEL", df$V1)]
    ## ------------------------------------- 
    ## process level 1
    if(mass_level == "MSLEVEL=1"){
      if(discard_level1)
        return()
      if(mass_process_level_1)
        df <- mass_process_level_1(df, ...)
      return(df)
      ## ------------------------------------- 
      ## process level 2
    }else{
      if(mass_process_level_2)
        df <- mass_process_level_2(df, ...)
      return(df)
    }
  }
mass_process_level_1 <- 
  function(
           df,
           ...
           ){
    # list <- separate_peak_info(df, ...)
    # list[[2]] <- mass_shift(list[[2]], merge = T)
    # df <- rbindlist(list)
    # return(df)
  }
mass_process_level_2 <- 
  function(
           df,
           mass_shift = T,
           ...
           ){
    list <- separate_peak_info(df, ...)
    if(mass_shift == F)
      return(list)
    list[[2]] <- mass_shift(list[[2]], merge = T, ...)
    ## ------------------------------------- 
    if(length(list) == 2)
      return()
    ## ------------------------------------- 
    df <- rbindlist(list)
    return(df)
  }
separate_peak_info <- 
  function(
           df,
           sep = " ",
           only_peak_info = F,
           ...
           ){
    peak_row <- grep("^[0-9]", df$V1)
    peak_info <- slice(df, peak_row)
    peak_info <- separate(peak_info, col = "V1", into = c("mass", "inte"), sep = sep)
    if(only_peak_info == T)
      return(peak_info)
    list <- list(slice(df, 1:(min(peak_row) - 1)),
                 peak_info,
                 slice(df, (max(peak_row) + 1):nrow(df))
    )
    return(list)
  }
