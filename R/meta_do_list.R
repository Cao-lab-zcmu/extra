meta_do_list <- 
  function(
           metadata
           ){
    list <- by_group_as_list(metadata, "group")
    list <- lapply(list, select, sample) %>%
      lapply(unlist) %>%
      lapply(unname)
    return(list)
  }
