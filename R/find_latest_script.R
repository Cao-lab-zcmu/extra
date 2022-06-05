find_latest_script <- 
  function(
           path = "~/outline",
           pattern = ".*R$"
           ){
    set <- list.files(path = path,
                     pattern = pattern,
                     full.names = T, recursive = T)
    df <- file.info(set) %>%
      dplyr::mutate(file = rownames(.)) %>% 
      dplyr::relocate(file) %>%
      dplyr::arrange(desc(mtime)) %>% 
      dplyr::as_tibble()
    return(df)
  }
ssource <- 
  function(
           path = "~/outline",
           pattern = ".*R$"
           ){
    script <- find_latest_script(path, pattern)$file[1]
    cat("[INFO]: Latest script is:", script, "\n")
    source(script)
  }
