show_distribution <- 
  function(
           path = ".",
           level = NA
           ){
    set <- list.files(path, pattern = "(.*)[0-9]{1-5}$", full.names = T)
    list <- pbapply::pblapply(set, read_tsv)
    names(list) <- set
    Level = "Level"
    df <- data.table::rbindlist(list, idcol = T, fill = T)
    if(is.na(level) == F){
      df <- dplyr::filter(df, Level %in% all_of(level))
    }
    df <- dplyr::mutate(df, .id = stringr::str_extract(.id, "(?<=/)[a-z]{1,10}[0-9]{1,5}$"))
    df <- dplyr::as_tibble(df)
    return(df)
  }
