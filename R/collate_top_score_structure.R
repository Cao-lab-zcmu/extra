collate_top_score_structure <-
  function(
           path = "."
           ){
    file_set <- list.files(path) %>%
      stringr::str_extract(pattern = "(?<=_)[a-z]{1,50}[0-9]{1,50}$") %>%
      sort()
    id_set <- pbapply::pblapply(file_set, grep_id) %>%
      unlist()
    structure_set <- pbapply::pblapply(id_set, base_collate_top)
    names(structure_set) = id_set
    structure_set <- data.table::rbindlist(structure_set, idcol = T, fill = T)
    return(structure_set)
  }
base_collate_top <-
  function(
           key_id
           ){
    check <- try(df <- get_structure(key_id, return_row = 1), silent = T)
    if(class(check)[1] == "try-error"){
      df <- data.frame()
    }
    return(df)
  }
