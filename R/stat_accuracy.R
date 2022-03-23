stat_accuracy <- 
  function(
           ## a list contain .id
           dominant_list,
           ## the col are .id, inchikey2D
           structure,
           ## the col are .id, standard
           meta,
           return_id_stat = F
           ){
    id_stat <- lapply(dominant_list, merge, y = structure,
                             by = ".id",
                             all.x = T)
    id_stat <- lapply(id_stat, merge, y = meta,
                             by = ".id", all.x = T)
    id_stat <- lapply(id_stat, dplyr::mutate,
                             evaluate = ifelse(inchikey2D == standard, "true", "false"))
    if(return_id_stat == T)
      return(id_stat)
    table <- lapply(id_stat, table_app) %>%
      data.table::rbindlist(idcol = T) %>%
      dplyr::rename(classification = .id)
    return(table)
  }
stat_topn_candidates_accuracy <- 
  function(
           nebula_name,
           path = "mcnebula_results/candidates",
           meta,
           return_id_stat = F
           ){
    file_set <- lapply(nebula_name, mutate_list_files, path = path) %>% 
      unlist()
    id_stat <- lapply(file_set, read_tsv) %>% 
      lapply(dplyr::select, .id, inchikey2D, structure_rank) %>% 
      lapply(merge, meta, by = ".id", all.x = T) %>% 
      lapply(dplyr::mutate, evaluate = ifelse(inchikey2D == standard, "true", "false")) %>% 
      lapply(dplyr::arrange, .id, desc(evaluate)) %>% 
      lapply(dplyr::distinct, .id, .keep_all = T)
    names(id_stat) <- nebula_name
    if(return_id_stat == T)
      return(id_stat)
    table <- lapply(id_stat, table_app) %>%
      data.table::rbindlist(idcol = T) %>%
      dplyr::rename(classification = .id)
    return(table)
  }
mutate_list_files <- 
  function(
           pattern,
           path
           ){
    files <- list.files(path, pattern, full.names = T)
    return(files)
  }
