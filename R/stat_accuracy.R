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
