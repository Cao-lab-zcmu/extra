stat_accuracy <- 
  function(
           ## a list contain .id
           dominant_list,
           ## the col are .id, inchikey2D
           structure,
           ## the col are .id, standard
           meta
           ){
    id_stat <- lapply(dominant_list, merge, y = structure,
                             by = ".id",
                             all.x = T)
    id_stat <- lapply(id_stat, merge, y = meta,
                             by = ".id", all.x = T)
    id_stat <- lapply(id_stat, dplyr::mutate,
                             evaluate = ifelse(inchikey2D == standard, "true", "false"))
    table <- lapply(id_stat, table_app) %>%
      data.table::rbindlist(idcol = T) %>%
      dplyr::rename(classification = .id)
    return(table)
  }
