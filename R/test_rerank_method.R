test_rerank_method <- 
  function(
           ## nebula_name
           name,
           ## involve col of .id, standard
           meta
           ){
    # rerank method
    test_structure <- nebula_re_rank(nebula_name = name, top_n = 5)
    ## gather results with reference data
    stat <- merge(test_structure[, c(".id", "inchikey2D")], meta,
                            by = ".id", all.x = T) %>%
      dplyr::mutate(evaluate = ifelse(inchikey2D == standard, "true", "false")) %>%
      table_app()
    print(stat)
    return(stat)
  }

