test_rerank_method <- 
  function(
           ## nebula_name
           name,
           ## involve col of .id, standard
           meta,
           top_n = 10,
           ...
           ){
    # rerank method
    test_structure <- nebula_re_rank(nebula_name = name, top_n = top_n, ...)
    ## gather results with reference data
    stat <- merge(test_structure[, c(".id", "inchikey2D")], meta,
                            by = ".id", all.x = T) %>%
      dplyr::mutate(evaluate = ifelse(inchikey2D == standard, "true", "false")) %>%
      table_app()
    cat("## nebula_name:", name, "\n")
    print(stat)
    return(stat)
  }

