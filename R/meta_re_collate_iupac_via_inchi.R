meta_re_collate_iupac_via_inchi <- 
  function(
           simp_candi,
           name_df,
           export
           ){
    export <- simp_candi %>%
      merge(name_df, by = "sp.id", all.x = T) %>%
      dplyr::mutate(name = ifelse(name == "null", IUPACName, name)) %>%
      dplyr::select(.id, name, classification, tanimotoSimilarity) %>%
      dplyr::rename(id = .id, info = classification) %>%
      merge(export, by = "id", all.y = T) %>%
      dplyr::filter(is.na(name) == F) %>%
      dplyr::arrange(desc(tanimotoSimilarity)) %>%
      dplyr::as_tibble()
    return(export)
  }
