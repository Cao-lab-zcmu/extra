metabo_get_id_via_mz_rt <- 
  function(
           metabo_results,
           mutate_mz_rt
           ){
    metabos <- metabo_results %>%
      lapply(dplyr::rename, mz = Query.Mass, rt = Retention.Time) %>%
      lapply(merge, mutate_mz_rt, by = c("mz", "rt")) %>%
      lapply(dplyr::mutate,
             info = paste0(pathway,  " ---- Gamma: ", Gamma, " ---- Hits.sig: ", Hits.sig)) %>%
      lapply(dplyr::select, id, name, mz, info) %>%
      lapply(dplyr::as_tibble)
  }
