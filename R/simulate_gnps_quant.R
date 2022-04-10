simulate_gnps_quant <- 
  function(
           meta,
           save_path,
           file = paste0(save_path, "/", "quant.csv"),
           rt = 1000,
           area = 10000,
           id = ".id",
           mz = "PRECURSORMZ",
           simu_id = "row ID",
           simu_mz = "row m/z",
           simu_rt = "row retention time",
           simu_quant = "sample.mzML Peak area",
           return_df = F
           ){
    meta <- dplyr::select(meta, all_of(c(id, mz)))
    meta <- dplyr::mutate(meta, rt = rt, sample = area)
    colnames(meta) <- colnames(meta) %>% 
      .meta_find_and_sort(., c(id, mz, "rt", "sample")) %>% 
      mapply_rename_col(., c(simu_id, simu_mz, simu_rt, simu_quant),
                        colnames(meta))
    if(return_df)
      return(meta)
    write.table(meta, file = file, sep = ",", row.names = F, col.names = T, quote = F)
  }
