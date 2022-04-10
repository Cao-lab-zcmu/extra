collate_as_noise_pool <- 
  function(
           origin_list,
           valid_list
           ){
    ## ---------------------------------------------------------------------- 
    ## filter origin_list
    args <- list(list = origin_list, discard_level1 = T, only_peak_info = T, mass_shift = F)
    ## get mz and intensity
    cat("## Catch main peak information\n")
    origin_list <- do.call(spectrum_add_noise, args)
    ## discard the NULL data
    cat("## Discard empty dataset\n")
    origin_list <- pbapply::pblapply(origin_list, is.data.frame) %>% 
      unlist(use.names = F) %>% 
      origin_list[.]
    ## ---------------------------------------------------------------------- 
    ## order the origin_list and valid_list according to .id
    ## first, filter the origin_list, only the .id in valid_list is reserved.
    origin_list <- origin_list[names(origin_list) %in% names(valid_list)]
    ## keep identical
    valid_list <- valid_list[names(valid_list) %in% names(origin_list)]
    ## order
    cat("## Order the lists...\n")
    origin_list <- order_list(origin_list)
    valid_list <- order_list(valid_list)
    ## ---------------------------------------------------------------------- 
    ## list merge (use mapply)
    cat("## Merge to get noise list\n")
    noise_list <- pbapply::pbmapply(numeric_round_merge, origin_list, valid_list,
                                    main_col = "mass", sub_col = "mz",
                                    mz.tol = 0.002, noise = T, SIMPLIFY = F)
    ## ------------------------------------- 
    noise_df <- data.table::rbindlist(noise_list, fill = T)
    ## ------------------------------------- 
    noise_df <- mutate(noise_df, mass = as.numeric(mass), inte = as.numeric(inte))
    ## ------------------------------------- 
    return(noise_df)
    ## ---------------------------------------------------------------------- 
  }
load_all_valid_spectra <- 
  function(
           formula_adduct = .MCn.formula_set,
           path = .MCn.sirius
           ){
    cat("Collate as metadata\n")
    metadata <- list.files(path = path, pattern = "^[0-9]{1,}_(.*)_(.*)[0-9]{1,}$") %>% 
      data.table::data.table(dir = .) %>% 
      dplyr::mutate(.id = stringr::str_extract(dir, "(?<=_)[^_]{1,}$")) %>% 
      merge(formula_adduct, by = ".id", all.x = T) %>% 
      dplyr::select(.id, dir, precursorFormula, adduct) %>% 
      dplyr::mutate(adduct = gsub(" ", "", adduct),
                    file = paste0(path, "/", dir, "/spectra/", precursorFormula, "_", adduct, ".tsv"),
                    exists = unlist(pbapply::pblapply(file, file.exists), use.names = F)) %>% 
      dplyr::filter(exists == T)
    ## ------------------------------------- 
    cat("Read file and collate\n")
    list <- pbapply::pblapply(metadata$file, read_tsv) %>% 
      pbapply::pblapply(dplyr::select, mz, rel.intensity)
    ## ------------------------------------- 
    names(list) <- metadata$.id
    return(list)
  }
