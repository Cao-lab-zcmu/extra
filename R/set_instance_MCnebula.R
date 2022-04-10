set_instance_MCnebula <- 
  function(
           path = "~/MCnebula"
           ){
    inst_structure_set <- dplyr::filter(.MCn.structure_set, tanimotoSimilarity >= 0.6) %>% 
      dplyr::select(.id, file_name, score, smiles, tanimotoSimilarity, structure_rank) %>% 
      dplyr::slice(., sample(1:nrow(.), 700))
    ## ---------------------------------------------------------------------- 
    inst_formula_set <- dplyr::filter(.MCn.formula_set, .id %in% inst_structure_set$.id)
    ## ---------------------------------------------------------------------- 
    inst_ppcp_dataset <- .MCn.ppcp_dataset %>% 
      .[names(.) %in% inst_structure_set$.id] %>% 
      lapply(function(df){
               df <- dplyr::mutate(df, V1 = round(V1, 2))
               return(df)
           })
    dir <- getwd()
    setwd(path)
    usethis::use_data(inst_formula_set, overwrite = TRUE)
    usethis::use_data(inst_structure_set, overwrite = TRUE)
    usethis::use_data(inst_ppcp_dataset, overwrite = TRUE)
    setwd(dir)
    cat("Set instance files done\n")
  }
