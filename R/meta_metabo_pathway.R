meta_metabo_pathway <- 
  function(
           export = NA,
           mz_rt = NA,
           p_col = NA,
           extra_entity = NA,
           only_return = F,
           ## from name involves the character rename into the columns
           key = c("mz", "q_value", "log2.fc", "rt"),
           ## note that both key and as_col must be ordered
           as_col = c("m.z", "p.value", "t.score", "r.t"),
           ion_mode = "negative",
           ppm = 10,
           p_cutoff = 0.05,
           db_pathway = "hsa_mfn"
           ){
    ## ---------------------------------------------------------------------- 
    if(is.data.frame(extra_entity)){
      df_mz_rt <- extra_entity
    }else{
      df_mz_rt <- mz_rt %>% 
        dplyr::filter(id %in% export$id) %>% 
        merge(export[, c("id", p_col)], all.x = T, by = "id") %>% 
        dplyr::as_tibble()
    }
    ## ---------------------------------------------------------------------- 
    ## note that this step is setting up to auto find and rename
    colnames(df_mz_rt) <- colnames(df_mz_rt) %>% 
      ## find and sort as order of `key`
      .meta_find_and_sort(., key) %>% 
      ## rename columns of df_mz_rt
      mapply_rename_col(., as_col, colnames(df_mz_rt))
    ## ------------------------------------- 
    ## then, select and relocate (sort) the columns of df
    df <- dplyr::select(df_mz_rt, all_of(as_col)) %>% 
      ## convert rt from min to secounds
      dplyr::mutate(r.t = r.t * 60)
    ## ---------------------------------------------------------------------- 
    ## save to file
    write_tsv(df, file = "tmp.txt")
    ## ------------------------------------- 
    ## get the submit file
    if(only_return == T)
      return(list(id = df_mz_rt, submit = df))
    ## ------------------------------------- 
    cat("## submit to MetaboAnalyst\n")
    print(dplyr::as_tibble(df))
    ## submit to MetaboAnalyst
    mSet <- InitDataObjects("mass_all", "mummichog", FALSE)
    mSet <- SetPeakFormat(mSet, "mprt")
    mSet <- UpdateInstrumentParameters(mSet, ppm, ion_mode, "yes", 0.02);
    mSet <- Read.PeakListData(mSet, "tmp.txt");
    mSet <- SetRTincluded(mSet, "seconds")
    mSet <- SanityCheckMummichogData(mSet)
    mSet <- SetPeakEnrichMethod(mSet, "mum", "v2")
    mSet <- SetMummichogPval(mSet, p_cutoff)
    mSet <- PerformPSEA(mSet, db_pathway, "current", 3 , 100)
    return(mSet)
  }
.meta_find_and_sort <- 
  function(
           name_set,
           pattern_set
           ){
    name_set <- lapply(pattern_set, .meta_mutate_grep_get,
                       string_set = name_set) %>% 
      unlist()
    return(name_set)
  }
.meta_mutate_grep_get <- 
  function(
           pattern,
           string_set
           ){
    string <- string_set %>% 
      .[grepl(pattern, .)]
    return(string)
  }
