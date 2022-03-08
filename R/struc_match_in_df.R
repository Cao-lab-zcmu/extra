struc_match_in_df <- 
  function(
           df,
           pattern,
           id_col = "id",
           smiles_col = "SMILES"
           ){
    cat("\n## match:", pattern, "\n")
    list <- pbapply::pblapply(df[[smiles_col]], base_pattern_chem,
                              pattern = pattern)
    df <- data.table::data.table(.id = df[[id_col]],
                                 evaluate = c(unlist(list)))
    return(df)
  }
base_pattern_chem <- 
  function(
           smiles,
           pattern
           ){
    mol <- rcdk::parse.smiles(smiles)
    check <- try(match <- rcdk::matches(pattern, mol, return.matches = F), silent = T)
    if(class(check) == "try-error")
      return(c(X = "is.na"))
    return(match)
  }
