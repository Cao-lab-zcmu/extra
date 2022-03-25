mutate_deal_with_msp_record <- 
  function(
           ...
           ){
    args <- list(...,
                 mass_sep = " ",
                 input = c(
                           name = "Name",
                           mass = "PrecursorMZ", 
                           adduct = "Precursor_type",
                           formula = "Formula",
                           rt = "NA"),
                 other = c(
                           "Name", "Synon", "DB#", "InChIKey",
                           "Precursor_type", "Spectrum_type", "PrecursorMZ",
                           "Instrument_type", "Instrument", "Ion_mode",
                           "Collision_energy", "Formula",
                           "MW", "ExactMass", "Comments")
    )
    do.call(deal_with_msp_record, args)
  }
deal_with_msp_record <-
  function(
           string,
           id_prefix,
           cache,
           store,
           mass_level = 2,
           mass_sep = "\t",
           id = get("id", envir = cache),
           input = c(name = "NAME",
                     mass = "PRECURSORMZ",
                     adduct = "PRECURSORTYPE",
                     formula = "FORMULA",
                     rt = "RETENTIONTIME"),
           other = c("NAME", "PRECURSORMZ", "PRECURSORTYPE",
                     "FORMULA", "Ontology", "INCHIKEY", "SMILES",
                     "RETENTIONTIME", "CCS", "IONMODE",
                     "INSTRUMENTTYPE","INSTRUMENT",
                     "COLLISIONENERGY", "Comment", "Num Peaks"),
           output = c(begin = "BEGIN IONS",
                     id = "FEATURE_ID=",
                     mass = "PEPMASS=",
                     charge = "CHARGE=",
                     rt = "RTINSECONDS=",
                     level = "MSLEVEL=",
                     end = "END IONS")
           ){
    ## ---------------------------------------------------------------------- 
    ## get name and value
    name = get_name(string)
    name = ifelse(is.na(name) == T, "", name)
    if(grepl("^[A-Z]", name) == T){
      value = get_value(string)
    }
    ## ---------------------------------------------------------------------- 
    cat = 0
    if(name == input[["name"]]){
      catapp(output[["begin"]], "\n")
      ## id update
      id = id + 1
      assign("id", id, envir = cache)
      assign("ion", 1, envir = cache)
      ## output
      cat = 1
      p = output[["id"]]
      s = paste0(id_prefix, id)
      ## new var in envir: store
      info <- data.table::data.table(.id = s, name = value)
      assign(paste0(id), info, envir = store)
    ## ---------------------------------------------------------------------- 
    }else if(name == input[["mass"]]){
      cat = 1
      p = output[["mass"]]
      s = value
    ## ---------------------------------------------------------------------- 
    }else if(name == input[["adduct"]]){
      cat = 1
      p = output[["charge"]]
      s = ifelse(grepl("]-|]+", value) == F, "0",
                 ifelse(grepl("]-", value), "-1", "+1"))
      id <- get("id", envir = cache)
      info = get(paste0(id), envir = store)
      info[["charge"]] = s
      assign(paste0(id), info, envir = store)
      assign("adduct", value, envir = cache)
    ## ---------------------------------------------------------------------- 
    }else if(name == input[["formula"]]){
      assign("formula", value, envir = cache)
    ## ---------------------------------------------------------------------- 
    }else if(name == input[["rt"]]){
      cat = 1
      p = output[["rt"]]
      s = value
    ## ---------------------------------------------------------------------- 
    }else if(name == "Num Peaks"){
      cat = 0
      id <- get("id", envir = cache)
      info = get(paste0(id), envir = store)
      ## ------------------------------------- 
      if(mass_level == "all"){
        catapp(output[["level"]], "1\n")
        ## ------------------ 
        catapp(info[["PRECURSORMZ"]], "100\n", sep = " ")
        ## here, use rcdk to simulate calculate the isotope pattern
        adduct <- get("adduct", envir = cache)
        if(grepl("FA|ACN", adduct)){
          adduct <- gsub("FA", "CO2H2", adduct)
          adduct <- gsub("ACN", "C2H3N", adduct)
        }
        if(adduct != "[M+H-99]+"){
          formula <- get("formula", envir = cache)
          ## according to adduct to revise formula
          formula <- formula_reshape_with_adduct(formula, adduct)
          ## rcdk function
          array <- get.isotopes.pattern(get.formula(formula))
          apply(array, 1, cat_isotope)
        }
        ## ------------------ 
        catapp(output[["end"]], "\n")
        catapp("\n")
        ## begin mass level 2
        catapp(output[["begin"]], "\n")
        catapp(output[["id"]], info[[".id"]], "\n")
        catapp(output[["mass"]], info[["PRECURSORMZ"]], "\n")
        catapp(output[["charge"]], info[["charge"]], "\n")
      }
      ## ------------------------------------- 
      catapp(output[["rt"]], info[["RETENTIONTIME"]], "\n")
      catapp(output[["level"]], "2\n")
    ## ---------------------------------------------------------------------- 
    }else if(grepl("^[0-9]", string)){
      cat = 2
      p = get_name(string, sep = mass_sep)
      s = get_value(string, sep = mass_sep)
    }else if(string == ""){
      ion <- get("ion", envir = cache)
      if(ion == 0){
        return()
      }
      assign("ion", 0, envir = cache)
      cat = 1
      p = output[["end"]]
      s = "\n"
    }
    ## ---------------------------------------------------------------------- 
    if(cat == 1){
      catapp(p, s, "\n")
    }else if(cat == 2){
      catapp(p, s, "\n", sep = " ")
    }
    ## ---------------------------------------------------------------------- 
    ## data store
    if(name %in% other == T){
      id <- get("id", envir = cache)
      info = get(paste0(id), envir = store)
      info[[name]] = value
      assign(paste0(id), info, envir = store)
    }
    return()
    ## ---------------------------------------------------------------------- 
    ## output
  }
catapp <-
  function(
           ...,
           sep = "",
           mgf = get("mgf", envir = get("envir_meta"))
           ){
    cat(paste(..., sep = sep), file = mgf, append = T)
  }
cat_isotope <- 
  function(
           vector
           ){
    catapp(vector[1], vector[2] * 100, "\n", sep = " ")
  }
get_value <-
  function(
           string,
           sep = ": "
           ){
    string <- unlist(strsplit(string, split = sep))
    return(string[2])
  }
get_name <-
  function(
           string,
           sep = ": "
           ){
    string <- unlist(strsplit(string, split = sep))
    return(string[1])
  }
