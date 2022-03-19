inchikey_get_formula <- 
  function(
           inchikey
           ){
    df <- data.table::data.table(sp.id = as.character(1:length(inchikey)), inchikey = inchikey)
    ## ------------------------------------- 
    args <- list(df$inchikey, df$sp.id, get = c("MolecularFormula", "ExactMass", "MonoisotopicMass"))
    do.call(mutate_inchi_curl, args)
    from_csv <- gather_inchi_curl()
    system("rm -r inchi_pub")
    ## ------------------------------------- 
    df <- merge(df, from_csv, by = "sp.id", all.x = T)
    return(df)
  }
gather_inchi_curl <- 
  function(
           path = "inchi_pub"
           ){
    file_set <- list.files(path = path, pattern = "csv$", full.names = T)
    list <- lapply(file_set, mutate_fread)
    names(list) <- file_set %>% 
      stringr::str_extract("(?<=/)[0-9]{1,100}")
    df <- data.table::rbindlist(list, idcol = T, fill = T) %>% 
      dplyr::rename(sp.id = .id)
    return(df)
  }
mutate_fread <- 
  function(
           path
           ){
    check <- try(df <- fread(path, fill = T), silent = T)
    if(class(check)[1] == "try-error")
      df <- fread(path, fill = T, skip = 3)
    if("V1" %in% colnames(df)){
      print(path)
      return()
    }
    return(df)
  }
mutate_inchi_curl <- 
  function(
           key_set,
           .id_set,
           dir = "inchi_pub",
           ...
           ){
    if(file.exists(dir) == F)
      dir.create(dir)
    list <- mapply(data.table, key_set, .id_set, SIMPLIFY = F)
    pbapply::pblapply(list, function(df, ...){
                        setwd(dir)
                        inchi_curl(df[[1]], df[[2]], ...)}, ..., cl = 8)
  }
inchi_curl <- 
  function(
           key,
           .id,
           type = "inchikey",
           get = "InChIkey",
           save = paste0(.id, ".csv")
           ){
    http = paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/", type, "/")
    http_end = paste0("/property/", paste(get, collapse = ","), "/CSV > ")
    curl <- "curl -s --connect-timeout 20 --retry 100 --retry-delay 30 "
    curl_http <- paste0(curl, http)
    system(paste0(curl_http, key, http_end, save))
  }
int_inchi_curl <- 
  function(
           seq,
           type = "inchikey",
           get = "InChIkey",
           ...
           ){
    init <- dload[seq, "init"]
    .id <- dload[seq, ".id"]
    save <- paste0(.id, ".csv")
    while(class(try(read.csv(save), silent = T))[1] == "try-error"){
      inchi_curl(init, .id, 
                 get = get,
                 type = type,
                 ...)
    }
  }
