mutate_inchi_curl_syno <- 
  function(
           key_set,
           .id_set,
           dir = "inchi_pub",
           ...
           ){
    if(file.exists(dir) == F)
      dir.create(dir)
    origin <- getwd()
    setwd(dir)
    list <- mapply(data.table, key_set, .id_set, SIMPLIFY = F)
    pbapply::pblapply(list, function(df, ...){
                        inchi_curl_syno(df[[1]], df[[2]], ...)}, ..., cl = 20)
    setwd(origin)
  }
inchi_curl_syno <- 
  function(
           key,
           .id,
           type = "inchikey",
           get = "synonyms",
           save = paste0(.id, ".xml")
           ){
    http = paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/", type, "/")
    http_end = paste0("/", paste(get, collapse = ","), "/XML > ")
    curl <- "curl -s --connect-timeout 20 --retry 100 --retry-delay 30 "
    curl_http <- paste0(curl, http)
    system(paste0(curl_http, key, http_end, save))
  }

