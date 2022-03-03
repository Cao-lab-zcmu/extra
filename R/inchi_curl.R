inchi_curl <- 
  function(
           inchikey,
           .id,
           http = "curl -s --connect-timeout 20 --retry 100 --retry-delay 30 https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/inchikey/",
           http_end = "/property/InChIKey/CSV > ", 
           save = paste0(.id, ".csv")
           ){
    system(paste0(http, inchikey, http_end, save))
  }
int_inchi_curl <- 
  function(
           seq
           ){
    init <- dload[seq, "init"]
    .id <- dload[seq, ".id"]
    save <- paste0(.id, ".csv")
    while(class(try(read.csv(save), silent = T))[1] == "try-error"){
      inchi_curl(init, .id)
    }
  }
