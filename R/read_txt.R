read_txt <- 
  function(
           path,
           ...
           ){
    file <- data.table::fread(path, sep = NULL, header = F, quote = "")
    return(file)
  }
