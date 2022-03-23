collate_comment <- 
  function(
           vector,
           names
           ){
    list <- pbapply::pblapply(vector, base_collate_comment)
    names(list) <- names
    return(list)
  }
base_collate_comment <- 
  function(
           string
           ){
    ch <- strsplit(string, split = "\" \"")
    ch <- unlist(ch)
    ch <- gsub("\"", "", ch)
    ch <- sub("=", "###", ch)
    ch <- strsplit(ch, split = "###")
    ch <- lapply(ch, function(str){
               df <- data.table::data.table(comment = str[1], record = str[2])
               return(df)
           })
    ch <- data.table::rbindlist(ch)
    return(ch)
  }
