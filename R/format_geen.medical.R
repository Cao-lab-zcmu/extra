# format_geen.medical <- 
  # function(
  #          file = "~/Downloads/pubmed.xlsx",
  #          get_full.text = T,
  #          save_path = paste0("~/Documents/", Sys.time())
  #          ){
  #   metadata <- readxl::read_xlsx(file)
  #   colnames(metadata) <- c("title", "IF", "author.1", "freq.author.1", 
  #                           "author.corr", "freq.author.corr", "email.author.corr",
  #                           "journal", "freq.journal", "publish.year",
  #                           "pmid", "url", "affi.author.1", "freq.affi.author.1")
  #   if(get_full.text){
  #     save_path <- gsub(":", "-", save_path)
  #     ## create dir
  #     if(!file.exists(save_path))
  #       dir.create(save_path)
  #   }
  #   return(metadata)
  # }
