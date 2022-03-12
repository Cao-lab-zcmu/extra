roman_convert <- 
  function(
           file,
           from = "Nimbus Roman",
           to = "Times New Roman"
           ){
    txt <- read_txt(file)
    txt <- dplyr::mutate(txt, V1 = gsub(from, to, V1))
    write.table(txt, file = file, sep = "", col.names = F, row.names = F, quote = F)
  }
