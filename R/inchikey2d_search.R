inchikey2d_search <- 
  function(
           inchikey2d,
           db,
           col = "inchikey2D"
           ){
    check <- try(df <- db[which(db[[col]] == inchikey2d), ], silent = T)
    if(class(check)[1] == "try-error")
      return()
    return(df)
  }
