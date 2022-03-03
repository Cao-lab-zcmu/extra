auto_classy <- 
  function(
           df,
           store,
           cache,
           ...
           ){
    ## classyfireR
    list <- by_group_as_list(df, ".id")
    pbapply::pblapply(list, base_auto_classy,
                        store = store,
                        cache = cache,
                        ...)
  }
base_auto_classy <- 
  function(
           df,
           store,
           cache
           ){
    .id <- df[1,][[".id"]]
    lapply(df[["InChIKey"]], base2_classy,
                      .id = .id,
                      store = store,
                      cache = cache)
  }
base2_classy <- 
  function(
           inchi,
           .id,
           store,
           cache
           ){
    ch <- try(get(paste0(.id), envir = cache), silent = T)
    if(class(ch) == "try-error"){
      ch <- classyfireR::get_classification(inchi)
    }else{
      return()
    }
    if(is.null(ch)){
      return()
    }else{
      ch <- classyfireR::classification(ch)
      assign(paste0(.id), ch, envir = store)
      assign(paste0(.id), 1, envir = cache)
    }
  }
