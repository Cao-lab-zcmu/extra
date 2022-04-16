stat_results_class <- 
  function(
           ## only .id is needed
           df,
           standard,
           path = ".",
           class_cutoff = 4
           ){
    ## ------------------------------------- 
    if(is.data.frame(df) == F)
      return()
    db <- dplyr::filter(.MCn.class_tree_data, hierarchy >= class_cutoff)
    ## ------------------------------------- 
    ## for name to get id
    db_id <- lapply(db$id, c)
    names(db_id) <- db$name
    ## for id to get parentId
    db_parent <- lapply(db$parentId, c)
    names(db_parent) <- db$id
    ## for id to get name
    db_name <- lapply(db$name, c)
    names(db_name) <- db$id
    ## ------------------------------------- 
    set <- get_parent_class(standard,
                            db_id,
                            db_parent,
                            db_name)
    ## ------------------------------------- 
    cat("stat:", standard, "\n")
    id_set <- df[[".id"]]
    list <- pbapply::pblapply(id_set, base_stat_results_class,
                      standard = standard,
                      set = set,
                      path = path)
    df <- data.table::rbindlist(list)
    return(df)
  }
base_stat_results_class <- 
  function(
           id,
           set,
           path = ".",
           standard
           ){
    check <- try(class <- read_tsv(paste0(path, "/", id)), silent = T)
    if(class(check)[1] == "try-error"){
      stat <- data.table::data.table(id = id, evaluate = NA)
      return(stat)
    }
    if(standard %in% class[["Classification"]]){
      stat <- data.table::data.table(id = id, evaluate = "true")
    }else{
      if(class[3,]$Classification %in% set){
        ## at least the cluster is T in "class" level
        evaluate <- "latent"
      }else{
        evaluate <- "false"
      }
      stat <- data.table::data.table(id = id, evaluate = evaluate)
    }
    return(stat)
  }

