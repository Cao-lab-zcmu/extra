msp_to_mgf <-
  function(
           name,
           id_prefix,
           path = "~/Downloads/msp/MoNA/",
           write_meta_data = paste0(path, "/", name, ".meta.tsv"),
           fun = "mutate_deal_with_msp_record",
           pre_modify = T
           ){
    if(pre_modify == T){
      system(paste0("sed -i 's/\r//g' ", path, "/", name))
    }
    msp <- read_msp(paste0(path, "/", name))
    cache <- new.env()
    store <- new.env()
    assign("id", 0, envir = cache)
    mgf <- paste0(path, "/", name, ".mgf")
    assign("envir_meta", environment(), envir = parent.env(environment()))
    cat("", file = mgf)
    ms_fun <- match.fun(fun)
    pbapply::pblapply(msp[[1]], ms_fun, 
                      id_prefix = id_prefix,
                      cache = cache,
                      store = store)
    set <- ls(envir = store)
    meta_data <- lapply(set, get_envir_df,
                        envir = store)
    meta_data <- data.table::rbindlist(meta_data, fill = T)
    if(is.null(write_meta_data) == F){
      write_tsv(meta_data, write_meta_data)
    }
    return(meta_data)
  }
read_msp <-
  function(
           filepath
           ){
    msp <- data.table::fread(filepath, sep = NULL, header = F)
  }
get_envir_df <-
  function(
           var,
           envir
           ){
    df <- get(var, envir = envir)
    return(df)
  }
