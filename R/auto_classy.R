gather_classyfire <- 
  function(
           path = "classyfire",
           class = "Indoles and derivatives",
           inchi_df = NA
           ){
    file_set <- list.files(path, pattern = "^[0-9]{1,100}$", full.names = T)
    list <- file_set %>% 
      lapply(read_tsv)
    names(list) <- file_set %>% 
      stringr::str_extract("(?<=/)[0-9]{1,100}$")
    ## ------------------------------------- 
    if(is.na(class) == F){
      list <- list %>% 
        data.table::rbindlist(idcol = T) %>% 
        dplyr::filter(Classification == dplyr::all_of(class)) %>% 
        dplyr::select(.id, Classification) %>% 
        dplyr::rename(classification = Classification)
    } 
    ## ------------------------------------- 
    if(is.data.frame(inchi_df)){ 
      list <- inchi_df %>% 
        dplyr::rename(inchikey = InChIKey) %>% 
        dplyr::mutate(inchi2d = stringr::str_extract(inchikey, "^[A-Z]{1,1000}")) %>% 
        merge(list, by = ".id", all.y = T) %>% 
        dplyr::distinct(inchi2d, .keep_all = T) %>% 
        dplyr::select(inchi2d, classification)
    }
    return(list)
  }
mutate_auto_classy <- 
  function(
           df,
           path = "classyfire",
           ...
           ){
    if(file.exists(path) == F)
      dir.create(path)
    origin = getwd()
    setwd(path)
    auto_classy(df, ...)
    setwd(origin)
  }
auto_classy <- 
  function(
           df,
           ...
           ){
    ## classyfireR
    list <- by_group_as_list(df, ".id")
    pbapply::pblapply(list, base_auto_classy,
                        ...)
  }
base_auto_classy <- 
  function(
           df
           ){
    .id <- df[1,][[".id"]]
    lapply(df[["InChIKey"]], base2_classy,
                      .id = .id)
  }
base2_classy <- 
  function(
           inchi,
           .id
           ){
    ch <- try(read_tsv(paste0(.id)), silent = T)
    if(class(ch) == "try-error"){
      ch <- classyfireR::get_classification(inchi)
    }else{
      return()
    }
    if(is.null(ch)){
      return()
    }else{
      ch <- classyfireR::classification(ch)
      write_tsv(ch, paste0(.id))
    }
  }
