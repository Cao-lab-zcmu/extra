pubmed_query <- 
  function(
           key,
           mindate = 2000,
           maxdate = data.table::year(Sys.Date()),
           retmax = 1000
           ){
    cat("[INFO] step1: RISmed::EUtilsSummary\n")
    res <- RISmed::EUtilsSummary(query = key,
                                 mindate = mindate, maxdate = maxdate, retmax = retmax,
                                 type = "esearch", db = "pubmed", datetype = "ppdt")
    ## ---------------------------------------------------------------------- 
    cat("[INFO] step2: RISmed::EUtilsGet\n")
    res.get <- RISmed::EUtilsGet(res)
    return(res.get)
  }
## ---------------------------------------------------------------------- 
## ---------------------------------------------------------------------- 
## ---------------------------------------------------------------------- 
pubmed_query.meta <- 
  function(
           res.get,
           affi = F,
           author = F,
           mesh = F,
           citation = F,
           metadata = T,
           IF.data = NULL
           ){
    ## cite times
    if(citation){
      cites = RISmed::Citations(res.get) %>% 
        base_pubmed_query.meta("ref")
      return(cites)
    }
    if(affi){
      ## Affiliation, a vector
      extra.affi <- RISmed::Affiliation(res.get) %>% 
        base_pubmed_query.meta("affiliation")
      return(extra.affi)
    }
    if(author){
      ## authors
      extra.author <- RISmed::Author(res.get) %>% 
        base_pubmed_query.meta("name")
      return(extra.author)
    }
    ## meshs, a df 
    if(mesh){
      extra.mesh <- RISmed::Mesh(res.get) %>% 
        lapply(function(df){
                 if(is.data.frame(df))
                   return(df)
           }) %>% 
      data.table::rbindlist(idcol = T) %>% 
      dplyr::rename(id = .id)
      return(extra.mesh)
    }
    ## ---------------------------------------------------------------------- 
    metadata <- data.table::data.table(titles = RISmed::ArticleTitle(res.get),
                                       ## journal name
                                       journal = RISmed::ISOAbbreviation(res.get),
                                       ## institutions
                                       year_pubmed = RISmed::YearPubmed(res.get),
                                       id = RISmed::ArticleId(res.get))
    if(!is.null(IF.data)){
      IF.data <- readxl::read_xlsx(IF.data, skip = 1) %>% 
        dplyr::select(`Full Journal Title`, `Impact Factor`)
      metadata <- merge(metadata, IF.data, by.x = "journal", by.y = "Full Journal Title",
                        all.x = T, sort = F)
    }
    metadata <- dplyr::as_tibble(metadata)
    ## ---------------------------------------------------------------------- 
    return(metadata)
  }
base_pubmed_query.meta <- 
  function(
           list,
           index = "name"
           ){
    df <- list %>% 
      lapply(unlist, use.names = F) %>% 
      lapply(function(vec){
               data.table::data.table(name = vec)
           }) %>% 
      data.table::rbindlist(idcol = T, fill = T) %>% 
      dplyr::rename(id = .id) %>% 
      dplyr::as_tibble()
    ## set colnames
    colnames(df)[2] <- index
    return(df)
  }
