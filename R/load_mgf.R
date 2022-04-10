filter_mgf <- 
  function(
           filter_id = prapare_inst_data(.MCn.structure_set)$.id,
           file = "~/Downloads/msp/msms_pos_gnps.msp.mgf"
           ){
    mgf <- read_msp(file)
    start <- which(mgf$V1 == "BEGIN IONS")
    end <- which(mgf$V1 == "")
    ## ------------------------------------- 
    id <- mgf[grepl("FEATURE_ID", mgf$V1), ]
    id <- stringr::str_extract(id, "(?<==).*$")
    ## ------------------------------------- 
    list <- pbapply::pbmapply(base_mgf_as_list,
                              start,
                              end,
                              MoreArgs = list(mgf = mgf),
                              SIMPLIFY = F)
    names(list) <- id
    if(is.null(filter_id) == F){
      list <- list[names(list) %in% filter_id]
    }
    return(list)
  }
base_mgf_as_list <- 
  function(
           start,
           end,
           mgf
           ){
    df <- dplyr::slice(mgf, start:end)
    return(df)
  }
