meta_get_metadata <- 
  function(
           mzmine_col_peak_area
           ){
    metadata <- mzmine_col_peak_area %>%
      gsub(" Peak area", "", .) %>%
      data.table::data.table(sample = .) %>%
      dplyr::mutate(name = stringr::str_extract(sample, ".*(?=\\.)"),
                    prefix = stringr::str_extract(name, "[^[0-9]]*(?=[0-9])"),
                    identifier = stringr::str_extract(name, "[0-9]{1,100}"),
                    group = unlist(lapply(prefix, sub_data, meta = GROUP)),
                    identifier = paste0(group, "_", identifier),
                    super_group = gsub("_.*$", "", group)) %>%
      dplyr::select(sample, identifier, group, super_group) %>%
      dplyr::arrange(identifier)
    return(metadata)
  }
