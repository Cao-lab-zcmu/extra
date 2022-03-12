get_reference_class_density <- 
  function(
           path = "classyfire"
           ){
    distribution <- show_distribution(path = path)
    ## metadata show all classification
    meta_distribution <- distribution %>%
      dplyr::distinct(Level, Classification) %>%
      dplyr::rename(level = Level, classification = Classification)
    ## classes distribution density table
    table_distribution <- table(distribution$Classification) %>%
      data.table(classification = names(.), sum = unname(.)) %>%
      dplyr::select(classification, sum.N)
    ## merge meta and density stat
    reference_density <- merge(meta_distribution, table_distribution,
                               by = "classification", all.x = T) %>%
      dplyr::filter(is.na(level) == F)
    return(reference_density)
  }
get_reference_class_parent <- 
  function(
           df,
           min_possess = 50
           ){
    test <- df %>%
      filter(sum.N >= min_possess, !level %in% c("superclass", "kingdom"))
    p_test <- mutate_get_parent_class(test$class, this_class = T) %>%
      lapply(end_of_vector) %>%
      unlist() %>%
      unname() %>%
      unique()
    return(p_test)
  }
