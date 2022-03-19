meta_gather_pub_classyfire_sirius <- 
  function(
           ## contain sp.id
           pub,
           class,
           ## contain sp.id
           sirius
           ){
    class_anno <- gather_classyfire(class = class, inchi_df = pub)
    ## ------------------------------------- 
    ## annotate sirius results with classification
    simp_candi <- sirius %>% 
      merge(class_anno, by.x = "inchikey2D", by.y = "inchi2d", all.x = T) %>% 
      dplyr::filter(classification == class) %>% 
      dplyr::select(.id, inchikey2D, name, classification, tanimotoSimilarity) %>% 
      dplyr::mutate(sp.id = rownames(.)) %>% 
      dplyr::as_tibble()
    return(simp_candi)
  }
