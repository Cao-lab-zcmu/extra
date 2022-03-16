meta_get_couple <- 
  function(
           group
           ){
    compare <- group %>%
      .[which(!. %in% c("blank", "positive"))] %>%
      unique() %>%
      combn(2) %>%
      t() %>%
      data.table::data.table() %>%
      ## exclude group compare in different dosage
      dplyr::filter(stringr::str_extract(V1, "_.*$") == stringr::str_extract(V2, "_.*$") |
                    ## get the control or model group
                    V1 %in% c("control", "model") |
                    V2 %in% c("control", "model"),
                  ## with control, only compare with model
                  !(V1 == "control" & V2 != "model"),
                  !(V2 == "control" & V1 != "model"))
      return(compare)
  }
meta_get_extra_couple <- 
  function(
           compare,
           ...
           ){
    extra_compare_1 <- compare %>%
      dplyr::filter(!((V1 == "control" & V2 =="model") | (V1 == "model" & V2 == "control"))) %>%
      dplyr::mutate(V3 = "control", V4 = "model")
    extra_compare_2 <- extra_compare_1 %>%
      apply(., 1, .meta_muti_add) %>%
      lapply(unlist) %>%
      unique()
    extra_compare_3 <- compare %>%
      apply(., 1, .meta_muti_add) %>%
      lapply(unlist) %>%
      unique() %>%
      .[which(lengths(.) != 2)]
    list <- list(extra_compare_1, extra_compare_2, extra_compare_3)
    names(list) <- c("c1", "c2", "c3")
    return(list)
  }
.meta_muti_add <- 
  function(
           vector,
           dose = c("high", "medium", "low")
           ){
    group <- stringr::str_extract(vector, "^.*(?=_)") %>%
      sort() %>%
      expand.grid(dose) %>%
      dplyr::mutate(combine = paste0(Var1, "_", Var2))
    combine <- c(group$combine, vector) %>%
      unique()
    return(combine)
  }
vector_delete_var <- 
  function(
           vector,
           delete
           ){
    vector <- vector[!vector %in% delete]
    return(vector)
  }
