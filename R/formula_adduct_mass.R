formula_adduct_mass <- 
  function(
           formula = NA,
           compound_weight = NA,
           iontype = "neg",
           db_adduct = "[M+H]+,[M+K]+,[M+Na]+,[M+H-H2O]+,[M+H-H4O2]+,[M+NH4]+,[M-H]-,[M+Cl]-,[M-H2O-H]-,[M+Br]-,[M+FA-H]-,[M+ACN-H]-"
           ){
    ## ---------------------------------------------------------------------- 
    welement <- c(H = 1.007825,
                  C = 12.0,
                  N = 14.003074,
                  O = 15.994915,
                  F = 18.998403,
                  P = 30.973762,
                  S = 31.972071,
                  Cl = 34.968853,
                  Br = 78.918336,
                  Na = 22.989770,
                  K = 38.963708)
    ## ---------------------------------------------------------------------- 
    db_adduct <- db_adduct %>%
      strsplit(split = ",") %>% 
      unlist()
    ## ------------------------------------- 
    if(iontype == "neg"){
      db_adduct <- db_adduct %>% 
        .[grepl("(?<=\\])-$", ., perl = T)]
    }else{
      db_adduct <- db_adduct %>% 
        .[grepl("(?<=\\])\\+$", ., perl = T)]
    }
    ## ------------------------------------- 
    db_adduct <- db_adduct %>% 
      gsub("FA", "CO2H2", .) %>% 
      gsub("ACN", "C2H3N", .)
    cat(paste0("[INFO] use adduct: ", paste(db_adduct, collapse = " | "), " \n\n"))
    ## ------------------------------------- 
    ## calculate adduct mass
    adduct_mass <- lapply(db_adduct, get_adduct_mass) %>% 
      unlist()
    ## ---------------------------------------------------------------------- 
    if(is.na(compound_weight)){
      compound_weight <- lapply(formula, element_extract) %>% 
        lapply(element_calculate, welement = welement) %>% 
        unlist()
    }
    list <- lapply(compound_weight, function(x, plus){x + plus}, adduct_mass) %>% 
      lapply(function(mass, adduct){data.table::data.table(adduct = adduct, mass = mass)},
             adduct = db_adduct)
    names(list) <- formula
    return(list)
  }
get_adduct_mass <- 
  function(
           adduct
           ){
    ## ------------------------------------- 
    welement <- c(H = 1.007825,
                  C = 12.0,
                  N = 14.003074,
                  O = 15.994915,
                  F = 18.998403,
                  P = 30.973762,
                  S = 31.972071,
                  Cl = 34.968853,
                  Br = 78.918336,
                  K = 38.963708,
                  Na = 22.989770)
    ## ------------------------------------- 
    com <- stringr::str_extract_all(adduct, "(?<=[\\-\\+])[A-Z&a-z&0-9]{1,}(?=[\\-\\+\\]])")
    com <- unlist(com)
    ufunc <- stringr::str_extract_all(adduct, "(?<=[0-9|a-z|A-Z])\\+|-(?=[0-9|a-z|A-Z])")
    ufunc <- unlist(ufunc)
    mass <- lapply(com, element_extract)
    mass <- lapply(mass, element_calculate, welement = welement)
    mass <- unlist(mass)
    df <- data.table::data.table(ufunc = ufunc, mass = mass)
    df <- dplyr::mutate(df, mass = ifelse(ufunc == "+", mass, mass * (-1)))
    sum <- sum(df$mass)
    return(sum)
  }
element_calculate <- 
  function(
           df,
           welement
           ){
    weight <- mapply(function(element, number, welement){
                       welement[[element]] * number},
                     df$element, df$number,
                     MoreArgs = list(welement = welement))
    weight <- sum(weight)
    return(weight)
  }
element_extract <- 
  function(
           formula
           ){
    element <- unlist(stringr::str_extract_all(formula, "[A-Z]{1}[a-z]{0,1}"))
    number <- unlist(lapply(paste0("(?<=", element, ")[0-9]{0,}[0-9]{0,}[0-9]{0,}"), mutate_extract,
                     db = formula))
    df <- data.table::data.table(element = element, number = number)
    ## if is NA, set as 1
    df <- dplyr::mutate(df, number = ifelse(number == "", 1, as.numeric(number)))
    if(T %in% duplicated(df$element))
      error
    return(df)
  }
mutate_extract <- 
  function(
           pattern,
           db
           ){
    ch <- stringr::str_extract(db, pattern)
    return(ch)
  }
