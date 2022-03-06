na_as <- 
  function(
           x,
           expr = 0
           ){
    x <- ifelse(is.na(x), expr, x)
  }
