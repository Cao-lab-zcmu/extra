numeric_round_merge <- 
  function(
           main = NULL,
           sub = NULL,
           list = NULL,
           main_col = "mass",
           sub_col = "mz",
           mz.tol = 0.002,
           noise = F
           ){
    if(is.list(list)){
      ## as a list to store data, so that lead to easily apply 'pbapply::pblapply'
      ## for multi-threads running
      main <- list[[1]]
      sub <- list[[2]]
    }
    mutate_main <- mutate(main, .id.m = 1)
    sub <- mutate(sub, .id.m = 1)
    ## ------------------------------------- 
    ## expand merge
    df <- merge(mutate_main, sub, by = ".id.m", all.x = T, allow.cartesian = T)
    ## ------------------------------------- 
    ## expression
    .Expr <- paste0("abs(as.numeric(", main_col, ") - as.numeric(", sub_col, "))")
    ## eval do expression
    df <- mutate(df, .diff = eval(parse(text = .Expr)))
    ## filter
    df <- filter(df, .diff <= mz.tol)
    ## remove the assist col
    df <- select(df, -.id.m, -.diff)
    ## ------------------------------------- 
    if(noise){
      ## as relative intensity
      main <- mutate(main, re.inte = as.numeric(inte) / max(as.numeric(inte)))
      ## eval expression
      .Expr <- paste0("!", main_col, " %in% df$", main_col)
      ## get the noise peak (invalid for sirius)
      df <- filter(main, eval(parse(text = .Expr)))
    }
    ## ------------------------------------- 
    return(df)
  }
