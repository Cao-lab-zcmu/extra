compound_align <- 
  function(
           main,
           sub,
           main_col = "mz",
           mz_tol = 0.002,
           rt_tol = 0.1,
           mz_wight = 75,
           rt_wight = 25
           ){
    list <- lapply(main[[main_col]], mz_align,
                   df = sub, mz_tol = mz_tol)
    if(is.na(rt_tol))
      return()
  }
rt_align <- 
  function(
           the_rt,
           df,
           rt_tol = 0.1
           ){
    df <- dplyr::filter(df, rt <= the_rt + rt_tol &
                        rt >= the_rt - rt_tol)
    return(df)
  }
mz_align <- 
  function(
           the_mz,
           df,
           mz_tol = 0.002
           ){
    df <- dplyr::filter(df, mz <= the_mz + mz_tol &
                        mz >= the_mz - mz_tol)
    return(df)
  }
