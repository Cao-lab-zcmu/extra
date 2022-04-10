# |mass      |inte |
# |:---------|:----|
# |117.06971 |20   |
mass_shift <- 
  function(
           df,
           merge = T,
           sep = " ",
           int.sigma = 1,
           re.ppm = 1e-6,
           global.sigma = 10/3 * re.ppm,
           indivi.sigma = 10/3 * re.ppm,
           sub.factor = 0.03,
           .noise_pool = noise_pool,
           alpha = 0.2,
           ...
           ){
    df <- mutate(df, mass = as.numeric(mass), inte = as.numeric(inte))
    ## ---------------------------------------------------------------------- 
    ## intensity variation
    var <- rnorm(nrow(df), 1, int.sigma)
    df <- mutate(df, inte = inte * var)
    ## subtract according to max intensity
    df <- mutate(df, inte = round(inte - max(inte) * sub.factor, 0))
    ## if intensity less than 0, discard
    df <- filter(df, inte > 0)
    ## almost one peak, discard the data
    if(nrow(df) <= 1)
      return()
    ## ---------------------------------------------------------------------- 
    ## global shift
    var <- rnorm(1, 0, global.sigma)
    df <- mutate(df, mass = mass + mass * var)
    ## ---------------------------------------------------------------------- 
    ## individual shift
    var <- rnorm(nrow(df), 0, indivi.sigma)
    df <- mutate(df, mass = round(mass + mass * var, 4))
    ## ---------------------------------------------------------------------- 
    ## add noise peak
    ## random drawn noise peak from noise pool
    noise <- .noise_pool[sample(1:nrow(.noise_pool), round(alpha * nrow(df))), ]
    ## reshape intensity
    noise <- mutate(noise, inte = max(df$inte) * re.inte)
    ## bind into df
    df <- bind_rows(df, select(noise, mass, inte))
    ## ---------------------------------------------------------------------- 
    if(merge == T){
      df <- mutate(df, V1 = paste0(mass, sep, inte))
      df <- select(df, V1)
    }
    ## ------------------------------------- 
    return(df)
  }
