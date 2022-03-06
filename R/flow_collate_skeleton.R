flow_collate_skeleton <- 
  function(
           classes,
           ...
           ){
    lapply(classes, base_flow, ...)
  }
base_flow <-
  function(
           class,
           savepath = "~/extra/data"
           ){
    cat("[INFO] the class is >>>", paste0(class), "\n")
    cat("Enter the skeleton smiles:\n")
    smiles <- scan(n = 1, what = "character")
    if(length(smiles) == 1){
      write.table(smiles, file = paste0(savepath, "/", class),
                  col.names = F, row.names = F, quote = F)
    }
  }

