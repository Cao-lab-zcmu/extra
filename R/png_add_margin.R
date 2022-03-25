png_add_margin <- 
  function(
           png_file,
           width = 5000,
           height = 5000
           ){
    png <- png::readPNG(png_file)
    path <- get_path(png_file)
    name <- get_filename(png_file)
    png(paste0(path, "/", "ps_", name), width = width, height = height)
    plot.new()
    rasterImage(png, 0.05 , 0.1, 0.95, 1)
    dev.off()
  }
png_gather_two <- 
  function(
           png_file1,
           png_file2,
           width = 5000,
           height = 3500
           ){
    png1 <- png::readPNG(png_file1)
    png2 <- png::readPNG(png_file2)
    path <- get_path(png_file1)
    name <- get_filename(png_file1)
    png(paste0(path, "/", "gather_", name), width = width, height = height)
    plot.new()
    rasterImage(png1, 0, 0, 0.47, 1)
    rasterImage(png2, 0.53, 0, 1, 1)
    dev.off()
  }
get_path <- 
  function(
           path_str
           ){
    path <- stringr::str_extract(path_str, ".*(?=/)")
    return(path)
  }
get_filename <- 
  function(
           path_str
           ){
    filename <- stringr::str_extract(path_str, "(?<=/)[^/]*$")
    return(filename)
  }
