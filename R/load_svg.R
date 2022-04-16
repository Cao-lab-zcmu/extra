read_svg <- 
  function(
           file,
           as_cairo = T,
           grobify = F,
           arrange = F
           ){
    if(as_cairo)
      rsvg::rsvg_svg(file, file)
    ## ------------------------------------- 
    svg <- grImport2::readPicture(file)
    ## ------------------------------------- 
    if(grobify)
      svg <- grImport2::grobify(svg)
    ## ------------------------------------- 
    if(arrange)
      svg <- gridExtra::arrangeGrob(svg)
    return(svg)
  }
grid_draw_svg.legend <- 
  function(
           main,
           legend,
           savename,
           position.main = 0.55,
           position.legend = 0.1,
           main_size = 1,
           legend_size = 0.8,
           width = 13,
           height = 12
           ){
    svg(savename, width = width, height = height)
    ## ------------------ 
    grImport2::grid.picture(main, width = main_size, height = main_size, x = position.main)
    grImport2::grid.picture(legend, width = legend_size, height = legend_size, x = position.legend)
    ## ------------------ 
    dev.off()
  }

