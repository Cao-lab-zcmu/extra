png_auto_zoom <- 
  function(
           file,
           zoom.center.x = 0.5,
           zoom.center.y = 0.5,
           global.h = 5000,
           zoom.width = 1.5,
           internal.width = 0.1,
           zoom.window = 0.2
           ){
    ## readpng
    png <- EBImage::readImage(file)
    ## size ratio
    ratio.png <- ncol(png) / nrow(png)
    ## dev ratio
    ratio.dev <- zoom.width + ratio.png + internal.width
    ## create dev
    png(filename = paste0(file, ".zoom.png"),
        width = global.h * ratio.dev,
        height = global.h)
    ## use grid
    grid.newpage()
    ## ---------------------------------------------------------------------- 
    ## origin plot
    ## position and size
    ori.x <- (internal.width + zoom.width) / ratio.dev
    ori.y <- 0
    ori.tmp.width <- ratio.png / ratio.dev
    ori.tmp.height <- 1
    ## create windows
    view.port <- fast.view(ori.x, ori.y, ori.tmp.width, ori.tmp.height)
    ## push
    grid.raster(png, vp = view.port)
    ## ---------------------------------------------------------------------- 
    ## zoom plot
    ## clip the plot
    ## ------------------ 
    ## row clip 
    row.start <- nrow(png) * (1 - zoom.center.y - zoom.window / 2)
    row.end <- nrow(png) * (1 - zoom.center.y + zoom.window / 2)
    ## relative width of zoom size
    zoom.relative.width <- (1 / ratio.dev) * zoom.window * zoom.width
    ## col clip
    col.start <- ncol(png) * (zoom.center.x - zoom.relative.width / 2)
    col.end <- ncol(png) * (zoom.center.x + zoom.relative.width / 2)
    ## ------------------ 
    focus.png <- png[row.start:row.end, col.start:col.end, ]
    ## ------------------------------------- 
    ## position
    focus.x <- 0
    focus.y <- 0
    focus.tmp.width <- zoom.width / ratio.dev
    focus.tmp.height <- 1
    ## viewport
    view.port <- fast.view(focus.x, focus.y, focus.tmp.width, focus.tmp.height)
    ## push focus.png
    grid.raster(focus.png, vp = view.port)
    ## zoom rect
    grid.rect(gp = gpar(fill = NA, lwd = 100, col = "black"),
              vp = view.port)
    ## ---------------------------------------------------------------------- 
    dev.off()
  }
fast.view <-
  function(
           x, y, tmp.width, tmp.height,
           just = c("left", "bottom")
           ){
  view.port <- viewport(x = unit(x, "npc"),
                        y = unit(y, "npc"),
                        width = unit(tmp.width, "npc"),
                        height = unit(tmp.height, "npc"),
                        just = just)
  return(view.port)
}

