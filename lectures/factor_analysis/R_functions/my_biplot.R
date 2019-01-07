my_biplot <- function (sc, ld, dims = c(1,2), xld = c(-1.3, 1.3),
                       yld = c(-1.3, 1.3)) {
  par(mar=c(5,6,5,5), pty="s")
  
  ylab_tmp <- paste0("Score", dims[1])
  xlab_tmp <- paste0("Score", dims[2])
  
  my_x <- c(min(sc[ ,1]) * 1.1, max(sc[ ,1]) * 1.1)
  plot(sc, axes = F, ann = FALSE, xlim = my_x)
  mtext(xlab_tmp, side = 4, line = 3)
  mtext(ylab_tmp, side = 3, line = 3)
  axis(side = 4)
  axis(side = 3)
  # text(sc[ ,1], sc[ ,2], gp, cex=0.6, pos=4)
  text(sc[ ,1], sc[ ,2], rownames(sc), pos=4, col = alpha("darkgray", 1))
  
  
  par(new = TRUE, pty="s") 
  my_x <- xld
  my_y <- yld
  
  xlab_tmp <- paste0("Loading", dims[1])
  ylab_tmp <- paste0("Loading", dims[2])
  
  plot(ld, 
       col  = "white", 
       xlab = xlab_tmp, 
       ylab = ylab_tmp, 
       xlim = my_x, 
       ylim = my_y)
  arrows(rep(0, times = nrow(ld)), rep(0, times = nrow(ld)), ld[ ,1], ld[ ,2],
         col = "red", length = 0.1)
  text(ld[ ,1], ld[ ,2], rownames(ld), pos=4, col = "red")
}