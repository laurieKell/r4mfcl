prepplot <- function(x, y, xlim = range(x, na.rm = TRUE), 
           ylim = range(if (zero) 0 else NA, y, na.rm = TRUE), 
           zero = TRUE, xaxis = TRUE, yaxis = TRUE, 
           xtix = xaxis, ytix = yaxis, cex = 1.1, box=TRUE,
           axwidth=1, ...)
{
  if(is.complex(x)){
    y <- Im(x)
    x <- Re(x)
  } else
  if(missing(y)) {
    y <- x
    x <- seq(x)
  }
  plot(xlim, ylim, type = "n", ann = FALSE, axes = FALSE, ...)
  if(box) box(bty = "l", lwd = axwidth)
  if (xaxis) 
    axis(1, lwd = axwidth, cex.axis = cex)
  else {
    if (xtix) 
      axis(1, labels = FALSE)
  }
  if (yaxis) 
    axis(2, lwd = axwidth, cex.axis = cex)
  else {
    if (ytix) 
      axis(2, labels = FALSE)
  }
}
##prepplot <-
##  function(x, y, xlim = range(x, na.rm = TRUE), 
##           ylim = range(if (zero) 0 else NA, y, na.rm = TRUE), 
##           zero = TRUE, xaxis = TRUE, xpos=par("usr")[3], yaxis = TRUE, 
##           xtix = xaxis, ytix = yaxis, cex = 1.3, lwd=1,...)
##{
##  if(missing(y)) {
##    y <- x
##    x <- seq(x)
##  }
##  plot(xlim, ylim, type = "n", ann = FALSE, axes = FALSE, ...)
##  ##box(bty = "l", lwd = lwd)
##  if (xaxis) 
##    axis(1, lwd = lwd, cex.axis = cex, pos=xpos)
##  else {
##    if (xtix) 
##      axis(1, labels = FALSE, pos=xpos)
##  }
##  if (yaxis) 
##    axis(2, lwd = lwd, cex.axis = cex)
##  else {
##    if (ytix) 
##      axis(2, labels = FALSE)
##  }
##  tt <- par("usr")
##  segments(x0=rep(tt[1],2),y0=rep(xpos,2),x1=tt[1:2],y1=c(tt[4],xpos),lwd=lwd)
##}
