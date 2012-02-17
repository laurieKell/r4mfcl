"plot0" <- function(x,y,...) {
##=================================
## includes zero in the y-axis.
##=================================
  plot(x,y,ylim=range(0,y),...)
}
