errplot <- function(x,y,se,add=TRUE,border=NA,col="lightgray",func=I,...){
##---------------------------------------------------------------------
## plots error ( + or - 2*se) by shaded area. Function func transforms
##  the error boundary.
##---------------------------------------------------------------------
  if(!add) prepplot(x,func(c(y+2*se,y-2*se)))
  polygon(c(x,rev(x)),func(c(y+2*se,rev(y-2*se))),border=border,col=col,...)
}
