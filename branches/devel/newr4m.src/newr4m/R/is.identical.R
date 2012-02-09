is.identical <- function(tt,xx,haltpoint=0) {
  if(!identical(length(tt),length(xx))) stop("different number of components")
  if(!identical(names(tt),names(xx))) warning("names differ")
  pp <- logical(length(tt))
  for(i in 1:length(tt)) pp[i] <- !identical(tt[[i]],xx[[i]])
  for(i in which(pp)) {
    ttt <- unlist(tt[[i]]) # in case the component is a list
    xxx <- unlist(xx[[i]])
    if(i==haltpoint) browser()
    if(max(abs((ttt-xxx)/(ttt+1e-12))) < 1e-10) pp[i] <- FALSE
    if(pp[i]) print(paste("Component",i,names(tt)[i],"differs."))
  }
  return(!any(pp))
}
