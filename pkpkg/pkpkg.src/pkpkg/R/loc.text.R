loc.text <- function(labels, ...){
  for(i in labels) {
    z <- .Internal(locator(1,"n"))
    text(z[[1L]],z[[2L]],labels=i,...)
  }
}

         
