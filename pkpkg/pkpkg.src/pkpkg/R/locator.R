locator <-
  function (n = 512, type = "n", ...) 
{
  if (length(extras <- list(...))) {
    opar <- par(extras)
    on.exit(par(opar))
  }

  if(type=="n"){
    x <- y <- numeric()
    for(i in 1:n) {
      z <- .Internal(locator(1, type = "n"))
      if(z[[3L]] == 0) break
      x <- c(x,z[[1L]])
      y <- c(y,z[[2L]])
    }
    return(list(x=x,y=y))
  }
  
  z <- .Internal(locator(n, type = type))
  x <- z[[1L]]
  y <- z[[2L]]
  if ((n <- z[[3L]]) > 0) 
    list(x = x[1L:n], y = y[1L:n])
}

