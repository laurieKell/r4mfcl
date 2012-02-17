paletteTest <- function(pal){
  n <- length(pal)
  x <- seq(0,1,length=n+1)
  pie(rep(1,n), col=pal, border=0, labels=NA)  
}

  
