pielet <- function(x,y,r,a,col,...) {
##-------------------------------------------------------------------
##  Makes a piechart centered at position x,y with radius r (inches).
##  The relative values of vector, a, are the relative sizes of the
##  segments. col, if given, should be a vector of colors. If col is
##  missing the colors are chosen from current palette.
##-------------------------------------------------------------------
  usr <- par("usr")
  if(is.complex(x)) ctr <- x
  else ctr <- complex(,x,y)
  asp <- par("pin")
  r <- r*min(asp)/2
  asp <- asp[2]/asp[1]
  asp <- asp*(usr[2]-usr[1])/(usr[4]-usr[3])

  a <- c(0,cumsum(2*pi*a/sum(a)))
  
  pts <- list()
  for(i in 2:length(a)) {
    pts[[i-1]] <- c(0+0i,complex(mod=r,arg=seq(a[i-1],a[i],length=50)),0+0i)
  }
  pts <- sapply(pts,function(x) complex(,Re(x)*asp,Im(x)))

  if(missing(col)) col <- 1:ncol(pts)
  for(i in 1:ncol(pts)) polygon(ctr+pts[,i],col=col[i],border=NA,...)
}
