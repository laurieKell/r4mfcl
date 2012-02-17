beta.values <- function(a, b, x1=0, x2=1){
##=========================================================
##  calc mean, mode, and sigma for non-standard beta
##  distribution on interval x1 to x2.  a and b are the
##  shape parameters.
##=========================================================
  mean <- x1 + (x2-x1)*a/(a + b)
  mode <- x1 + (x2-x1)*(a - 1)/(a + b - 2)
  sigma <- (x2-x1)*sqrt(a*b/(a + b + 1))/(a + b)
  c(mode=mode, mean=mean, sigma=sigma)
}

inv.beta.values <- function (mode, sigma, x1 = 0, x2 = 1,range=c(1,100)) 
##=========================================================
##  Find shape parameters for non-standard beta
##  distribution on interval x1 to x2 given the mode and
##  sigma.
##=========================================================
{
  tt <- (mode - x1)/(x2 - x1)
  var <- sigma^2
  f <- function(x) {
    b <- 2 - x + (x - 1)/tt
    var/((x2 - x1)^2) - x * b/((x + b)^2 * (x + b + 1))
  }
  ##print(f(range))
  a <- uniroot(f, range)$root
  c(a = a, b = 2 - a + (a - 1)/tt)
}

plotbeta <- function (a, b, x1, x2,add=FALSE,plot=TRUE) 
##=========================================================
##  plot non-standard beta density on interval x1 to x2.
##  a and b are the shape parameters.
##=========================================================
{
  x <- seq(x1,x2,length=100)
  y <- dbeta((x - x1)/(x2 - x1), a, b)
  if(plot) {
    if(!add) prepplot(x,y)
    lines(x,y)
  }else
  data.frame(x=x,dbeta=y)
}
