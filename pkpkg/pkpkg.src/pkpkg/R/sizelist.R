"sizelist" <-
function(env=1)
##=======================================================
## returns sorted list of objects in env giving approx.
##  sizes.  
##=======================================================
{
  lst <- sapply(ls(env), function(x) object.size(get(x)))
  lst <- as.data.frame(lst[order(lst)])
  names(lst) <- "size"
  lst
}

