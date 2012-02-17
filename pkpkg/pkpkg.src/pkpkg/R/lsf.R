"lsf" <-
function (pos = 1) 
##=======================================================
## Like ls() but only prints functions
##=======================================================
{
    junk <- ls(pos, all.names = TRUE)
    junk[sapply(junk, function(x) is.function(eval(as.symbol(x))))]
}
