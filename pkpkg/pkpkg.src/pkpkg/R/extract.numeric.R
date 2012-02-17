extract.numeric <- function(x) 
##===================================================================
## extracts numerical elements of a string containing blank separated
## words
##===================================================================
{
  op <- options(warn=-1)
  on.exit(options(op))
  x <- as.numeric(unlist(strsplit(x," +")))
  x[which(!is.na(x))]
}
