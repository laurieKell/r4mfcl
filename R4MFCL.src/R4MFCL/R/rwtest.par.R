rwtest.par <- function(parfile,haltpoint=0) {
  #require(R4MFCL)
  tt <- read.par(parfile)
  tmpfile <- tempfile()
  write.par(tmpfile,tt)
  xx <- read.par(tmpfile)
  file.remove(tmpfile)
  paste(parfile,ifelse(is.identical(tt,xx,haltpoint=haltpoint),
                       "passed","failed"),"read.par/write.par test")
}
