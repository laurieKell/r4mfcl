rwtest.frq <- function(frqfile,haltpoint=0) {
  #require(R4MFCL)
  tt <- read.frq(frqfile)
  tmpfile <- tempfile()
  write.frq(tmpfile,tt)
  xx <- read.frq(tmpfile)
  file.remove(tmpfile)
  paste(frqfile,ifelse(is.identical(tt,xx,haltpoint=haltpoint),
                       "passed","failed"),"read.frq/write.frq test")
}
