rwtest.tag <- function(tagfile,haltpoint=0) {
  #require(R4MFCL)
  tt <- read.tag(tagfile)
  tmpfile <- tempfile()
  write.tag(tmpfile,tt)
  xx <- read.tag(tmpfile)
  file.remove(tmpfile)
  paste(tagfile,ifelse(is.identical(tt,xx,haltpoint=haltpoint),
                       "passed","failed"),"read.tag/write.tag test")
}
