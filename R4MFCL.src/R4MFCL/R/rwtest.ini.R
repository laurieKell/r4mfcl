rwtest.ini <- function(inifile,haltpoint=0) {
  #require(R4MFCL)
  tt <- read.ini(inifile)
  tmpfile <- tempfile()
  write.ini(tmpfile,tt)
  xx <- read.ini(tmpfile)
  file.remove(tmpfile)
  paste(inifile,ifelse(is.identical(tt,xx,haltpoint=haltpoint),
                       "passed","failed"),"read.ini/write.ini test")
}
