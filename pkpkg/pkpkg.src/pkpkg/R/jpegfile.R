##' Saves R graphic as a jpeg file.
##'
##' Sequesters corresponding eps file in subdirectory "epsfiles"
##' which is created if necessary. Requires Imagemagick program "convert".
##' @title jpegfile
##' @param fname Output file name root (or path)
##' @param quality jpeg quality parameter (0 - 100)
##' @param epsdir If not false, the directory for eps output file.
##' @return No return value
##' @author Pierre Kleiber
"jpegfile" <- function(fname="R-plot",quality=100,epsdir=FALSE) {
##-----------------------------------------------------------------------
##  Saves R graphic as a jpeg file.  Sequesters corresponding eps file
##  in subdirectory "epsfiles" which is created if necessary.
##  Requires Imagemagick program "convert".
##-----------------------------------------------------------------------
  fname <- gsub("\\.jpg$","",fname)
  dev.copy2eps(file=paste(fname,".eps",sep=""))
  system(paste("convert -quality ",quality," ",
               fname,".eps ",fname,".jpg",sep=""))
  if(epsdir) {
    subdir <- paste(gsub("[^/]*$","",fname),"epsfiles",sep="")
    dir.create(subdir,showWarnings=FALSE)
    file.copy(paste(fname,".eps",sep=""),subdir,overwrite=TRUE)
  }
  file.remove(paste(fname,".eps",sep=""))
  invisible()
}
