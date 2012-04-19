##' Saves R graphic as a png file.
##' Requires Imagemagick program "convert".
##' @title pngfile
##' @param fname Output file name root (or path)
##' @return No return value
##' @author Pierre Kleiber
pngfile <-
  function (fname = "R-plot") 
{
  fname <- gsub("\\.jpg$", "", fname)
  dev.copy2pdf(file = paste(fname, ".pdf", sep = ""))
  system(paste("convert -quiet ", " ", fname, ".pdf ", 
               fname, ".png", sep = ""))
  file.remove(paste(fname, ".pdf", sep = ""))
  invisible()
}
