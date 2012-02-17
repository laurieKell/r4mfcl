easylayout <- function(n) {
##======================================
## Sets up an appropriate layout for n
## sub-plots
##======================================
  ncolpl <- ceiling(sqrt(n))
  nrowpl <- ceiling(n/ncolpl)
  par(mfcol=c(nrowpl,ncolpl))
}
