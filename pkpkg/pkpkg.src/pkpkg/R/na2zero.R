na2zero <- function(x) {
##--------------------------------------------------
## returns with all NA element of x set to 0
##--------------------------------------------------
  x[] <- lapply(x,function(x){x[is.na(x)] <- 0;x})
  x
}
