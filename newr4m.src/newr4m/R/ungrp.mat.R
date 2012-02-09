ungrp.mat <- function(frq.obj) {
##=============================================================================   
## Transforms frq.obj$mat into a 3 matrix form c(mat.ce,mat.l,mat.w)
##=============================================================================
  dsets <- frq.obj$dl$dsets
  if(dsets != nrow(frq.obj$mat)) stop("discrepancy in matrix size")
  lfint <- frq.obj$dl$lfint
  wfint <- frq.obj$dl$wfint
  ##a <- as.vector(frq.obj$mat)
  hh <- ifelse(frq.obj$struct$te>=6,7,6)
  mat.ce <- frq.obj$mat[,1:hh]
  if (hh == 7)  colnames(mat.ce) <- c("year", "qtr", "week", "fishery", 
                       "catch", "effort", "cv")
  else          colnames(mat.ce) <- c("year", "qtr", "week", "fishery", 
                       "catch", "effort")
  
  mat.l <- matrix(0,dsets,lfint)
  mat.w <- matrix(0,dsets,wfint) # has no columns if wfint==0
    ## fill mat.l and mat.w
  mat <- frq.obj$mat[ ,-(1:hh)]
  for(row in 1:dsets) {
    if(mat[row,1]<0) {
      mat.l[row,1] <- mat[row,1]
      if(wfint>0) {
        if(mat[row,2]<0) mat.w[row,1] <- mat[row,2]
        else             mat.w[row, ] <- mat[row,2:(1+wfint)]
      }
    } else {
      mat.l[row, ] <- mat[row,1:lfint]
      if(wfint>0) {
        if(mat[row,1+lfint]<0) mat.w[row,1] <- mat[row,1+lfint]
        else                   mat.w[row, ] <- mat[row,(1+lfint):(lfint+wfint)]
      }
    }
  }
  list(mat.ce=mat.ce,mat.l=mat.l,mat.w=mat.w)
}
