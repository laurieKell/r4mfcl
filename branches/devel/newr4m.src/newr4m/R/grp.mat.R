##' catch/effort and sample data from three matrix to single matrix form 
##' @title grp.mat
##' @param mat.ce 
##' @param mat.l 
##' @param mat.w 
##' @return single matrix
##' @author Pierre Kleiber
grp.mat <- function(mat.ce,mat.l,mat.w) {
##=============================================================================   
## Transforms matrices of a 3 matrix form of a frq object into the mushed
## single matrix form
##=============================================================================
  dsets <- nrow(mat.ce)
    ## sanity check
  if(exists("mat.l") & (nrow(mat.l) != dsets)) stop("matrices don't conform")
  if(exists("mat.w") & (nrow(mat.w) != dsets)) stop("matrices don't conform")
  
  lfint <- ifelse(exists("mat.l"),ncol(mat.l),0)
  wfint <- ifelse(exists("mat.w"),ncol(mat.w),0)
  mattt <- matrix(0,nrow=dsets,ncol=lfint+wfint)

  if ((lfint != 0 && wfint == 0) || (lfint == 0 && wfint != 0)) {
    if(lfint==0) {
      mat.l <- mat.w
      lfint <- wfint
    }
    for (i in 1:dsets) {
      if (mat.l[i,1] == -1) {
        mattt[i,1] <- -1
      }
      else {
        mattt[i, 1:lfint] <- mat.l[i, ]
      }
    }
  }
  else if (lfint != 0 && wfint != 0) {
    for (i in 1:dsets) {
     if (mat.l[i,1] == -1) {
        mattt[i,1] <- -1
        hh <- 2
      }
      else {
        mattt[i,1:lfint] <- mat.l[i,]
        hh <- lfint+1
      }
      if (mat.w[i,1] == -1) 
        mattt[i,hh] <-  -1
      else
        mattt[i,hh:(hh+wfint-1)] <- mat.w[i,]
    }
  }
  cbind(mat.ce,mattt)
}
