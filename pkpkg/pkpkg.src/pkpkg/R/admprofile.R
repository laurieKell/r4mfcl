admprofile <- function(fname,x="x",y="y"){
#=========================================================================
# gets likelihood profile from admodel output file, fname. Usually named
#  something like "vblname.plt".
#=========================================================================
   tt <- scanpipe(paste("awk '/Profile/{x=1;next}/^Min/{exit}(x==1){print}'",
                        fname), what=list(0,0))
   tt <- as.data.frame(tt)
   names(tt) <- c(x,y)
   tt
}

admnormapprox <- function(fname,x="x",y="y"){
#=========================================================================
# gets normal approximation of likelihood profile from admodel output file,
#  fname. Usually named something like "vblname.plt".
#=========================================================================
   tt <- scanpipe(
            paste("awk '/Normal/{x=1;next}/^Min/{if(x) exit}(x==1){print}'",
                  fname), what=list(0,0))
   tt <- as.data.frame(tt)
   names(tt) <- c(x,y)
   tt
}
