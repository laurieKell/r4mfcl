adm.std <- function(filen){
  tt <- scan(filen,what=list(0,"",0,0),skip=1)
  tt <- as.data.frame(tt[-1])
  names(tt) <- c("name","value","std")
  nms <- as.character(unique(tt$name))
  x <- list()
  for(nm in nms) x[[nm]] <- tt[tt$name==nm,-1]
  x
}
