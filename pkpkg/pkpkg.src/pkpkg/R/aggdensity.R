aggdensity <- function(dat,resol=1){
##---------------------------------------------------------------------
## Returns density matrix "mm" giving percentage levels for occurrence
## of locations which are aggregated to lat/lon squares with resolution
## given by "resol". Centers of squares returned in "lons" and "lats".
## "dat" is either a vector of locations given as complex numbers
## (longitude is real and latitude is imaginary) or is a data frame
## containing column names "lat" and "lon".
##---------------------------------------------------------------------
  if(is.complex(dat)){
    xx <- resol*floor(Re(dat)/resol)
    yy <- resol*floor(Im(dat)/resol)
  } else if(sum(colnames(dat)%in%c("lat","lon"))==2) {
    xx <- resol*floor(dat[,"lon"]/resol)
    yy <- resol*floor(dat[,"lat"]/resol)    
  } else stop('argument "dat" is defective')
  dx <- min(diff(sort(unique(xx))))
  dy <- min(diff(sort(unique(yy))))
  xx <- xx+dx/2
  yy <- yy+dy/2
  loc <- complex(real=xx,imaginary=yy)
  tt <- table(loc)
  ttt <- table(tt)
  ttt <- 100-cumsum(100*ttt*as.numeric(names(ttt))/length(xx))
  uu <- tt
  for(i in 1:length(ttt)) uu[which(uu==names(ttt[i]))] <- ttt[i]
  uun <- as.complex(names(uu))
  xr <- range(Re(uun))
  yr <- range(Im(uun))
  lons <- seq(xr[1],xr[2],by=dx)
  lats <- seq(yr[1],yr[2],by=dy)
  mm <- matrix(nrow=length(lons),ncol=length(lats))
  for(i in 1:length(uu)) mm[which(lons==Re(uun[i])),
                            which(lats==Im(uun[i]))] <- uu[i]
  list(mm=mm,lons=lons,lats=lats)
}

