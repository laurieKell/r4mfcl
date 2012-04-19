plotmap <- function (lon1, lon2, lat1, lat2, resolution = 0, grid = FALSE, 
            add = FALSE, landcolor = "grey") 
##==================================================================
##  lon1, lon2, lat1, lat2 give coordinates of map window in degrees
##    east from Grenwich.
##  resolution of 0 makes resolution. Larger values make cruder
##    resolution.
##==================================================================
{
  require(maps)
  if(add) {
    map(add=T,fill=T,col=landcolor,border=NA,resolution=resolution)
  } else {
    map(add=F,xlim=c(lon1, lon2),ylim=c(lat1, lat2),
        fill=T,col=landcolor,border=NA,resolution=resolution)
  }
  if (grid) {
      axis(1, tck = 1)
      axis(2, tck = 1)
  }
}
