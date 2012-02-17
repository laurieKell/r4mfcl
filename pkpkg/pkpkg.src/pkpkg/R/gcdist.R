"gcdist" <- function(lat,lon,lat0,lon0){
##############################################################
## returns great circle dist. in nautical miles  between
## (lat0,lon0) and (lat,lon) in degrees.  These can be
## vectors in which case a vector of distances is returned. 
##############################################################
  lat <- pi*lat/180
  lon <- pi*lon/180
  lat0 <- pi*lat0/180
  lon0 <- pi*lon0/180
  180*60*acos(cos(lat0)*cos(lat)*cos(lon-lon0)+sin(lat0)*sin(lat))/pi
}
