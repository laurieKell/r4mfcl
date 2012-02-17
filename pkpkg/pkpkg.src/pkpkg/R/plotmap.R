"plotmap" <- function(lon1, lon2, lat1, lat2, resolution=3,
               grid=FALSE, add=FALSE, save=FALSE,
               landcolor="grey", seacolor="white") {
##==================================================================
##  lon1, lon2, lat1, lat2 give coordinates of map window in degrees
##    east from Grenwich.
##  resolution goes from 1 (full resolution) to 5 (crude) in integer
##    steps.
##==================================================================
  gmt <- function(lon1,lon2,lat1,lat2,resolution=3) {
    read.ps.line<-function(txt){
      txt.split<-strsplit(txt, split=" ")[[1]]
      ret<-c(NA,NA)
      if(length(txt.split)==3){       
        if(txt.split[3]%in%c("M","moveto","D")){
          ret<-as.numeric(txt.split[1:2])
        }
      }
      return(ret)
    }
    if(resolution<1 || resolution>5)
        stop("resolution from 1 (full) to 5(crude)")
    res<-c("f","h","i","l","c")[resolution]
    filen <- tempfile("gmtmap")
    on.exit(unlink(c(filen,".gmtcommands4")))
    cmd<-paste("pscoast -R",lon1,"/",lon2,"/",lat1,"/",lat2,
               " -Jx2id -P -G -D",res,
               " -X0 -Y0 >",filen,sep="")
    system(cmd)
    txt<-readLines(filen)
    mat<-matrix(unlist(lapply(txt, read.ps.line)),
                ncol=2, byrow=TRUE)
    for(i in 2:nrow(mat)){
      if(!is.na(mat[i,1])&!is.na(mat[i-1,1]))
        mat[i,]<-mat[i,]+mat[i-1,]
    }
    maxx<-max(mat[,1], na.rm=TRUE)
    maxy<-max(mat[,2], na.rm=TRUE)

    mat[,1]<-mat[,1]/600+lon1
    mat[,2]<-mat[,2]/600+lat1
    return(mat)
  }
  junk <- gmt(lon1,lon2,lat1,lat2,resolution=resolution)
  if(!add) {
    plot(c(lon1,lon2),c(lat1,lat2),type='n',ylab="",xlab="",
       xaxs="i", yaxs="i")
    rect(lon1,lat1,lon2,lat2, col=seacolor)
    if(grid) {
      axis(1,tck=1)
      axis(2,tck=1)
    }
  }
  polygon(junk,border=landcolor,col=landcolor)
  if(save) {
    dimnames(junk)[[2]] <- c("longitude","latitude")
    return(junk)
  }
  box()
}
