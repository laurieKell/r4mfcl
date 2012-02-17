"rwiki.search" <-
function(string){
   RwikiURL="http://fawn.unibw-hamburg.de/cgi-bin/Rwiki.pl"
   RwikiSearchURL=paste(RwikiURL,"?search=",string,sep='')
   browseURL(RwikiSearchURL)
   return(invisible(0))
}
