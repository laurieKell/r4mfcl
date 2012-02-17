"getdat" <- function(filen,tag,what=double(0)) {
##=======================================================
## Returns data from records in filen tagged at the beginning
## of each record with regular expression tag.
##=======================================================
  awkcall <- paste("awk '/",tag,"/{match($0,/",tag,"/,x);",
           "print substr($0,1+length(x[0]))}' ",filen,sep="")
  con <- pipe(awkcall)
  out <- if(missing(what)) scan(con)
            else scan(con, what=what)
  close(con)
  out
}
