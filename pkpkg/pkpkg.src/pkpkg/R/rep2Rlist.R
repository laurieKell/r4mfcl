rep2Rlist = function(fn) {
##=================================================================
## parses a file consisting of numbers interspersed with names.
## The names set names of elements in output list. The numbers
## (which are the elements) can be vectors, which are given in a
## single line in the file, or matrices, which are given by row.
##=================================================================
  ifile=scan(fn,what="character",flush=TRUE,blank.lines.skip=FALSE,quiet=TRUE)
  op=options(warn=-1)
  idx=sapply(as.double(ifile),is.na)
  options(op)
  vnam=ifile[idx] #list names
  nv=length(vnam) #number of objects
  A=list()
  ir=0
  for(i in 1:nv){
    ir=match(vnam[i],ifile)
    if(i!=nv) irr=match(vnam[i+1],ifile) else irr=length(ifile)+1 #next row
    dum=NA
    if(irr-ir==2) dum=as.double(scan(fn,skip=ir,nlines=1,quiet=TRUE,what=""))
    if(irr-ir>2) dum=as.matrix(read.table(fn,skip=ir,nrow=irr-ir-1,fill=TRUE))
    
    if(is.numeric(dum)) { #Logical test to ensure dealing with numbers
      A[[vnam[i]]]=dum
    }
  }
  return(A)
}
