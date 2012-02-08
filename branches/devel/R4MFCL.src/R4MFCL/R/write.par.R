 write.par <- function(par.file,par.obj)
##======================================================================
## re-write, P Kleiber, Jan 2012
##  
## Numeric elements of the par file are taken to be contiguous groups of
## purely numeric file records preceeded by a header record indicated by
## "#" in column 1. Numeric elements are extracted into a list with
## names given by the header records preceeding each element. Special
## case of numeric data within a header record is accommodated.
##  
## Several elements of the list are renamed for convenience in accessing
## the list, and some derived quantities are added to the list.
##======================================================================
{
    ## rename some elements of the list to original file header names
  names(par.obj)[grep("^pfl$"            ,names(par.obj))] <- "# The parest_flags"                            
  names(par.obj)[grep("^nages$"          ,names(par.obj))] <- "# The number of age classes"                   
  names(par.obj)[grep("^afl$"            ,names(par.obj))] <- "# age flags"                                   
  names(par.obj)[grep("^ffl$"            ,names(par.obj))] <- "# fish flags"                                  
  names(par.obj)[grep("^tfl$"            ,names(par.obj))] <- "# tag flags"                                   
  names(par.obj)[grep("^trpfl$"          ,names(par.obj))] <- "# tag fish rep"                               
  names(par.obj)[grep("^trpgpfl$"        ,names(par.obj))] <- "# tag fish rep group flags"                    
  names(par.obj)[grep("^trpacfl$"        ,names(par.obj))] <- "# tag_fish_rep active flags"                   
  names(par.obj)[grep("^treptarg$"       ,names(par.obj))] <- "# tag_fish_rep target"                         
  names(par.obj)[grep("^treppen$"        ,names(par.obj))] <- "# treptarg"                                    
  names(par.obj)[grep("^maturity$"       ,names(par.obj))] <- "# percent maturity"                            
  names(par.obj)[grep("^totpop$"         ,names(par.obj))] <- "# total populations scaling parameter"         
  names(par.obj)[grep("^totpop_implicit$",names(par.obj))] <- "# implicit total populations scaling parameter"
  names(par.obj)[grep("^rec_init$"       ,names(par.obj))] <- "# rec init pop level difference"               
  names(par.obj)[grep("^rectimes$"       ,names(par.obj))] <- "# recruitment times"                           
  names(par.obj)[grep("^rel_recruitment$",names(par.obj))] <- "# relative recruitment "                       
  names(par.obj)[grep("^selectivity$"    ,names(par.obj))] <- "# fishery selectivity"                         
  names(par.obj)[grep("^Mbase$"          ,names(par.obj))] <- "# natural mortality coefficient"               
  names(par.obj)[grep("^effdevcoffs$"    ,names(par.obj))] <- "# effort deviation coefficients"               
  names(par.obj)[grep("^meanqs$"         ,names(par.obj))] <- "# average catchability coefficients"           
  names(par.obj)[grep("^obj$"            ,names(par.obj))] <- "# Objective function value"                    
  names(par.obj)[grep("^npars$"          ,names(par.obj))] <- "# The number of parameters"                    
  names(par.obj)[grep("^gradient$"       ,names(par.obj))] <- "# Maximum magnitude gradient value"            
  names(par.obj)[grep("^Richards$"       ,names(par.obj))] <- "# extra par for Richards"                      
  ##names(par)[grep("",names(par))] <- ""

  ops <- options(digits=12)
  on.exit(options(ops))
  fn <- file(par.file,"w")
  on.exit(close(fn),add=TRUE)
  for(i in 1:par.obj$num.elements) {
    write(names(par.obj)[i],fn,append=TRUE)
    if(is.list(par.obj[[i]])) {
      for(j in 1:length(par.obj[[i]])) {
        write(par.obj[[i]][[j]],fn, ncolumns=length(par.obj[[i]][[j]]),append=TRUE)
        ##write.table(t(as.vector(par.obj[[i]][[j]])),fn, quote=FALSE, sep = " ",
        ##            row.names = FALSE, col.names = FALSE,append = T)
      }
    } else {
      dd <- dim(par.obj[[i]])
      if(length(dd)==1) {
        if(dd>0) write(par.obj[[i]],fn, ncolumns=dd,append=TRUE)
        ##write.table(t(as.vector(par.obj[[i]])),fn, quote=FALSE, sep = " ",
        ##          row.names = FALSE, col.names = FALSE,append = T)
      } else {
        write.table(par.obj[[i]],fn, quote=FALSE, sep = " ",
                  row.names = FALSE, col.names = FALSE,append = T)
      }
    }
  }
}
