 read.par <- function(par.file)
##======================================================================
## re-write, P Kleiber, Jan 2012
##  
## Numeric elements of the par file are taken to be contiguous groups of
## purely numeric file records preceeded by a header record indicated by
## "#" in column 1. Numeric elements are extracted into a list with
## names given by the header records preceeding each element. Special
## case of numeric data within a header record is accomodated.
##  
## Several elements of the list are renamed for convenience in accessing
## the list, and some derived quantities are added to the list.
##======================================================================
{
    ## read input as text
  txt <- readLines(par.file)
    ## get rid of blank (or almost blank) lines
  blanks <- grep("^#* *$|^#[ .0-9]*$",txt)
          ## last bit above assumes that a record with "#" followed
          ## by purely numeric stuff is a commented out record
  if(length(blanks)>0) txt <- txt[-blanks]
    ## extract numeric parts into a list with elements
    ##  named by the preceeding header records
  par <- list()
  hptrs <- c(which(substr(txt,1,1)=="#"),length(txt)+1)
  for(hpt in 1:(length(hptrs)-1)) {
    p1 <- hptrs[hpt]+1
    p2 <- hptrs[hpt+1]-1
          ##print(paste(hpt,txt[hptrs[hpt]],p1,p2))
    if(p2 >= p1) {
      tt <- scanText(txt[p1:p2])
      nrow=p2-p1+1
      ss <- length(tt)/nrow
      if(identical(ss,floor(ss)))
           par[[txt[hptrs[hpt]]]] <- drop(matrix(as.numeric(tt),byrow=TRUE,nrow=nrow))
      else {
        xx <- list()
        for(i in 1:nrow) xx[[i]] <- extract.numeric(txt[p1+i-1])
        names(xx) <- format(1:nrow)
        par[[txt[hptrs[hpt]]]] <- xx
      }
    }
    else par[[txt[hptrs[hpt]]]] <- extract.numeric(txt[hptrs[hpt]])
  }
  par[["num.elements"]] <- length(par)
  
    ## for convenience, rename some elements of the list
  names(par)[grep("# The parest_flags",names(par))] <- "pfl"
  names(par)[grep("# The number of age classes",names(par))] <- "nages"
  names(par)[grep("# age flags",names(par))] <- "afl"
  names(par)[grep("# fish flags",names(par))] <- "ffl"
  names(par)[grep("# tag flags",names(par))] <- "tfl"
  names(par)[grep("# tag fish rep$",names(par))] <- "trpfl"
  names(par)[grep("# tag fish rep group flags",names(par))] <- "trpgpfl"
  names(par)[grep("# tag_fish_rep active flags",names(par))] <- "trpacfl"
  names(par)[grep("# tag_fish_rep target",names(par))] <- "treptarg"
  names(par)[grep("# treptarg",names(par))] <- "treppen"
  names(par)[grep("# percent maturity",names(par))] <- "maturity"
  names(par)[grep("# total populations scaling parameter",names(par))] <- "totpop"
  names(par)[grep("# implicit total populations scaling parameter",names(par))] <- "totpop_implicit"
  names(par)[grep("# rec init pop level difference",names(par))] <- "rec_init"
  names(par)[grep("# recruitment times",names(par))] <- "rectimes"
  names(par)[grep("# relative recruitment ",names(par))] <- "rel_recruitment"
  names(par)[grep("# fishery selectivity",names(par))] <- "selectivity"
  names(par)[grep("# natural mortality coefficient",names(par))] <- "Mbase"
  names(par)[grep("# effort deviation coefficients",names(par))] <- "effdevcoffs"
    ## Note: effdevcoffs is an unstacked ragged array rather than a matrix padded with NAs
  names(par)[grep("# average catchability coefficients",names(par))] <- "meanqs"
  names(par)[grep("# Objective function value",names(par))] <- "obj"
  names(par)[grep("# The number of parameters",names(par))] <- "npars"
  names(par)[grep("# Maximum magnitude gradient value",names(par))] <- "gradient"
  names(par)[grep("# extra par for Richards",names(par))] <- "Richards"
  ##names(par)[grep("",names(par))] <- ""

    ## derive some exta elements for the list
  par$nfisheries <- dim(par$ffl)[1]
  pos <- grep("# The von Bertalanffy parameters",names(par))
    par$Lmin <- par[[pos]][1,1]
    par$Lmax <- par[[pos]][2,1]
    par$K    <- par[[pos]][3,1]
  pos <- grep("# Variance parameters",names(par))
    par$growth_vars <- par[[pos]][,1]
  pos <- grep("# age-class related parameters",names(par))
    par$M_offsets <-  par[[pos]][2,]
    par$gr_offsets <- par[[pos]][3,]
 
  return(par)
}
