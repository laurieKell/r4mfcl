##' 
##' read data from a vector of character strings
##' 
##' Sets up a connection to a vector of character
##' strings which is read by scan(). 
##' 
##' @param string A vector of character strings.
##' @param what As for scan. The type of what gives the type of data
##'   to be read. If what is a list, it is assumed that the lines of the
##'   data file are records each containing length(what) items.
##' @param ... Other arguments passed to scan().
##' 
##' @return A vector or list.
##'
##' @examples
##'   scanText("A few short words")
##'   as.numeric(scanText("1 2 3\n89 90"))
##'   scanText("A B C 4 5 6", what = list("", "", "", 0, 0, 0))
##' 
##' @seealso \link{scan()}
##' @author P. Kleiber
scanText <-
  function(string, what = character(0), ...){
    ##============================================================
    ## Like scan() but reading from a vector of character strings
    ##============================================================
    tc <- textConnection(string)
    result <- scan(tc, what = what, quiet = TRUE, ...)
    close(tc)
    return(result)}

