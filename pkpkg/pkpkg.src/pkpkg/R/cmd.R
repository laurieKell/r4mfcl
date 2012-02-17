"cmd" <-
function (..., cmd = "cat") 
##=======================================================
## applies Unix command, cmd, to other input variables
##=======================================================
{
    tfile <- tempfile()
    on.exit(unlink(tfile))
    cat(..., sep = "\n", file = paste("|", cmd, " >", tfile))
    type.convert(readLines(tfile), as.is = TRUE)
}
